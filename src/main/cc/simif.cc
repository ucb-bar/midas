#include "simif.h"
#include <iostream>
#include <fstream>
#include <algorithm>

midas_time_t timestamp(){
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return 1000000L * tv.tv_sec + tv.tv_usec;
}

double diff_secs(midas_time_t end, midas_time_t start) {
  return ((double)(end - start)) / TIME_DIV_CONST;
}

simif_t::simif_t() {
  pass = true;
  t = 0;
  fail_t = 0;
  seed = time(NULL); // FIXME: better initail seed?
}

void simif_t::init(int argc, char** argv, bool log) {
  // Simulation reset
  write(MASTER(SIM_RESET), 1);
  while(!read(MASTER(DONE)));

  this->log = log;
  std::vector<std::string> args(argv + 1, argv + argc);
  std::string loadmem;
  bool fastloadmem = false;
  for (auto &arg: args) {
    if (arg.find("+fastloadmem") == 0) {
      fastloadmem = true;
    }
    if (arg.find("+loadmem=") == 0) {
      loadmem = arg.c_str() + 9;
    }
    if (arg.find("+seed=") == 0) {
      seed = strtoll(arg.c_str() + 6, NULL, 10);
    }
  }
  gen.seed(seed);
  fprintf(stderr, "random min: 0x%llx, random max: 0x%llx\n", gen.min(), gen.max());
#ifdef LOADMEM
  if (!fastloadmem && !loadmem.empty()) {
    load_mem(loadmem.c_str());
  }
#endif

#ifdef ENABLE_SNAPSHOT
  init_sampling(argc, argv);
#endif
#ifdef ENABLE_PRINT
  init_prints(argc, argv);
#endif
}

void simif_t::target_reset(int pulse_start, int pulse_length) {
  poke(reset, 0);
  take_steps(pulse_start, true);
#ifdef ENABLE_SNAPSHOT
  read_snapshot(true);
#endif
  poke(reset, 1);
  take_steps(pulse_length, true);
  poke(reset, 0);
#ifdef ENABLE_SNAPSHOT
  // flush I/O traces by target resets
  trace_count = std::min((size_t)(pulse_start + pulse_length), tracelen);
  read_traces(NULL);
  trace_count = 0;
#endif
}

int simif_t::finish() {
#ifdef ENABLE_SNAPSHOT
  finish_sampling();
#endif

  fprintf(stderr, "Runs %llu cycles\n", cycles());
  fprintf(stderr, "[%s] %s Test", pass ? "PASS" : "FAIL", TARGET_NAME);
  if (!pass) { fprintf(stdout, " at cycle %llu", fail_t); }
  fprintf(stderr, "\nSEED: %ld\n", seed);

  return pass ? EXIT_SUCCESS : EXIT_FAILURE;
}

static const size_t data_t_chunks = sizeof(data_t) / sizeof(uint32_t);

void simif_t::poke(size_t id, mpz_t& value) {
  if (log) {
    char* v_str = mpz_get_str(NULL, 16, value);
    fprintf(stderr, "* POKE %s.%s <- 0x%s *\n", TARGET_NAME, INPUT_NAMES[id], v_str);
    free(v_str);
  }
  size_t size;
  data_t* data = (data_t*)mpz_export(NULL, &size, -1, sizeof(data_t), 0, 0, value);
  for (size_t i = 0 ; i < INPUT_CHUNKS[id] ; i++) {
    write(INPUT_ADDRS[id]+i, i < size ? data[i] : 0);
  }
}

void simif_t::peek(size_t id, mpz_t& value) {
  const size_t size = (const size_t)OUTPUT_CHUNKS[id];
  data_t data[size];
  for (size_t i = 0 ; i < size ; i++) {
    data[i] = read((size_t)OUTPUT_ADDRS[id]+i);
  }
  mpz_import(value, size, -1, sizeof(data_t), 0, 0, data);
  if (log) {
    char* v_str = mpz_get_str(NULL, 16, value);
    fprintf(stderr, "* PEEK %s.%s -> 0x%s *\n", TARGET_NAME, (const char*)OUTPUT_NAMES[id], v_str);
    free(v_str);
  }
}

bool simif_t::expect(size_t id, mpz_t& expected) {
  mpz_t value;
  mpz_init(value);
  peek(id, value);
  bool pass = mpz_cmp(value, expected) == 0;
  if (log) {
    char* v_str = mpz_get_str(NULL, 16, value);
    char* e_str = mpz_get_str(NULL, 16, expected);
    fprintf(stderr, "* EXPECT %s.%s -> 0x%s ?= 0x%s : %s\n",
      TARGET_NAME, (const char*)OUTPUT_NAMES[id], v_str, e_str, pass ? "PASS" : "FAIL");
    free(v_str);
    free(e_str);
  }
  return expect(pass, NULL);
}

void simif_t::step(int n, bool blocking) {
  if (n == 0) return;
  assert(n > 0);
#ifdef ENABLE_SNAPSHOT
  if (sample_cycle == 0) {
    reservoir_sampling(n);
  } else if ((t + n) > sample_cycle && (t + n) - sample_cycle <= tracelen) {
    fprintf(stderr, "Snapshot at %llu\n", t);
    // flush trace buffer
    trace_count = std::min((size_t)(t + n), tracelen);
    read_traces(NULL);
    trace_count = 0;
    // take a snaphsot
    last_sample = read_snapshot();
    last_sample_id = 0;
  }
  delta = n;
#endif
  // take steps
  if (log) fprintf(stderr, "* STEP %d -> %llu *\n", n, (t + n));
  take_steps(n, blocking);
  t += n;
}

#ifdef ENABLE_DEBUG
void simif_t::detect_assert() {
  if (read(ASSERTWIDGET(fire))) {
    // Read assertion information
    std::vector<std::string> msgs;
    std::ifstream file(std::string(TARGET_NAME) + ".asserts");
    std::string line;
    std::ostringstream oss;
    while (std::getline(file, line)) {
      if (line == "0") {
        msgs.push_back(oss.str());
        oss.str(std::string());
      } else {
        oss << line << std::endl;
      }
    }
    uint64_t assert_cycle = read(ASSERTWIDGET(cycle_low));
    assert_cycle |= ((uint64_t)read(ASSERTWIDGET(cycle_high))) << 32;
    std::cerr << msgs[read(ASSERTWIDGET(id))];
    std::cerr << " at cycle: " << assert_cycle << std::endl;
#ifdef ENABLE_SNAPSHOT
    trace_count = assert_cycle - (t - delta);
#endif
    finish();
    exit(EXIT_FAILURE);
  }
}
#endif // ENABLE_DEBUG

#ifdef ENABLE_PRINT
bool simif_t::detect_prints() {
  bool detected = false;
  for (size_t i = 0 ; i < PRINTS_NUM ; i++) {
    if (read(PRINTS_VALID_ADDRS[i])) {
      size_t chunk = PRINTS_BITS_CHUNKS[i];
      data_t* data = new data_t[chunk];
      for (size_t k = 0 ; k < chunk ; k ++) {
        data[k] = read(PRINTS_BITS_ADDRS[i] + k);
      }
      mpz_t bits;
      mpz_init(bits);
      mpz_import(bits, chunk, -1 , sizeof(data_t), 0, 0, data);
      delete[] data;
      const char* fmt = print_formats[i].c_str();
      size_t k = 0, off = 0;
      while(*fmt) {
        if (*fmt == '%' && fmt[1] != '%') {
          size_t width = print_widths[i][k];
          mpz_t value, mask;
          mpz_init(value);
          mpz_init(mask);
          // value = bits >> off
          mpz_fdiv_q_2exp(value, bits, off);
          // mask = (1 << width) - 1
          mpz_set_ui(mask, 1);
          mpz_mul_2exp(mask, mask, width);
          mpz_sub_ui(mask, mask, 1);
          // value = value & mask
          mpz_and(value, value, mask);
          char* v = NULL;
          if (fmt[1] == 's') {
            // Is order right?
            size_t size;
            v = (char*)mpz_export(NULL, &size, 1, sizeof(char), 0, 0, value);
            for (size_t j = 0 ; j < size ; j++) fputc(v[j], stderr);
            fmt++;
          } else {
            switch(*(++fmt)) {
              // TODO: exhaustive?
              case 'h':
              case 'x': v = mpz_get_str(NULL, 16, value); break;
              case 'd': v = mpz_get_str(NULL, 10, value); break;
              case 'b': v = mpz_get_str(NULL, 2, value); break;
              default: break;
            }
            if (v) fprintf(stderr, "%s", v);
          }
          free(v);
          mpz_clear(value);
          mpz_clear(mask);
          fmt++;
          k++;
          off += width;
        } else if (*fmt == '%') {
          fputc(*(++fmt), stderr);
          fmt++;
        } else if (*fmt == '\\' && fmt[1] == 'n') {
          fputc('\n', stderr);
          fmt += 2;
        } else {
          fputc(*fmt, stderr);
          fmt++;
        }
      }
      mpz_clear(bits);
      write(PRINTS_READY_ADDRS[i], 1);
      detected = true;
    }
  }
  return detected;
}

void simif_t::init_prints(int argc, char** argv) {
  std::vector<std::string> args(argv + 1, argv + argc);
  enable_prints = false;
  for (auto &arg: args) {
    if (arg.find("+prints") == 0) {
      enable_prints = true;
    }
  }
  write(PRINTS_ENABLE, enable_prints);
  std::string filename = std::string(TARGET_NAME) + ".prints";
  std::ifstream file(filename.c_str());
  if (!file) {
    fprintf(stderr, "Cannot open %s\n", filename.c_str());
    if (enable_prints) exit(EXIT_FAILURE);
    else return;
  }

  std::fill(print_names.begin(), print_names.end(), std::vector<std::string>());
  std::fill(print_widths.begin(), print_widths.end(), std::vector<size_t>());
  std::string line;
  size_t i = 0;
  bool is_format = true;
  while (std::getline(file, line)) {
    if (is_format) {
      print_formats[i] = line;
      is_format = false;
    } else {
      std::istringstream iss(line);
      std::string token;
      bool is_name = true;
      while (std::getline(iss, token, ' ')) {
        if (is_name) {
          print_names[i].push_back(token);
          is_name = false;
        } else {
          print_widths[i].push_back((size_t)atol(token.c_str()));
          is_name = true;
        }
      }
      assert(is_name);
      is_format = true;
      i++;
    }
  }
  assert(is_format);
  assert(i == PRINTS_NUM);
}
#endif // ENABLE_PRINT

#ifdef LOADMEM
void simif_t::load_mem(std::string filename) {
  fprintf(stdout, "[loadmem] start loading\n");
  std::ifstream file(filename.c_str());
  if (!file) {
    fprintf(stderr, "Cannot open %s\n", filename.c_str());
    exit(EXIT_FAILURE);
  }
  const size_t chunk = MEM_DATA_BITS / 4;
  size_t addr = 0;
  std::string line;
  while (std::getline(file, line)) {
    assert(line.length() % chunk == 0);
    for (int j = line.length() - chunk ; j >= 0 ; j -= chunk) {
      mpz_t data;
      mpz_init(data);
      mpz_set_str(data, line.substr(j, chunk).c_str(), 16);
      write_mem(addr, data);
      addr += chunk / 2;
    }
  }
  file.close();
  fprintf(stdout, "[loadmem] done\n");
}

void simif_t::read_mem(size_t addr, mpz_t& value) {
  write(LOADMEM_R_ADDRESS, addr);
  const size_t size = MEM_DATA_CHUNK;
  data_t data[size];
  for (size_t i = 0 ; i < size ; i++) {
    data[i] = read(LOADMEM_R_DATA);
  }
  mpz_import(value, size, -1, sizeof(data_t), 0, 0, data);
}

void simif_t::write_mem(size_t addr, mpz_t& value) {
  write(LOADMEM_W_ADDRESS, addr);
  size_t size;
  data_t* data = (data_t*)mpz_export(NULL, &size, -1, sizeof(data_t), 0, 0, value);
  for (size_t i = 0 ; i < MEM_DATA_CHUNK ; i++) {
    write(LOADMEM_W_DATA, i < size ? data[i] : 0);
  }
}
#endif // LOADMEM
