#include "simif.h"
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
  while(!done());

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
}

void simif_t::target_reset(int pulse_start, int pulse_length) {
  poke(reset, 0);
  take_steps(pulse_start, true);
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
  mpz_clear(value);
  return expect(pass, NULL);
}

void simif_t::step(int n, bool blocking) {
  if (n == 0) return;
  assert(n > 0);
#ifdef ENABLE_SNAPSHOT
  reservoir_sampling(n);
#endif
  // take steps
  if (log) fprintf(stderr, "* STEP %d -> %llu *\n", n, (t + n));
  take_steps(n, blocking);
  t += n;
}

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
  mpz_t data;
  mpz_init(data);
  while (std::getline(file, line)) {
    assert(line.length() % chunk == 0);
    for (int j = line.length() - chunk ; j >= 0 ; j -= chunk) {
      mpz_set_str(data, line.substr(j, chunk).c_str(), 16);
      write_mem(addr, data);
      addr += chunk / 2;
    }
  }
  mpz_clear(data);
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
