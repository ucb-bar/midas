#include "simif.h"
#include <iostream>
#include <fstream>
#include <algorithm>
#include <memory>

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
void print_format(const char* fmt, print_vars_t* vars) {
  size_t k = 0;
  while(*fmt) {
    if (*fmt == '%' && fmt[1] != '%') {
      mpz_t* value = vars->data[k];
      char* v = NULL;
      if (fmt[1] == 's') {
        // Is order right?
        size_t size;
        v = (char*)mpz_export(NULL, &size, 1, sizeof(char), 0, 0, *value);
        for (size_t j = 0 ; j < size ; j++) fputc(v[j], stderr);
        fmt++;
      } else {
        switch(*(++fmt)) {
          // TODO: exhaustive?
          case 'h':
          case 'x': v = mpz_get_str(NULL, 16, *value); break;
          case 'd': v = mpz_get_str(NULL, 10, *value); break;
          case 'b': v = mpz_get_str(NULL, 2, *value); break;
          default: break;
        }
        if (v) fprintf(stderr, "%s", v);
      }
      free(v);
      fmt++;
      k++;
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
  assert(k == vars->data.size());
}

static const size_t BUF_SIZE = 512 / 8;
static const size_t BUF_DEPTH = 8 * 1024;
static std::unique_ptr<char> buf(new char[BUF_DEPTH * BUF_SIZE]()); 

bool simif_t::read_prints() {
  bool done;
  size_t count, total_count = 0;
  // uint64_t start_time = timestamp();
  do {
    count = read(PRINTS_COUNT_ADDR);
#ifndef HAS_DMA_CHANNEL
    size_t offset = 0;
    for (size_t i = 0 ; i < count ; i++) {
      for (size_t k = 0 ; k < PRINTS_CHUNKS ; k++) {
        data_t bits = read(PRINTS_DATA_ADDRS[k]);
        std::copy((char*)&bits, (char*)&bits + sizeof(data_t), buf.get() + offset);
        offset += sizeof(data_t);
      }
    }
#else
    if (size_t size = count * DMA_WIDTH) {
      assert(pread(0, buf.get(), size) == size);
    }
#endif // HAS_DMA_CHANNEL
    for (size_t i = 0 ; i < count ; i++) {
      mpz_t* print = (mpz_t*)malloc(sizeof(mpz_t));
      mpz_init(*print);
#ifndef HAS_DMA_CHANNEL
      mpz_import(*print, PRINTS_CHUNKS, -1, sizeof(data_t), 0, 0, ((data_t*)buf.get()) + (i * PRINTS_CHUNKS));
#else
      mpz_import(*print, DMA_WIDTH, -1, sizeof(char), 0, 0, buf.get() + (i * DMA_WIDTH));
fprintf(stderr, "%s\n", mpz_get_str(NULL, 16, *print));
#endif
      print_state.prints.push(print);
    }
    total_count += count;
  } while(count > 0);

  return total_count > 0;
}

bool simif_t::parse_print_vars() {
  if (print_state.prints.empty()) return false;

  mpz_t* print = print_state.prints.front();
  // delta = *print & delta_mask
  mpz_t delta;
  mpz_init(delta);
  mpz_and(delta, *print, print_state.delta_mask);
  print_state.cycles += mpz_get_ui(delta);
  mpz_clear(delta);
  // *print = *print >> 24
  mpz_fdiv_q_2exp(*print, *print, 24);

  for (size_t id = 0 ; id < PRINTS_NUM ; id++) {
    if (mpz_get_ui(*print) & 0x1) {
      // print_var_t* vars = parse_print_vars(*print, k);
      size_t off = 1, size = print_state.widths[id].size();
      print_vars_t* vars = new print_vars_t;
      for (size_t k = 0 ; k < size ; k++) {
        mpz_t* var = (mpz_t*)malloc(sizeof(mpz_t));
        mpz_t* mask = print_state.masks[id]->data[k];
        mpz_init(*var);
        // *var = *print >> off
        mpz_fdiv_q_2exp(*var, *print, off);
        // *var = *var & *mask
        mpz_and(*var, *var, *mask);
        vars->data.push_back(var);
        off += print_state.widths[id][k];
      }
      print_state.values[id].push(vars);
    }
    // *print = *print >> PRINTS_WIDTHS[id]
    mpz_fdiv_q_2exp(*print, *print, PRINTS_WIDTHS[id]);
  }
  mpz_clear(*print);
  print_state.prints.pop();

  return true;
}

void simif_t::show_prints() {
  while (parse_print_vars()) { 
    for (size_t id = 0 ; id < PRINTS_NUM ; id++) {
      while (!print_state.values[id].empty()) {
        print_vars_t* vars = print_state.values[id].front();
        print_format(print_state.formats[id].c_str(), vars);
        print_state.values[id].pop();
      }
    }
  }
}

void simif_t::init_prints(int argc, char** argv) {
  std::vector<std::string> args(argv + 1, argv + argc);
  print_state.enable = false;
  for (auto &arg: args) {
    if (arg.find("+prints") == 0) {
      print_state.enable = true;
    }
  }
  write(PRINTS_ENABLE, print_state.enable);
  std::string filename = std::string(TARGET_NAME) + ".prints";
  std::ifstream file(filename.c_str());
  if (!file) {
    fprintf(stderr, "Cannot open %s\n", filename.c_str());
    if (print_state.enable) exit(EXIT_FAILURE);
    else return;
  }

  std::fill(print_state.names.begin(), print_state.names.end(), std::vector<std::string>());
  std::fill(print_state.widths.begin(), print_state.widths.end(), std::vector<size_t>());
  std::fill(print_state.values.begin(), print_state.values.end(), std::queue<print_vars_t*>());

  std::string line;
  size_t i = 0;
  enum { PRINT_FMT, PRINT_ARGS } line_state = PRINT_FMT;
  while (std::getline(file, line)) {
    switch (line_state) {
      case PRINT_FMT:
        print_state.formats[i] = line;
        line_state = PRINT_ARGS;
        break;
      case PRINT_ARGS:
        std::istringstream iss(line);
        std::string token;
        enum { PRINT_NAME, PRINT_VAR } token_state = PRINT_NAME;
        while (std::getline(iss, token, ' ')) {
          switch (token_state) {
            case PRINT_NAME:
              print_state.names[i].push_back(token);
              token_state = PRINT_VAR;
              break;
            case PRINT_VAR:
              print_state.widths[i].push_back((size_t)atol(token.c_str()));
              token_state = PRINT_NAME;
              break;
          }
        }
        assert(token_state == PRINT_NAME);
        size_t size = print_state.widths[i].size();
        assert(size == print_state.names[i].size());
        for (size_t k = 0 ; k < size ; k++) {
           mpz_t* mask = (mpz_t*)malloc(sizeof(mpz_t));
           size_t width = print_state.widths[i][k];
           // *mask = (1 << width) - 1
           mpz_init(*mask);
           mpz_set_ui(*mask, 1);
           mpz_mul_2exp(*mask, *mask, width);
           mpz_sub_ui(*mask, *mask, 1);
           print_state.masks[i]->data.push_back(mask);
        }
        i++;
        line_state = PRINT_FMT;
        break;
    }
  }
  assert(line_state == PRINT_FMT);
  assert(i == PRINTS_NUM);
}
#endif // ENABLE_PRINT
