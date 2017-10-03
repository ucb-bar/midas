#include "simif.h"
#include <iostream>
#include <fstream>
#include <algorithm>

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
  assert(k == vars->size);
}

int simif_t::select_print() {
  bool done;
  do {
    done = true;
    for (size_t i = 0 ; i < PRINTS_NUM ; i++) {
      if (read(PRINTS_VALID_ADDRS[i])) {
        read_print_vars(i);
        print_state.deltas[i].push(read(PRINTS_DELTA_ADDRS[i]));
        done = false;
      }
    }
  } while(!done);

  uint64_t min = -1L;
  print_state.select = -1;
  for (int i = 0 ; i < PRINTS_NUM ; i++) {
    if (!print_state.deltas[i].empty()) {
      uint64_t delta = print_state.deltas[i].front();
      uint64_t cycle = print_state.cycles[i] + delta;
      if (min > cycle) {
        min = cycle;
        print_state.select = i;
      }
    }
  }
  if (print_state.select >= 0) {
    print_state.cycles[print_state.select] = min;
    print_state.deltas[print_state.select].pop();
  }
  return print_state.select;
}

void simif_t::read_print_vars(size_t id) {
  const size_t MAX_CHUNK = 32;
  data_t data[MAX_CHUNK];
  size_t chunk = PRINTS_BITS_CHUNKS[id];
  assert(chunk <= MAX_CHUNK);
  for (size_t k = 0 ; k < chunk ; k ++) {
    data[k] = read(PRINTS_BITS_ADDRS[id] + k);
  }
  mpz_t bits;
  mpz_init(bits);
  mpz_import(bits, chunk, -1 , sizeof(data_t), 0, 0, data);

  size_t off = 0, size = print_state.widths[id].size();
  print_vars_t* vars = new print_vars_t(size);
  for (size_t k = 0 ; k < size ; k++) {
    mpz_t* var = vars->data[k];
    mpz_t* mask = print_state.masks[id]->data[k];
    // *var = bits >> off
    mpz_fdiv_q_2exp(*var, bits, off);
    // *var = *var & *mask
    mpz_and(*var, *var, *mask);
    off += print_state.widths[id][k];
  }
  print_state.values[id].push(vars);
  write(PRINTS_READY_ADDRS[id], 1);
}

void simif_t::clear_print_vars() {
  int sel = print_state.select;
  assert(sel >= 0 && sel < PRINTS_NUM);
  free(print_state.values[sel].front());
  print_state.values[sel].pop();
}

bool simif_t::detect_prints() {
  int sel = select_print();

  if (sel >= 0) {
    // std::cerr << "[C: " << print_state.cycle[sel] << "]";
    print_format(print_state.formats[sel].c_str(),
                 print_state.values[sel].front());
    clear_print_vars();
  } 

  return sel >= 0;
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
  std::fill(print_state.cycles.begin(), print_state.cycles.end(), 0L);
  std::fill(print_state.deltas.begin(), print_state.deltas.end(), std::queue<data_t>());
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
        print_state.masks[i] = new print_vars_t(size);
        for (size_t k = 0 ; k < size ; k++) {
           mpz_t* mask = print_state.masks[i]->data[k];
           size_t width = print_state.widths[i][k];
           // *mask = (1 << width) - 1
           mpz_set_ui(*mask, 1);
           mpz_mul_2exp(*mask, *mask, width);
           mpz_sub_ui(*mask, *mask, 1);
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
