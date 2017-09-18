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
void simif_t::print_format(size_t i, mpz_t& bits) {
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
}

bool simif_t::detect_prints() {
  bool detected = false;
  for (size_t i = 0 ; i < PRINTS_NUM ; i++) {
    if (print_stamps[i] == 0 && read(PRINTS_VALID_ADDRS[i])) {
      print_stamps[i] = read(PRINTS_STAMP_ADDRS[i]);
    }
  }

  size_t sel;
  uint64_t min = -1L;
  for (size_t i = 0 ; i < PRINTS_NUM ; i++) {
    if (print_stamps[i] > 0) {
      uint64_t cycle = print_cycles[i] + print_stamps[i];
      if (min > cycle) {
        min = cycle;
        sel = i;
      }
    }
  }

  if (min < (uint64_t)(-1L)) {
    size_t chunk = PRINTS_BITS_CHUNKS[sel];
    data_t* data = new data_t[chunk];
    for (size_t k = 0 ; k < chunk ; k ++) {
      data[k] = read(PRINTS_BITS_ADDRS[sel] + k);
    }
    mpz_t bits;
    mpz_init(bits);
    mpz_import(bits, chunk, -1 , sizeof(data_t), 0, 0, data);
    // std::cerr << "[C: " << min << " sel: " << sel << " ] ";
    print_format(sel, bits);
    delete[] data;
    mpz_clear(bits);
    write(PRINTS_READY_ADDRS[sel], 1);
    print_cycles[sel] = min;
    print_stamps[sel] = 0;
    detected = true;
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
  std::fill(print_cycles.begin(), print_cycles.end(), 0L);
  std::fill(print_stamps.begin(), print_stamps.end(), 0);
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
