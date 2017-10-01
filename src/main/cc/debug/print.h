#ifndef __PRINT_H
#define __PRINT_H

#include <gmp.h>
#include <array>

#define PRINT_VARS_MAX_SIZE 128

struct print_vars_t {
  mpz_t* data[PRINT_VARS_MAX_SIZE];
  size_t size;
  print_vars_t(size_t size): size(size) {
    assert(size < PRINT_VARS_MAX_SIZE);
    for (size_t i = 0 ; i < size ; i++) {
      data[i] = (mpz_t*)malloc(sizeof(mpz_t));
      mpz_init(*data[i]);
    }
  }
  ~print_vars_t() {
    for (size_t i = 0 ; i < size ; i++) {
      mpz_clear(*data[i]);
      free(data[i]);
    }
  }
};

struct print_state_t {
  bool enable;
  int select;
  std::array<std::string,               PRINTS_NUM> formats;
  std::array<std::vector<std::string>,  PRINTS_NUM> names;
  std::array<std::vector<size_t>,       PRINTS_NUM> widths;
  std::array<uint64_t,                  PRINTS_NUM> cycles;
  std::array<std::queue<data_t>,        PRINTS_NUM> deltas;
  std::array<std::queue<print_vars_t*>, PRINTS_NUM> values;
  print_vars_t* masks[PRINTS_NUM];
  ~print_state_t() {
    for (size_t i = 0 ; i < PRINTS_NUM ; i++) {
      delete masks[i];
    }
  }
};

#endif // __PRINT_H
