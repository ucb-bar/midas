#ifndef __PRINT_H
#define __PRINT_H

#include <gmp.h>
#include <array>

struct print_vars_t {
  mpz_t** data;
  size_t size;
  print_vars_t(size_t size): data(new mpz_t*[size]), size(size) {
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
    delete[] data;
  }
};

struct print_state_t {
  bool enable;
  int select;
  std::array<std::string,              PRINTS_NUM> formats;
  std::array<std::vector<std::string>, PRINTS_NUM> names;
  std::array<std::vector<size_t>,      PRINTS_NUM> widths;
  std::array<uint64_t,                 PRINTS_NUM> cycles;
  std::array<data_t,                   PRINTS_NUM> stamps;
  print_vars_t* vars[PRINTS_NUM];
  print_vars_t* masks[PRINTS_NUM];
  ~print_state_t() {
    for (size_t i = 0 ; i < PRINTS_NUM ; i++) {
      delete vars[i];
      delete masks[i];
    }
  }
};

#endif // __PRINT_H
