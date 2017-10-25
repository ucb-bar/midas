#ifndef __PRINT_H
#define __PRINT_H

#include <gmp.h>
#include <array>

struct print_vars_t {
  std::vector<mpz_t*> data;
  ~print_vars_t() {
    for (auto& e: data) {
      mpz_clear(*e);
      free(e);
    }
  }
};

struct print_state_t {
  bool enable;
  uint64_t cycles;
  std::queue<mpz_t*> prints;
  std::array<std::string,               PRINTS_NUM> formats;
  std::array<std::vector<std::string>,  PRINTS_NUM> names;
  std::array<std::vector<size_t>,       PRINTS_NUM> widths;
  std::array<std::queue<print_vars_t*>, PRINTS_NUM> values;

  mpz_t delta_mask;
  print_vars_t* masks[PRINTS_NUM];

  print_state_t() {
    cycles = 0;
    // delta_mask = (1 << 24) - 1
    mpz_init(delta_mask);
    mpz_set_ui(delta_mask, 1);
    mpz_mul_2exp(delta_mask, delta_mask, 24);
    mpz_sub_ui(delta_mask, delta_mask, 1);

    for (size_t i = 0 ; i < PRINTS_NUM ; i++) {
      masks[i] = new print_vars_t;
    }
  }
  ~print_state_t() {
    mpz_clear(delta_mask);
    for (size_t i = 0 ; i < PRINTS_NUM ; i++) {
      delete masks[i];
    }
  }
};

#endif // __PRINT_H
