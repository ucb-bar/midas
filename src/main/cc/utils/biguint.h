#ifndef __BIGUINT_H
#define __BIGUINT_H

#include <stdint.h>
#include <ostream>
#include <iomanip>

#define UINT_WIDTH 32
#define HEX_WIDTH 8

class biguint_t {
public:
  // constructor
  biguint_t() { data = NULL; size = 0; }
  biguint_t(const uint8_t value) { init(value); }
  biguint_t(const uint16_t value) { init(value); }
  biguint_t(const uint32_t value) { init(value); }
  biguint_t(const uint64_t value) { init((uint32_t*)&value, 2); }
  biguint_t(const int8_t value) { init(value); }
  biguint_t(const int16_t value) { init(value); }
  biguint_t(const int32_t value) { init(value); }
  biguint_t(const int64_t value) { init((uint32_t*)&value, 2); }
  biguint_t(const uint32_t *v, size_t s) { init(v, s); }
  biguint_t(const char* value, size_t base = 16);
  biguint_t(const biguint_t& that);
  ~biguint_t() { if (size > 0) delete[] data; }
  uint32_t uint() const { return data[0]; }
  const uint32_t& operator[](size_t idx) const { return data[idx]; }
  std::string str();
  uint32_t* const get_data() const { return data; }
  const size_t get_size() const { return size; }
  biguint_t operator<<(const size_t shamt);
  biguint_t operator>>(const size_t shamt);
  biguint_t operator=(const biguint_t &that);
  biguint_t operator|(const biguint_t &that);
  biguint_t operator&(const biguint_t &that);
  void operator&=(const biguint_t &that);
  void operator|=(const biguint_t &that);
  bool operator==(const biguint_t &that);
  friend std::ostream& operator<<(std::ostream &os, const biguint_t& value);
  friend std::istream& operator>>(std::istream &is, biguint_t& value);

  // Bit extraction
  uint32_t extract_uint32(size_t lsb, size_t msb);
  uint64_t extract_uint64(size_t lsb, size_t msb);
  biguint_t extract(size_t lsb, size_t msb);

  // Subfield assignment
  void set_bits(size_t lsb, size_t msb, uint32_t bits);
  void set_bits(size_t lsb, size_t msb, uint64_t bits);
  void set_bits(size_t lsb, size_t msb, biguint_t& bits);

private:
  void init(const uint32_t value);
  void init(const uint32_t* value, size_t size);
  void init_hex(const char* hex);
  void init_bin(const char* bin);
  void copy_biguint(const biguint_t &that);
  void bit_or(const biguint_t &a, const biguint_t &b);
  void bit_and(const biguint_t &a, const biguint_t &b);
  // Sets the bits within one storage word of the biguint, given by idx;.
  // Argument bits will be masked and shifted into alignment
  void set_word_bits(size_t idx, size_t lsb, size_t msb, uint32_t bits);
  size_t size;
  uint32_t *data;
};

#define parse_nibble(c) ((c) >= 'a' ? (c)-'a'+10 : (c)-'0')

inline uint32_t hex_to_dec(const char *hex) {
  uint32_t value = 0;
  while(*hex) {
    value = (value << 4) | parse_nibble(*hex);
    hex++;
  }
  return value;
}

inline uint32_t bin_to_dec(const char *bin) {
  uint32_t value = 0;
  while(*bin) {
    value = (value << 1) | (*bin - '0');
    bin++;
  }
  return value;
}

inline uint32_t make_mask(size_t lsb, size_t msb) {
  return (-1U >> (UINT_WIDTH - msb - 1)) & (-1U << lsb);
}

#endif // __BIGUINT_H
