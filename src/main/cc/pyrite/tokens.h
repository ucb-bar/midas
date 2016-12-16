#ifndef __TOKENS_H
#define __TOKENS_H

#include <cstdlib>
#include "biguint.h"

class MidasToken {};
class AXI4MToken: public MidasToken {
  private:
    int addrWidth;
    int dataWidth;
    int idWidth;

  public:
    struct r_b {
      uint32_t ready;
    };
    r_b r;
    struct ar_b {
      struct bits_b {
        uint32_t qos;
        uint32_t prot;
        uint32_t cache;
        uint32_t lock;
        uint32_t burst;
        uint32_t size;
        uint32_t len;
        uint64_t addr;
        uint32_t id;
      };
      bits_b bits;
      uint32_t valid;
    };
    ar_b ar;
    struct b_b {
      uint32_t ready;
    };
    b_b b;
    struct w_b {
      struct bits_b {
        uint32_t last;
        uint32_t strb;
        biguint_t data;
      };
      bits_b bits;
      uint32_t valid;
    };
    w_b w;
    struct aw_b {
      struct bits_b {
        uint32_t qos;
        uint32_t prot;
        uint32_t cache;
        uint32_t lock;
        uint32_t burst;
        uint32_t size;
        uint32_t len;
        uint64_t addr;
        uint32_t id;
      };
      bits_b bits;
      uint32_t valid;
    };
    aw_b aw;
    

    AXI4MToken(int addrWidth, int dataWidth, int idWidth) : addrWidth{addrWidth}, dataWidth{dataWidth}, idWidth{idWidth} {};
    ~AXI4MToken() {};
    int get_addrWidth(void){ return addrWidth; };
    int get_dataWidth(void){ return dataWidth; };
    int get_idWidth(void){ return idWidth; };
};

class AXI4SToken: public MidasToken {
  private:
    int dataWidth;
    int idWidth;

  public:
    struct r_b {
      struct bits_b {
        uint32_t last;
        uint32_t resp;
        biguint_t data;
        uint32_t id;
      };
      bits_b bits;
      uint32_t valid;
    };
    r_b r;
    struct ar_b {
      uint32_t ready;
    };
    ar_b ar;
    struct b_b {
      struct bits_b {
        uint32_t resp;
        uint32_t id;
      };
      bits_b bits;
      uint32_t valid;
    };
    b_b b;
    struct w_b {
      uint32_t ready;
    };
    w_b w;
    struct aw_b {
      uint32_t ready;
    };
    aw_b aw;
    

    AXI4SToken(int dataWidth, int idWidth) : dataWidth{dataWidth}, idWidth{idWidth} {};
    ~AXI4SToken() {};
    int get_dataWidth(void){ return dataWidth; };
    int get_idWidth(void){ return idWidth; };
};

#endif