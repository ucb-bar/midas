#ifndef __SIMIF_H
#define __SIMIF_H

#include <cassert>
#include <cstring>
#include <sstream>
#include <queue>
#include <sys/time.h>
#include "biguint.h"
#include "sample.h"

static inline uint64_t timestamp() {
  struct timeval tv;
  gettimeofday(&tv,NULL);
  return 1000000L * tv.tv_sec + tv.tv_usec;
}

typedef std::map< std::string, size_t > idmap_t;
typedef std::map< std::string, size_t >::const_iterator idmap_it_t;

class simif_t
{
  public:
    simif_t(std::vector<std::string> args, bool _log = false);
    virtual ~simif_t();

  private:
    virtual void load_mem(std::string filename);

    // simulation information
    const std::string prefix;
    const bool log; 
    bool ok;
    uint64_t t;
    uint64_t fail_t;
    size_t trace_count;
    size_t trace_len;
    time_t seed; 

    // maps 
    uint32_t poke_map[POKE_SIZE];
    uint32_t peek_map[PEEK_SIZE];

    // sample information
    sample_t** samples;
    size_t sample_num;
    size_t last_sample_id;
    sample_t* last_sample;

    // profile information    
    bool profile;
    size_t sample_count;
    uint64_t sample_time;
    uint64_t sim_start_time;
    std::vector<std::string> hargs;
    std::vector<std::string> targs;

  protected:
    // channel communication
    virtual void write(size_t addr, uint32_t data) = 0;
    virtual uint32_t read(size_t addr) = 0;

    // Simulation APIs
    inline int exitcode() { return ok ? EXIT_SUCCESS : EXIT_FAILURE; }

    inline void poke(size_t id, uint32_t value) { 
      if (log) fprintf(stdout, "* POKE %s.%s <- 0x%x *\n", TARGET_NAME, INPUT_NAMES[id], value);
      poke_map[id] = value;
    }

    inline uint32_t peek(size_t id) {
      uint32_t value = peek_map[id]; 
      if (log) fprintf(stdout, "* PEEK %s.%s -> 0x%x *\n", TARGET_NAME, OUTPUT_NAMES[id], value);
      return value;
    }

    inline void poke(size_t id, biguint_t& value) {
      if (log) fprintf(stdout, "* POKE %s.%s <- 0x%s *\n", TARGET_NAME, INPUT_NAMES[id], value.str().c_str());
      for (size_t off = 0 ; off < INPUT_CHUNKS[id] ; off++) {
        poke_map[id+off] = value[off];
      }
    }

    inline void peek(size_t id, biguint_t& value) {
      value = biguint_t(peek_map+id, OUTPUT_CHUNKS[id]);
      if (log) fprintf(stdout, "* PEEK %s.%s -> 0x%s *\n", TARGET_NAME, OUTPUT_NAMES[id], value.str().c_str());
    }

    inline bool expect(size_t id, uint32_t expected) {
      uint32_t value = peek(id);
      bool pass = value == expected;
      std::ostringstream oss;
      if (log) oss << "EXPECT " << TARGET_NAME << "." << OUTPUT_NAMES[id] << " " << value << " == " << expected;
      return expect(pass, oss.str().c_str());
    }

    inline bool expect(size_t id, biguint_t& expected) {
      biguint_t value;
      peek(id, value);
      bool pass = value == expected;
      std::ostringstream oss;
      if (log) oss << "EXPECT " << TARGET_NAME << "." << OUTPUT_NAMES[id] << " " << value << " == " << expected;
      return expect(pass, oss.str().c_str());
    }

    bool expect(bool pass, const char *s);
    void step(size_t n);
    inline biguint_t read_mem(size_t addr) {
      write(MEM_AR_ADDR, addr);
      uint32_t d[MEM_DATA_CHUNK];
      for (size_t off = 0 ; off < MEM_DATA_CHUNK; off++) {
        d[off] = read(MEM_R_ADDR+off);
      }
      return biguint_t(d, MEM_DATA_CHUNK);
    }
    inline void write_mem(size_t addr, biguint_t& data) {
      write(MEM_AW_ADDR, addr);
      for (size_t off = 0 ; off < MEM_DATA_CHUNK ; off++) {
        write(MEM_W_ADDR+off, data[off]);
      }
    }
    sample_t* read_snapshot();
    sample_t* read_traces(sample_t* s);
    
    void init();
    void finish();
    inline uint64_t cycles() { return t; }
    inline void set_tracelen(size_t len) {
      assert(len > 2);
      trace_len = len;
      write(TRACELEN_ADDR, len);
    }
    inline size_t get_tracelen() { return trace_len; }
    uint64_t rand_next(uint64_t limit) { return rand() % limit; } 
};

#endif // __SIMIF_H