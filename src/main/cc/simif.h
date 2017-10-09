#ifndef __SIMIF_H
#define __SIMIF_H

#include <cassert>
#include <cstring>
#include <sstream>
#include <map>
#include <queue>
#include <random>
#ifdef ENABLE_SNAPSHOT
#include "sample/sample.h"
#endif
#include <gmp.h>
#include <sys/time.h>
#define TIME_DIV_CONST 1000000.0;
typedef uint64_t midas_time_t;

midas_time_t timestamp();

double diff_secs(midas_time_t end, midas_time_t start);

typedef std::map< std::string, size_t > idmap_t;
typedef std::map< std::string, size_t >::const_iterator idmap_it_t;

class simif_t
{
  public:
    simif_t();
    virtual ~simif_t() { }
  private:
    // simulation information
    bool log;
    bool pass;
    uint64_t t;
    uint64_t fail_t;
    // random numbers
    uint64_t seed;
    std::mt19937_64 gen;

    inline void take_steps(size_t n, bool blocking) {
      write(MASTER(STEP), n);
      if (blocking) while(!done());
    }
#ifdef LOADMEM
    virtual void load_mem(std::string filename);
#endif

  public:
    // Simulation APIs
    virtual void init(int argc, char** argv, bool log = false);
    virtual int finish();
    virtual void step(int n, bool blocking = true);
    inline bool done() { return read(MASTER(DONE)); }

    // Widget communication
    virtual void write(size_t addr, data_t data) = 0;
    virtual data_t read(size_t addr) = 0;

    inline void poke(size_t id, data_t value) {
      if (log) fprintf(stderr, "* POKE %s.%s <- 0x%x *\n",
        TARGET_NAME, INPUT_NAMES[id], value);
      write(INPUT_ADDRS[id], value);
    }

    inline data_t peek(size_t id) {
      data_t value = read(((unsigned int*)OUTPUT_ADDRS)[id]);
      if (log) fprintf(stderr, "* PEEK %s.%s -> 0x%x *\n",
        TARGET_NAME, (const char*)OUTPUT_NAMES[id], value);
      return value;
    }

    inline bool expect(size_t id, data_t expected) {
      data_t value = peek(id);
      bool pass = value == expected;
      if (log) fprintf(stderr, "* EXPECT %s.%s -> 0x%x ?= 0x%x : %s\n",
        TARGET_NAME, (const char*)OUTPUT_NAMES[id], value, expected, pass ? "PASS" : "FAIL");
      return expect(pass, NULL);
    }

    inline bool expect(bool pass, const char *s) {
      if (log && s) fprintf(stderr, "* %s : %s *\n", s, pass ? "PASS" : "FAIL");
      if (this->pass && !pass) fail_t = t;
      this->pass &= pass;
      return pass;
    }

    void poke(size_t id, mpz_t& value);
    void peek(size_t id, mpz_t& value);
    bool expect(size_t id, mpz_t& expected);

#ifdef LOADMEM
    void read_mem(size_t addr, mpz_t& value);
    void write_mem(size_t addr, mpz_t& value);
#endif

    // A default reset scheme that pulses the global chisel reset
    void target_reset(int pulse_start = 1, int pulse_length = 5);

    inline uint64_t cycles() { return t; }
    uint64_t rand_next(uint64_t limit) { return gen() % limit; }

#ifdef ENABLE_SNAPSHOT
  private:
    // sample information
#ifdef KEEP_SAMPLES_IN_MEM
    sample_t** samples;
#endif
    sample_t* last_sample;
    size_t sample_num;
    size_t last_sample_id;
    std::string sample_file;

    size_t tracelen;
    size_t trace_count;

    // profile information
    bool profile;
    size_t sample_count;
    midas_time_t sample_time;
    midas_time_t sim_start_time;

    void init_sampling(int argc, char** argv);
    void finish_sampling();
    void reservoir_sampling(size_t n);
    size_t trace_ready_valid_bits(
      sample_t* sample, bool poke, size_t id, size_t bits_id);
    inline void save_sample();

  protected:
    size_t get_tracelen() const { return tracelen; }
    sample_t* read_snapshot();
    sample_t* read_traces(sample_t* s);
#endif
};

#endif // __SIMIF_H
