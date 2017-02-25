#include "sample.h"
#include <cassert>
#include <cstring>
#include <fstream>
#include <sstream>

#ifdef ENABLE_SNAPSHOT
std::array<std::vector<std::string>, CHAIN_NUM> sample_t::signals = {};
std::array<std::vector<size_t>,      CHAIN_NUM> sample_t::widths  = {};
std::array<std::vector<int>,         CHAIN_NUM> sample_t::depths = {};
size_t sample_t::chain_len[CHAIN_NUM] = {0};
size_t sample_t::chain_loop[CHAIN_NUM] = {0};
void dump_f(FILE *file,
           SAMPLE_INST_TYPE type,
           const size_t t,
           const size_t id,
           data_t* const value,
           const size_t size,
           const int* const idx) {
  fprintf(file, "%u %zu %zu ", type, t, id);
  fprintf(file, "%x", value[size-1]);
  for (int i = size - 2 ; i >= 0 ; i--) {
    fprintf(file, "%08x", value[i]);
  }
  if (idx) fprintf(file, " %d", *idx);
  fprintf(file, "\n");
}

std::ostream& dump_s(std::ostream &os,
                     SAMPLE_INST_TYPE type,
                     const size_t t,
                     const size_t id,
                     data_t* const value,
                     const size_t size,
                     const int* const idx) {
  os << type << " " << t << " " << id << " ";
  os << std::hex << value[size-1];
  for (int i = size - 2 ; i >= 0 ; i--) {
    os << std::setfill('0') << std::setw(2*sizeof(data_t)) << value[i];
  }
  os << std::setfill(' ') << std::setw(0) << std::dec;
  if (idx) os << " " << *idx;
  os << std::endl;
  return os;
}

void sample_t::init_chains(std::string filename) {
  std::fill(signals.begin(), signals.end(), std::vector<std::string>());
  std::fill(widths.begin(),  widths.end(),  std::vector<size_t>());
  std::fill(depths.begin(), depths.end(), std::vector<int>());
  std::ifstream file(filename.c_str());
  if (!file) {
    fprintf(stderr, "Cannot open %s\n", filename.c_str());
    exit(EXIT_FAILURE);
  }
  std::string line;
  while (std::getline(file, line)) {
    std::istringstream iss(line);
    size_t type;
    std::string signal;
    iss >> type >> signal;
    size_t width;
    int depth;
    iss >> width >> depth;
    if (signal == "null") signal = "";
    signals[type].push_back(signal);
    widths[type].push_back(width);
    depths[type].push_back(depth);
    chain_len[type] += width;
    switch ((CHAIN_TYPE) type) {
      case SRAM_CHAIN:
        if (!signal.empty() && depth > 0) {
          chain_loop[type] = std::max(chain_loop[type], (size_t) depth);
        }
        break;
      default:
        chain_loop[type] = 1;
        break;
    }
  }
  for (size_t t = 0 ; t < CHAIN_NUM ; t++) {
    chain_len[t] /= DAISY_WIDTH;
  }
  file.close();
}

void sample_t::dump_chains(FILE* file) {
  for (size_t t = 0 ; t < CHAIN_NUM ; t++) {
    auto chain_signals = signals[t];
    auto chain_widths = widths[t];
    for (size_t id = 0 ; id < chain_signals.size() ; id++) {
      auto signal = chain_signals[id];
      auto width = chain_widths[id];
      fprintf(file, "%u %zu %s %zu\n", SIGNALS, t, signal.empty() ? "null" : signal.c_str(), width);
    }
  }
  for (size_t id = 0 ; id < IN_TR_SIZE ; id++) {
    fprintf(file, "%u %u %s\n", SIGNALS, IN_TR, IN_TR_NAMES[id]);
  }
  for (size_t id = 0 ; id < OUT_TR_SIZE ; id++) {
    fprintf(file, "%u %u %s\n", SIGNALS, OUT_TR, OUT_TR_NAMES[id]);
  }
}

void sample_t::dump_chains(std::ostream& os) {
  for (size_t t = 0 ; t < CHAIN_NUM ; t++) {
    auto chain_signals = signals[t];
    auto chain_widths = widths[t];
    for (size_t id = 0 ; id < chain_signals.size() ; id++) {
      auto signal = chain_signals[id];
      auto width = chain_widths[id];
      os << SIGNALS << " " << t << " " << (signal.empty() ? "null" : signal) << width << std::endl;
    }
  }
  for (size_t id = 0 ; id < IN_TR_SIZE ; id++) {
    os << SIGNALS << " " << IN_TR << " " << IN_TR_NAMES[id] << std::endl;
  }
  for (size_t id = 0 ; id < OUT_TR_SIZE ; id++) {
    os << SIGNALS << " " << OUT_TR << " " << OUT_TR_NAMES[id] << std::endl;
  }
}

size_t sample_t::read_chain(CHAIN_TYPE type, const char* snap, size_t start) {
  size_t t = static_cast<size_t>(type);
  auto chain_signals = signals[t];
  auto chain_widths = widths[t];
  auto chain_depths = depths[t];
  for (size_t i = 0 ; i < chain_loop[type] ; i++) {
    for (size_t s = 0 ; s < chain_signals.size() ; s++) {
      auto signal = chain_signals[s];
      auto width = chain_widths[s];
      auto depth = chain_depths[s];
      if (!signal.empty()) {
        char substr[1025];
        assert(width <= 1024);
        strncpy(substr, snap+start, width);
        substr[width] = '\0';
        biguint_t value(substr, 2);
#if DAISY_WIDTH > 32
        const size_t ratio = sizeof(data_t) / sizeof(uint32_t);
        const size_t size = (value.get_size() - 1) / ratio + 1;
        data_t* data = new data_t[size](); // zero-out
        // TODO: better way to copy?
        for (size_t i = 0 ; i < size ; i++) {
          for (size_t j = 0 ; j < ratio ; j++) {
            data[i] |= ((data_t)value[i * ratio + j]) << 32 * j;
          }
        }
#else
	const size_t size = value.get_size();
        data_t* data = new data_t[size];
        std::copy(value.get_data(), value.get_data() + value.get_size(), data);
#endif
        switch(type) {
          case TRACE_CHAIN:
            add_cmd(new force_t(type, s, data, size));
            break;
          case REGS_CHAIN:
            add_cmd(new load_t(type, s, data, size, -1));
            break;
          case SRAM_CHAIN:
            if (static_cast<int>(i) < depth)
              add_cmd(new load_t(type, s, data, size, i));
            break;
          case CNTR_CHAIN:
            add_cmd(new count_t(type, s, data, size));
            break;
          default:
            break;
        }
      }
      start += width;
    }
    assert(start % DAISY_WIDTH == 0);
  }
  return start;
}

sample_t::sample_t(const char* snap, uint64_t _cycle):
    cycle(_cycle), force_prev_id(-1) {
  size_t start = 0;
  for (size_t t = 0 ; t < CHAIN_NUM ; t++) {
    CHAIN_TYPE type = static_cast<CHAIN_TYPE>(t);
    start = read_chain(type, snap, start);
  }
}

sample_t::sample_t(CHAIN_TYPE type, const char* snap, uint64_t _cycle):
    cycle(_cycle), force_prev_id(-1) {
  read_chain(type, snap);
}
#endif

sample_t::~sample_t() {
  for (size_t i = 0 ; i < cmds.size() ; i++) {
    delete cmds[i];
  }
  cmds.clear();
}
