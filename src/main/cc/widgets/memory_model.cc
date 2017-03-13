#include <algorithm>
#include <exception>
#include <string>
#include <stdio.h>

#include "memory_model.h"

MemoryModel::MemoryModel(simif_t* sim, int argc, char** argv): sim(sim) {
  std::vector<std::string> args(argv + 1, argv + argc);
  const char* loadmem = NULL;
  // Creates a map of all arguments prefixed with +mm_, which should have a
  // 1:1 correspondence with programmable registers in the model
  for (auto &arg: args) {
    if(arg.find("+mm_") == 0) {
      auto sub_arg = std::string(arg.c_str() + 4);
      size_t delimit_idx = sub_arg.find_first_of("=");
      std::string key = sub_arg.substr(0, delimit_idx).c_str();
      int value = std::stoi(sub_arg.substr(delimit_idx+1).c_str());
      model_configuration[key] = value;
    }
  }
}

void MemoryModel::profile() {
  if (MEMMODEL_0_R_num_registers > 0) {
    stats_file << sim->read(MEMMODEL_0_R_addrs[0]);
    for (size_t i = 1; i < MEMMODEL_0_R_num_registers; i++) {
      stats_file << ", " << sim->read(MEMMODEL_0_R_addrs[i]);
    }
    stats_file << std::endl;
  }
}

void MemoryModel::init(std::string stats_file_name) {
  for (size_t i = 0; i < MEMMODEL_0_W_num_registers; i++) {
    auto value_it = model_configuration.find(std::string(MEMMODEL_0_W_names[i]));
    if (value_it != model_configuration.end()) {
      sim->write(MEMMODEL_0_W_addrs[i], value_it->second);
    } else {
      char buf[100];
      sprintf(buf, "No value provided for configuration register: %s", MEMMODEL_0_W_names[i]);
      throw std::runtime_error(buf);
    }
  }

  stats_file.open(stats_file_name, std::ofstream::out);
  if(!stats_file.is_open()) {
    throw std::runtime_error("Could not open output file: " + stats_file_name);
  }

  // Label the columns of the CSV
  if (MEMMODEL_0_R_num_registers > 0) {
    stats_file << "// " <<  MEMMODEL_0_R_names[0];
    for (size_t i = 1; i < MEMMODEL_0_R_num_registers; i++) {
      stats_file << ", " << MEMMODEL_0_R_names[i];
    }
    stats_file << std::endl;
  }
}

void MemoryModel::clean_up() {
  stats_file.close();
}
