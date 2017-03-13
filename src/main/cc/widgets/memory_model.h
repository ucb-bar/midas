#ifndef __MEMORY_MODEL_H
#define __MEMORY_MODEL_H

#include <unordered_map>
#include <fstream>

#include "simif.h"

class MemoryModel {
public:
  MemoryModel(simif_t* s, int argc, char** argv);
  // Writes out configuration to the memory model
  void init(std::string stats_file_name);
  // Reads all readable registers and dumps output to a CSV
  void profile();
  // Closes the output file
  void clean_up();

private:
  std::unordered_map<std::string, uint32_t> model_configuration;
  std::ofstream stats_file;
  simif_t * sim;
};

#endif // __MEMORY_MODEL_H
