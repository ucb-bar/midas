#ifndef __ENDPOINT_H
#define __ENDPOINT_H

#include "simif.h"
#include "address_map.h"

class endpoint_t
{
public:
  endpoint_t(simif_t* s, AddressMap addr_map): sim(s), addr_map(addr_map) { }
  virtual void tick() = 0;
  virtual void init() { }
  virtual bool stall() = 0;
  virtual bool done() = 0;

protected:
  AddressMap addr_map;
  inline void write(size_t addr, data_t data) {
    sim->write(addr, data);
  }
  
  inline data_t read(size_t addr) {
    return sim->read(addr);
  }

  void write(std::string reg, data_t data){
    sim->write(addr_map.w_addr(reg), data);
  }

  data_t read(std::string reg){
    return sim->read(addr_map.r_addr(reg));
  }

private:
  simif_t *sim;
};

#endif // __ENDPOINT_H
