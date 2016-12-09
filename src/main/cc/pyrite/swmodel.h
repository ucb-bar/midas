#ifndef __SWMODEL_H
#define __SWMODEL_H

#include <queue>
#include <vector>
#include <cstdint>

using namespace std;

using MidasChannel = queue<uint64_t>;

class SWModel {
  public:
    MidasChannel* inputChannel;
    MidasChannel* outputChannel;

    virtual bool tick(void) = 0;

    void connect(SWModel* that) {
      auto channel = new MidasChannel();
      this->inputChannel = channel;
      that->outputChannel = channel;
    }
};

#endif // __SWMODEL_H
