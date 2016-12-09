
#include <iostream>
#include <queue>
#include <vector>
#include <cstdint>
#include <stdio.h>
#include "swmodel.h"
#include <string>
#include <algorithm>

using namespace std;

class ModelA : public SWModel {
  private:
    string name;
    long incAmt;

  public:
    ModelA(string _name, long _incAmt) {
      name = _name;
      incAmt = _incAmt;
      std::cout << "Creating ModelA named " << name << "!\n";
    }
    ModelA(const ModelA&) = delete; // no copy

    bool tick(void) {
      cout << "Ticking " << name << endl;
      if (inputChannel->size() > 0) {
        cout << "  Something in the input channel!" << endl;
        uint64_t input = inputChannel->front();
        inputChannel->pop(); // remove
        cout << "  Got input " << input << "!\n";
        outputChannel->push(input + incAmt);
        return true;
      }
      cout << "  Nothing in input queue!" << endl;
      return false;
    }
};

// TODO better name
// Simple wrapper for models to include ticked
class ModelEntry {
  public:
    SWModel* model;
    bool ticked = false;

    ModelEntry(SWModel* _model) {
      model = _model;
    }
};

int main(int argc, char** argv) {
  printf("Hello World!\n");

  long runtime = 10;

  // Create Models
  vector<ModelEntry*> models;
  models.push_back(new ModelEntry(new ModelA("Model 1", 1)));
  models.push_back(new ModelEntry(new ModelA("Model 2", 2)));
  cout << "models created!" << endl;

  // Connect
  models[0]->model->connect(models[1]->model);
  models[1]->model->connect(models[0]->model);
  cout << "models connected!" << endl;

  // Start with a number
  models[0]->model->inputChannel->push(1L);
  models[1]->model->inputChannel->push(3L);
  cout << "initial tokens pushed!" << endl;

  // Simulation loop
  for (auto i = 0; i < runtime; i++) {
    cout << " ***** Cycle " << i << " *****" << endl;

    // Start with no one ticked
    for_each(models.begin(), models.end(), [](ModelEntry* e){e->ticked = false;});
    cout << "ticked all set to false" << endl;

    // While not everyone has ticked, keep trying
    // In degenerate shared memory all SW models case, everyone will tick first try
    while (!all_of(models.begin(), models.end(), [](ModelEntry* e){return e->ticked;})) {
      cout << "someone hasn't ticked yet!" << endl;
      for_each(models.begin(), models.end(), [](ModelEntry* e){
        if (!e->ticked) { // Don't tick if you've already ticked this cycle;
          e->ticked = e->model->tick();
        }
      });
    }
  }

  return 0;
}
