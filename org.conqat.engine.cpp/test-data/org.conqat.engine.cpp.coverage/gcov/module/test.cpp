#include "test.h"

void externalFunction (int a) {
  if (a < 42) {
    Printer<double>::printBoth();
    Printer<double>::printOne();
  } else {
    printf ("No calls here");
  }
}
