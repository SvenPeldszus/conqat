#include "test.h"

void called () {
  Printer<int>::printBoth();
}

void notCalled () {
  Printer<int>::printNever();
}

int main () {
  called ();
  externalFunction (17);
}
