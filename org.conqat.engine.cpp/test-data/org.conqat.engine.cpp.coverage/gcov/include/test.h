#ifndef TEST_H
#define TEST_H

#include <cstdio>
#include <iostream>

void externalFunction (int);

template<typename T>
class Printer {

 public:

  static void printOne () {
    std::cout << "called by one" << std::endl;
  }

  static void printNever () {
    std::cout << "never called" << std::endl;
  }

  static void printBoth () {
    printf("called from both\n");
  }
};

#endif // TEST_H
