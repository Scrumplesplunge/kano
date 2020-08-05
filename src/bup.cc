#include <cassert>

import parser;
import <iostream>;
import x86;

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cerr << "Usage: bup <filename>\n";
    return 1;
  }
  emit(std::cout, parse(argv[1]));
}
