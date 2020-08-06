#include <cassert>

import ir.parser;
import <iostream>;
import ir.x86;
import kano.ast;
import kano.code;
import kano.parser;
import kano.codegen;

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cerr << "Usage: bup <filename>\n";
    return 1;
  }
  ir::emit(std::cout, kano::compile(kano::parse(argv[1])));
}
