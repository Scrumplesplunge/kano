import <iostream>;
import ir.parser;
import ir.x86;
import kano.ast;
import kano.code;
import kano.codegen;
import kano.parser;

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cerr << "Usage: kano <filename>\n";
    return 1;
  }
  ir::emit(std::cout, kano::compile(kano::parse(argv[1])));
}
