import <iostream>;
import ir.parser;
import ir.x86;

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cerr << "Usage: bup <filename>\n";
    return 1;
  }
  ir::emit(std::cout, ir::parse(argv[1]));
}
