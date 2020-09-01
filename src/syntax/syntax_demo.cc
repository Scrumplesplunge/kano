import syntax;
import node;

import <iostream>;
import <string>;

struct wrap;
using demo = node<int, wrap>;
struct wrap {
  demo inner;
};

struct print {
  void operator()(int x) { std::cout << x; }
  void operator()(wrap& w) {
    std::cout << '{';
    w.inner.visit(*this);
    std::cout << '}';
  }
};

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cerr << "Usage: ./demo <filename>\n";
    return 1;
  }
  syntax::check(argv[1]);
}
