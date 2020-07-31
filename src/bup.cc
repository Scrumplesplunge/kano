#include <cassert>

import parser;
import <iostream>;

struct escaped { std::string_view value; };

std::ostream& operator<<(std::ostream& output, escaped e) {
  output << '"';
  for (char c : e.value) {
    switch (c) {
      case '\n': output << "\\n"; break;
      case '\\': output << "\\\\"; break;
      case '\"': output << "\\\""; break;
      default: output << c; break;
    }
  }
  return output << '"';
}

void print(const ast::expr&);

void print(const std::int32_t& x) {
  std::cout << x;
}

void print(const ast::local& l) {
  int offset(l);
  std::cout << "(ebp " << (offset < 0 ? '-' : '+')
            << (offset < 0 ? -offset : offset) << ")";
}

void print(const ast::global& g) {
  std::cout << g.name;
}

void print(const ast::loadw& l) {
  std::cout << "(*";
  print(l.inner);
  std::cout << ')';
}

void print(const ast::add& a) {
  std::cout << '(';
  print(a.left);
  std::cout << " + ";
  print(a.right);
  std::cout << ')';
}

void print(const ast::callw& c) {
  print(c.callee);
  std::cout << "(";
  bool first = true;
  for (const auto& x : c.args) {
    if (first) {
      first = false;
    } else {
      std::cout << ", ";
    }
    print(x);
  }
  std::cout << ')';
}

void print(const ast::builtin& b) {
  switch (b) {
    case ast::builtin::write: std::cout << "builtin::write"; break;
    case ast::builtin::exit: std::cout << "builtin::exit"; break;
  }
}

void print(const ast::expr& e) {
  std::visit([&](const auto& x) {
    void (*f)(const decltype(x)&) = print;
    return f(x);
  }, *e.value);
}

void print(ast::label l) {
  std::cout << l.name << ":\n";
}

void print(const ast::jump& j) {
  std::cout << "  jmp " << j.label << '\n';
}

void print(const ast::jumpc& j) {
  std::cout << "  # TODO: if (";
  print(j.cond);
  std::cout << ") goto " << j.label << ";\n";
}

void print(const ast::call& c) {
  std::cout << "  # TODO: ";
  print(c.callee);
  std::cout << "(";
  bool first = true;
  for (const auto& x : c.args) {
    if (first) {
      first = false;
    } else {
      std::cout << ", ";
    }
    print(x);
  }
  std::cout << ");\n";
}

void print(const ast::retw& r) {
  std::cout << "  # TODO: return ";
  print(r.value);
  std::cout << ";\n";
}

void print(const ast::storew& s) {
  std::cout << "  # TODO: *(int*)";
  print(s.location);
  std::cout << " = ";
  print(s.value);
  std::cout << ";\n";
}

void print(ast::ret) {
  std::cout << "  ret\n";
}

void print(const ast::stmt& s) {
  std::visit([&](const auto& x) { print(x); }, *s.value);
}

void debug(const ast::program& program) {
  std::unordered_map<std::string_view, const ast::zeros*> bss;
  std::unordered_map<std::string_view, const ast::rodata*> rodata;
  std::unordered_map<std::string_view, const ast::function*> text;
  for (const auto& [name, definition] : program) {
    switch (definition.value.index()) {
      case ast::definition::zeros:
        bss.emplace(name, &std::get<ast::definition::zeros>(definition.value));
        break;
      case ast::definition::rodata:
        rodata.emplace(name,
                       &std::get<ast::definition::rodata>(definition.value));
        break;
      case ast::definition::function:
        text.emplace(name,
                     &std::get<ast::definition::function>(definition.value));
        break;
    }
  }
  if (!bss.empty()) std::cout << ".section .bss\n";
  for (const auto& [name, size] : bss) {
    std::cout << name << ": .space " << (int)*size << '\n';
  }
  if (!rodata.empty()) std::cout << ".section .rodata\n";
  for (const auto& [name, value] : rodata) {
    std::cout << name << ": ";
    switch (value->index()) {
      case 0:
        std::cout << ".long " << std::get<0>(*value) << '\n';
        break;
      case 1:
        std::cout << ".ascii " << escaped{std::get<1>(*value)} << '\n';
        break;
    }
  }
  if (!text.empty()) std::cout << ".section .text\n";
  for (const auto& [name, function] : text) {
    std::cout << ".global " << name << '\n';
    std::cout << name << ":\n";
    for (const auto& code : function->code) print(code);
  }
}

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cerr << "Usage: bup <filename>\n";
    return 1;
  }
  debug(parse(argv[1]));
}
