// Pretty-printing for Kano IR.

module;

#include <cassert>

export module semantics.show;

import semantics.ir;
import <iostream>;
import <set>;

export namespace semantics::ir {

std::ostream& operator<<(std::ostream& output, symbol s) {
  return output << "kano" << (int)s;
}

std::ostream& operator<<(std::ostream& output, void_value) {
  return output << "void{}";
}

std::ostream& operator<<(std::ostream& output, const pointer& p) {
  return output << p.pointee_type << '{' << p.symbol << '}';
}

std::ostream& operator<<(std::ostream& output, const function_pointer& f) {
  return output << f.pointee_type << '{' << f.symbol << '}';
}

std::ostream& operator<<(std::ostream& output, local l) {
  return output << '%' << (int)l;
}

void print(std::ostream& output, local d, const constant& c) {
  output << d << " = " << c.value;
}

void print(std::ostream& output, local d, stack_allocate) {
  output << d << " = alloca";
}

void print(std::ostream& output, local d, const load& l) {
  output << d << " = *" << l.address;
}

void print(std::ostream& output, local, const store& s) {
  output << '*' << s.address << " = " << s.value;
}

void print(std::ostream& output, local d, const call& c) {
  output << d << " = " << c.op << '(';
  if (!c.arguments.empty()) {
    output << c.arguments[0];
    for (int i = 1, n = c.arguments.size(); i < n; i++) {
      output << ", " << c.arguments[i];
    }
    output << ')';
  }
}

void print(std::ostream& output, local, ret) {
  output << "ret";
}

void print(std::ostream& output, local d, negate n) {
  output << d << " = -" << n.inner;
}

void print(std::ostream& output, local d, add a) {
  output << d << " = " << a.left << " + " << a.right;
}

void print(std::ostream& output, local d, subtract s) {
  output << d << " = " << s.left << " - " << s.right;
}

void print(std::ostream& output, local d, multiply m) {
  output << d << " = " << m.left << " * " << m.right;
}

void print(std::ostream& output, local dest, divide d) {
  output << dest << " = " << d.left << " / " << d.right;
}

void print(std::ostream& output, local d, modulo m) {
  output << d << " = " << m.left << " % " << m.right;
}

void print(std::ostream& output, local d, compare_eq c) {
  output << d << " = " << c.left << " == " << c.right;
}

void print(std::ostream& output, local d, compare_ne c) {
  output << d << " = " << c.left << " != " << c.right;
}

void print(std::ostream& output, local d, compare_lt c) {
  output << d << " = " << c.left << " < " << c.right;
}

void print(std::ostream& output, local d, compare_le c) {
  output << d << " = " << c.left << " <= " << c.right;
}

void print(std::ostream& output, local d, compare_gt c) {
  output << d << " = " << c.left << " > " << c.right;
}

void print(std::ostream& output, local d, compare_ge c) {
  output << d << " = " << c.left << " >= " << c.right;
}

void print(std::ostream& output, local, jump j) {
  output << "jmp " << j.target;
}

void print(std::ostream& output, local, conditional_jump c) {
  output << "if (" << c.condition << ") jmp " << c.target;
}

void print(std::ostream& output, local d, logical_not l) {
  output << d << " = !" << l.inner;
}

void print(std::ostream& output, local d, index i) {
  output << d << " = " << i.address << '[' << i.offset << ']';
}

std::ostream& operator<<(std::ostream& output, const function& f) {
  std::map<std::vector<step>::size_type, std::set<symbol>> labels;
  for (const auto& [s, x] : f.labels) labels[x].insert(s);
  int i = 0;
  const int n = f.steps.size();
  while (true) {
    for (auto label : labels[i]) output << label << ":\n";
    if (i == n) break;
    output << "  ";
    const auto& s = f.steps[i];
    s.action.visit([&](const auto& x) { print(output, s.destination, x); });
    output << '\n';
    i++;
  }
  return output;
}

}  // namespace semantics::ir
