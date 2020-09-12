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

std::ostream& operator<<(std::ostream& output, variable l) {
  return output << '%' << (int)l;
}

void print(std::ostream& output, variable d, const constant& c) {
  output << "  " << d << " = " << c.value;
}

void print(std::ostream& output, variable d, stack_allocate) {
  output << "  " << d << " = alloca";
}

void print(std::ostream& output, variable d, const load& l) {
  output << "  " << d << " = *" << l.address;
}

void print(std::ostream& output, variable, const store& s) {
  output << "  *" << s.address << " = " << s.value;
}

void print(std::ostream& output, variable d, const call& c) {
  output << "  " << d << " = " << c.op << '(';
  if (!c.arguments.empty()) {
    output << c.arguments[0];
    for (int i = 1, n = c.arguments.size(); i < n; i++) {
      output << ", " << c.arguments[i];
    }
    output << ')';
  }
}

void print(std::ostream& output, variable, ret) {
  output << "  ret";
}

void print(std::ostream& output, variable d, negate n) {
  output << "  " << d << " = -" << n.inner;
}

void print(std::ostream& output, variable d, add a) {
  output << "  " << d << " = " << a.left << " + " << a.right;
}

void print(std::ostream& output, variable d, subtract s) {
  output << "  " << d << " = " << s.left << " - " << s.right;
}

void print(std::ostream& output, variable d, multiply m) {
  output << "  " << d << " = " << m.left << " * " << m.right;
}

void print(std::ostream& output, variable dest, divide d) {
  output << "  " << dest << " = " << d.left << " / " << d.right;
}

void print(std::ostream& output, variable d, modulo m) {
  output << "  " << d << " = " << m.left << " % " << m.right;
}

void print(std::ostream& output, variable d, compare_eq c) {
  output << "  " << d << " = " << c.left << " == " << c.right;
}

void print(std::ostream& output, variable d, compare_ne c) {
  output << "  " << d << " = " << c.left << " != " << c.right;
}

void print(std::ostream& output, variable d, compare_lt c) {
  output << "  " << d << " = " << c.left << " < " << c.right;
}

void print(std::ostream& output, variable d, compare_le c) {
  output << "  " << d << " = " << c.left << " <= " << c.right;
}

void print(std::ostream& output, variable d, compare_gt c) {
  output << "  " << d << " = " << c.left << " > " << c.right;
}

void print(std::ostream& output, variable d, compare_ge c) {
  output << "  " << d << " = " << c.left << " >= " << c.right;
}

void print(std::ostream& output, variable, label l) {
  output << l.name << ":";
}

void print(std::ostream& output, variable, jump j) {
  output << "  jmp " << j.target;
}

void print(std::ostream& output, variable, conditional_jump c) {
  output << "  if (" << c.condition << ") jmp " << c.target;
}

void print(std::ostream& output, variable d, logical_not l) {
  output << "  " << d << " = !" << l.inner;
}

void print(std::ostream& output, variable d, index i) {
  output << "  " << d << " = " << i.address << '[' << i.offset << ']';
}

std::ostream& operator<<(std::ostream& output, const function& f) {
  int i = 0;
  const int n = f.steps.size();
  while (true) {
    if (i == n) break;
    const auto& s = f.steps[i];
    s.action.visit([&](const auto& x) { print(output, s.destination, x); });
    output << '\n';
    i++;
  }
  return output;
}

}  // namespace semantics::ir
