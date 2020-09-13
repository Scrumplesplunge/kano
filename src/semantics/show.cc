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

std::ostream& operator<<(std::ostream& output, const local& l) {
  return output << "-local" << (int)l << "(%ebp)";
}

std::ostream& operator<<(std::ostream& output, const operand& o) {
  std::visit([&](const auto& x) { output << x; }, o);
  return output;
}

void print(std::ostream& output, const copy& c) {
  output << "  " << c.result << " = " << c.value;
}

void print(std::ostream& output, const load& l) {
  output << "  " << l.result << " = *" << l.address;
}

void print(std::ostream& output, const store& s) {
  output << "  *" << s.address << " = " << s.value;
}

void print(std::ostream& output, const call& c) {
  output << "  " << c.result << " = " << c.op << '(';
  if (!c.arguments.empty()) {
    output << c.arguments[0];
    for (int i = 1, n = c.arguments.size(); i < n; i++) {
      output << ", " << c.arguments[i];
    }
    output << ')';
  }
}

void print(std::ostream& output, ret) {
  output << "  ret";
}

void print(std::ostream& output, negate n) {
  output << "  " << n.result << " = -" << n.inner;
}

void print(std::ostream& output, add a) {
  output << "  " << a.result << " = " << a.left << " + " << a.right;
}

void print(std::ostream& output, subtract s) {
  output << "  " << s.result << " = " << s.left << " - " << s.right;
}

void print(std::ostream& output, multiply m) {
  output << "  " << m.result << " = " << m.left << " * " << m.right;
}

void print(std::ostream& output, divide d) {
  output << "  " << d.result << " = " << d.left << " / " << d.right;
}

void print(std::ostream& output, modulo m) {
  output << "  " << m.result << " = " << m.left << " % " << m.right;
}

void print(std::ostream& output, compare_eq c) {
  output << "  " << c.result << " = " << c.left << " == " << c.right;
}

void print(std::ostream& output, compare_ne c) {
  output << "  " << c.result << " = " << c.left << " != " << c.right;
}

void print(std::ostream& output, compare_lt c) {
  output << "  " << c.result << " = " << c.left << " < " << c.right;
}

void print(std::ostream& output, compare_le c) {
  output << "  " << c.result << " = " << c.left << " <= " << c.right;
}

void print(std::ostream& output, compare_gt c) {
  output << "  " << c.result << " = " << c.left << " > " << c.right;
}

void print(std::ostream& output, compare_ge c) {
  output << "  " << c.result << " = " << c.left << " >= " << c.right;
}

void print(std::ostream& output, label l) {
  output << l.name << ":";
}

void print(std::ostream& output, jump j) {
  output << "  jmp " << j.target;
}

void print(std::ostream& output, conditional_jump c) {
  output << "  if (" << c.condition << ") jmp " << c.target;
}

void print(std::ostream& output, logical_not l) {
  output << "  " << l.result << " = !" << l.inner;
}

void print(std::ostream& output, index i) {
  output << "  " << i.result << " = " << i.address << '[' << i.offset << ']';
}

void print(std::ostream& output, const step& s) {
  s.visit([&](const auto& x) { print(output, x); });
}

std::ostream& operator<<(std::ostream& output, const function& f) {
  int i = 0;
  const int n = f.steps.size();
  while (true) {
    if (i == n) break;
    print(output, f.steps[i]);
    output << '\n';
    i++;
  }
  return output;
}

}  // namespace semantics::ir
