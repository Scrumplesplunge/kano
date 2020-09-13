import syntax;
import semantics.ir;
import semantics.show;
import <concepts>;
import <iostream>;

#include <cassert>

namespace ir = ::semantics::ir;

template <typename F>
struct visit_operands {
  F f;
  void operator()(const ir::step& s) {
    s.visit([this](const auto& x) { (*this)(x); });
  }
  void operator()(const ir::copy& c) {
    f(c.result);
    f(c.value);
  }
  void operator()(const ir::load& l) {
    f(l.result);
    f(l.address);
  }
  void operator()(const ir::store& s) {
    f(s.address);
    f(s.value);
  }
  void operator()(const ir::call& c) {
    f(c.result);
    f(c.op);
    for (auto a : c.arguments) f(a);
  }
  void operator()(const ir::ret&) {}
  void operator()(const ir::negate& n) {
    f(n.result);
    f(n.inner);
  }
  void operator()(const ir::add& a) {
    f(a.result);
    f(a.left);
    f(a.right);
  }
  void operator()(const ir::subtract& s) {
    f(s.result);
    f(s.left);
    f(s.right);
  }
  void operator()(const ir::multiply& m) {
    f(m.result);
    f(m.left);
    f(m.right);
  }
  void operator()(const ir::divide& d) {
    f(d.result);
    f(d.left);
    f(d.right);
  }
  void operator()(const ir::modulo& m) {
    f(m.result);
    f(m.left);
    f(m.right);
  }
  void operator()(const ir::compare_eq& c) {
    f(c.result);
    f(c.left);
    f(c.right);
  }
  void operator()(const ir::compare_ne& c) {
    f(c.result);
    f(c.left);
    f(c.right);
  }
  void operator()(const ir::compare_lt& c) {
    f(c.result);
    f(c.left);
    f(c.right);
  }
  void operator()(const ir::compare_le& c) {
    f(c.result);
    f(c.left);
    f(c.right);
  }
  void operator()(const ir::compare_gt& c) {
    f(c.result);
    f(c.left);
    f(c.right);
  }
  void operator()(const ir::compare_ge& c) {
    f(c.result);
    f(c.left);
    f(c.right);
  }
  void operator()(const ir::label&) {}
  void operator()(const ir::jump&) {}
  void operator()(const ir::conditional_jump& c) {
    f(c.condition);
  }
  void operator()(const ir::logical_not& l) {
    f(l.result);
    f(l.inner);
  }
  void operator()(const ir::index& i) {
    f(i.result);
    f(i.address);
    f(i.offset);
  }
};
template <typename F> visit_operands(F) -> visit_operands<F>;

template <typename T>
concept step = requires (const ir::step& s) {
  { s.get<T>() };
};

template <typename T>
concept has_result = requires (const T& x) {
  { x.result } -> std::same_as<ir::operand>;
};

constexpr struct {
  template <typename T>
  requires step<T>
  const ir::variable* operator()(const T&) const {
    return nullptr;
  }
  template <typename T>
  requires step<T> && has_result<T>
  const ir::variable* operator()(const T& x) const {
    return std::get_if<ir::variable>(&x.result);
  }
} result;

// TODO: Make use of eax and edx as well. Currently they are not used so that
// they are available for use with `mul` or `div` without having to shuffle
// anything else out of the way.
enum reg {
  ebx,
  ecx,
  edi,
  esi,
  spill1,
  spill2,
  spill3,
  spill4,
  spill5,
  spill6,
  spill7,
  spill8,
};

std::ostream& operator<<(std::ostream& output, reg r) {
  switch (r) {
    case ebx: return output << "ebx";
    case ecx: return output << "ecx";
    case edi: return output << "edi";
    case esi: return output << "esi";
    case spill1: return output << "spill1";
    case spill2: return output << "spill2";
    case spill3: return output << "spill3";
    case spill4: return output << "spill4";
    case spill5: return output << "spill5";
    case spill6: return output << "spill6";
    case spill7: return output << "spill7";
    case spill8: return output << "spill8";
  }
}

struct live_range { int begin = 999'999'999, end = -1; };

std::map<ir::variable, live_range> live_ranges(const ir::function& f) {
  std::map<ir::variable, live_range> live_ranges;
  for (int i = 0, n = f.steps.size(); i < n; i++) {
    visit_operands{[&](const ir::operand& o) {
      if (auto* v = std::get_if<ir::variable>(&o)) {
        auto& range = live_ranges[*v];
        range.begin = std::min(range.begin, i);
        range.end = std::max(range.end, i);
      }
    }}(f.steps[i]);
  }
  return live_ranges;
}

// This register allocation is a simple linear scan, completely ignoring the
// control flow inside the function. This works fine for IR which the checker
// currently generates, since variables are only used within logical basic
// blocks and everything else uses stack allocations.
// TODO: Make this function work with more complicated control flow graphs, or
// at least add some debug assertions to detect cases where the allocation will
// be incorrect.
std::map<ir::variable, std::variant<reg, ir::local>> allocate_registers(
    const ir::function& f) {
  const auto ranges = live_ranges(f);
  std::map<ir::variable, std::variant<reg, ir::local>> assignments;
  std::vector<reg> available = {ebx, ecx, edi, esi};
  std::map<reg, ir::variable> in_use;
  for (int i = 0, n = f.steps.size(); i < n; i++) {
    // Collect registers for which the live range has ended.
    for (auto j = in_use.begin(); j != in_use.end();) {
      const auto& range = ranges.at(j->second);
      if (range.end < i) {
        available.push_back(j->first);
        j = in_use.erase(j);
      } else {
        ++j;
      }
    }
    const auto& x = f.steps[i];
    auto* destination = x.visit(result);
    // The only kind of operand which needs register allocation is a plain
    // variable. All other kinds are either constants or references to existing
    // variables.
    if (!destination) continue;
    if (available.empty()) {
      assert(!in_use.empty());
      const auto by_range_end = [&](const auto& l, const auto& r) {
        return ranges.at(l.second).end < ranges.at(r.second).end;
      };
      auto i = std::max_element(in_use.begin(), in_use.end(), by_range_end);
      const auto local = ir::make_local();
      assignments.at(i->second) = local;
      i->second = *destination;
      assignments.emplace(*destination, i->first);
    } else {
      assignments.emplace(*destination, available.back());
      in_use.emplace(available.back(), *destination);
      available.pop_back();
    }
  }
  return assignments;
}

void emit(const ir::function& f) {
  // TODO: Compute the stack frame size and emit instructions to reserve it.
  for (const auto& [v, x] : allocate_registers(f)) {
    std::cout << "  " << v << " -> ";
    std::visit([](const auto& x) { std::cout << x; }, x);
    std::cout << '\n';
  }
  // TODO: Actually emit the code.
}

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cerr << "Usage: ./demo <filename>\n";
    return 1;
  }
  auto program = syntax::check(argv[1]);
  std::cout << "_start:\n" << program.initialization << '\n';
  for (const auto& [s, f] : program.functions) {
    std::cout << s << ":\n" << f << '\n';
  }
  std::cout << "=====\n";
  emit(program.initialization);
  for (const auto& [s, f] : program.functions) emit(f);
}
