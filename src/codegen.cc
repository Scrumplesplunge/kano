import syntax;
import semantics.ir;
import semantics.show;
import <iostream>;

namespace ir = ::semantics::ir;

template <typename F>
struct visit_locals {
  F f;
  void operator()(const ir::step& s) {
    f(s.destination);
    s.action.visit([this](const auto& x) { (*this)(x); });
  }
  void operator()(const ir::constant&) {}
  void operator()(const ir::stack_allocate&) {}
  void operator()(const ir::load& l) {
    f(l.address);
  }
  void operator()(const ir::store& s) {
    f(s.address);
    f(s.value);
  }
  void operator()(const ir::call& c) {
    f(c.op);
    for (auto a : c.arguments) f(a);
  }
  void operator()(const ir::ret&) {}
  void operator()(const ir::negate& n) {
    f(n.inner);
  }
  void operator()(const ir::add& a) {
    f(a.left);
    f(a.right);
  }
  void operator()(const ir::subtract& s) {
    f(s.left);
    f(s.right);
  }
  void operator()(const ir::multiply& m) {
    f(m.left);
    f(m.right);
  }
  void operator()(const ir::divide& d) {
    f(d.left);
    f(d.right);
  }
  void operator()(const ir::modulo& m) {
    f(m.left);
    f(m.right);
  }
  void operator()(const ir::compare_eq& c) {
    f(c.left);
    f(c.right);
  }
  void operator()(const ir::compare_ne& c) {
    f(c.left);
    f(c.right);
  }
  void operator()(const ir::compare_lt& c) {
    f(c.left);
    f(c.right);
  }
  void operator()(const ir::compare_le& c) {
    f(c.left);
    f(c.right);
  }
  void operator()(const ir::compare_gt& c) {
    f(c.left);
    f(c.right);
  }
  void operator()(const ir::compare_ge& c) {
    f(c.left);
    f(c.right);
  }
  void operator()(const ir::label&) {}
  void operator()(const ir::jump&) {}
  void operator()(const ir::conditional_jump& c) {
    f(c.condition);
  }
  void operator()(const ir::logical_not& l) {
    f(l.inner);
  }
  void operator()(const ir::index& i) {
    f(i.address);
    f(i.offset);
  }
};
template <typename F> visit_locals(F) -> visit_locals<F>;

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

constexpr bool is_void(const ir::data_type& t) {
  if (!t) return true;
  const auto* b = t.get<ir::builtin_type>();
  return b && *b == ir::void_type;
}

// This register allocation is a simple linear scan, completely ignoring the
// control flow inside the function. This works fine for IR which the checker
// currently generates, since locals are only used within logical basic blocks
// and everything else uses stack allocations.
// TODO: Make this function work with more complicated control flow graphs, or
// at least add some debug assertions to detect cases where the allocation will
// be incorrect.
void emit(const ir::function& f) {
  // TODO: Compute the stack frame size and emit instructions to reserve it.
  struct live_range { int begin = 999'999'999, end = -1; };
  std::map<ir::local, live_range> live_ranges;
  for (int i = 0, n = f.steps.size(); i < n; i++) {
    visit_locals{[&](ir::local s) {
      auto& range = live_ranges[s];
      range.begin = std::min(range.begin, i);
      range.end = std::max(range.end, i);
    }}(f.steps[i]);
  }
  std::map<ir::local, reg> assignments;
  std::vector<reg> available = {spill1, spill2, spill3, spill4, spill5, spill6,
                                spill7, spill8, ebx,    ecx,    edi,    esi};
  std::map<reg, ir::local> in_use;
  for (int i = 0, n = f.steps.size(); i < n; i++) {
    // Collect registers for which the live range has ended.
    for (auto j = in_use.begin(); j != in_use.end();) {
      const auto& range = live_ranges.at(j->second);
      if (range.end < i) {
        available.push_back(j->first);
        j = in_use.erase(j);
      } else {
        ++j;
      }
    }
    const auto& x = f.steps[i];
    const auto& type = f.locals.at(x.destination);
    // If this step yields nothing, don't allocate a register for it.
    if (is_void(type)) continue;
    // If this step is an alloca, we don't need a register allocation. The
    // alloca represents a constant expression of the form -x(%ebp), so we can
    // substitute that in directly after computing x.
    // TODO: Consider merging this step into a constant-folding pass.
    if (x.action.is<ir::stack_allocate>()) continue;
    if (!available.empty()) {
      std::cout << "  " << x.destination << " -> " << available.back() << '\n';
      assignments.emplace(x.destination, available.back());
      in_use.emplace(available.back(), x.destination);
      available.pop_back();
      continue;
    }
    // TODO: Implement register spilling.
    io::fatal_message{type.location(), io::message::error}
        << "register spilling is not implemented.";
  }
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
