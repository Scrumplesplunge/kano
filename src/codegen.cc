import syntax;
import semantics.ir;
import semantics.show;
import <concepts>;
import <iostream>;

namespace ir = ::semantics::ir;

template <typename F>
struct visit_variables {
  F f;
  void operator()(const ir::step& s) {
    s.visit([this](const auto& x) { (*this)(x); });
  }
  void operator()(const ir::operand& o) {
    std::visit([this](const auto& x) { (*this)(x); }, o);
  }
  void operator()(std::int32_t) {}
  void operator()(ir::symbol) {}
  void operator()(const ir::local&) {}
  void operator()(const ir::variable& v) {
    f(v);
  }
  void operator()(const ir::constant& c) {
    (*this)(c.result);
  }
  void operator()(const ir::stack_allocate& s) {
    (*this)(s.result);
  }
  void operator()(const ir::load& l) {
    (*this)(l.result);
    (*this)(l.address);
  }
  void operator()(const ir::store& s) {
    (*this)(s.address);
    (*this)(s.value);
  }
  void operator()(const ir::call& c) {
    (*this)(c.result);
    (*this)(c.op);
    for (auto a : c.arguments) (*this)(a);
  }
  void operator()(const ir::ret&) {}
  void operator()(const ir::negate& n) {
    (*this)(n.result);
    (*this)(n.inner);
  }
  void operator()(const ir::add& a) {
    (*this)(a.result);
    (*this)(a.left);
    (*this)(a.right);
  }
  void operator()(const ir::subtract& s) {
    (*this)(s.result);
    (*this)(s.left);
    (*this)(s.right);
  }
  void operator()(const ir::multiply& m) {
    (*this)(m.result);
    (*this)(m.left);
    (*this)(m.right);
  }
  void operator()(const ir::divide& d) {
    (*this)(d.result);
    (*this)(d.left);
    (*this)(d.right);
  }
  void operator()(const ir::modulo& m) {
    (*this)(m.result);
    (*this)(m.left);
    (*this)(m.right);
  }
  void operator()(const ir::compare_eq& c) {
    (*this)(c.result);
    (*this)(c.left);
    (*this)(c.right);
  }
  void operator()(const ir::compare_ne& c) {
    (*this)(c.result);
    (*this)(c.left);
    (*this)(c.right);
  }
  void operator()(const ir::compare_lt& c) {
    (*this)(c.result);
    (*this)(c.left);
    (*this)(c.right);
  }
  void operator()(const ir::compare_le& c) {
    (*this)(c.result);
    (*this)(c.left);
    (*this)(c.right);
  }
  void operator()(const ir::compare_gt& c) {
    (*this)(c.result);
    (*this)(c.left);
    (*this)(c.right);
  }
  void operator()(const ir::compare_ge& c) {
    (*this)(c.result);
    (*this)(c.left);
    (*this)(c.right);
  }
  void operator()(const ir::label&) {}
  void operator()(const ir::jump&) {}
  void operator()(const ir::conditional_jump& c) {
    (*this)(c.condition);
  }
  void operator()(const ir::logical_not& l) {
    (*this)(l.result);
    (*this)(l.inner);
  }
  void operator()(const ir::index& i) {
    (*this)(i.result);
    (*this)(i.address);
    (*this)(i.offset);
  }
};
template <typename F> visit_variables(F) -> visit_variables<F>;

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

// This register allocation is a simple linear scan, completely ignoring the
// control flow inside the function. This works fine for IR which the checker
// currently generates, since variables are only used within logical basic
// blocks and everything else uses stack allocations.
// TODO: Make this function work with more complicated control flow graphs, or
// at least add some debug assertions to detect cases where the allocation will
// be incorrect.
void emit(const ir::function& f) {
  // TODO: Compute the stack frame size and emit instructions to reserve it.
  struct live_range { int begin = 999'999'999, end = -1; };
  std::map<ir::variable, live_range> live_ranges;
  for (int i = 0, n = f.steps.size(); i < n; i++) {
    visit_variables{[&](ir::variable s) {
      auto& range = live_ranges[s];
      range.begin = std::min(range.begin, i);
      range.end = std::max(range.end, i);
    }}(f.steps[i]);
  }
  std::map<ir::variable, reg> assignments;
  std::vector<reg> available = {spill1, spill2, spill3, spill4, spill5, spill6,
                                spill7, spill8, ebx,    ecx,    edi,    esi};
  std::map<reg, ir::variable> in_use;
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
    auto* destination = x.visit(result);
    // If this step yields nothing, don't allocate a register for it.
    if (!destination) continue;
    // If this step is an alloca, we don't need a register allocation. The
    // alloca represents a constant expression of the form -x(%ebp), so we can
    // substitute that in directly after computing x.
    // TODO: Consider merging this step into a constant-folding pass.
    if (x.is<ir::stack_allocate>()) continue;
    if (!available.empty()) {
      std::cout << "  " << *destination << " -> " << available.back() << '\n';
      assignments.emplace(*destination, available.back());
      in_use.emplace(available.back(), *destination);
      available.pop_back();
      continue;
    }
    // TODO: Implement register spilling.
    io::fatal_message{f.variables.at(*destination).location(),
                      io::message::error}
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
