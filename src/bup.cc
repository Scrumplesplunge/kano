#include <cassert>

import parser;
import <iostream>;

enum reg32 {
  reg_eax,
  reg_ecx,
  reg_edx,
  reg_ebx,
  reg_esp,
  reg_ebp,
  reg_esi,
  reg_edi,
};

std::ostream& operator<<(std::ostream& output, reg32 reg) {
  switch (reg) {
    case reg_eax: return output << "%eax";
    case reg_ecx: return output << "%ecx";
    case reg_edx: return output << "%edx";
    case reg_ebx: return output << "%ebx";
    case reg_esp: return output << "%esp";
    case reg_ebp: return output << "%ebp";
    case reg_esi: return output << "%esi";
    case reg_edi: return output << "%edi";
  }
}

struct emitter {
  std::ostream& output;
  std::vector<reg32> available_registers = {reg_eax, reg_ebx, reg_ecx,
                                            reg_edx, reg_esi, reg_edi};
  bool available(reg32 r) {
    const auto i =
        std::find(available_registers.begin(), available_registers.end(), r);
    return i != available_registers.end();
  }
  reg32 allocate_register(std::optional<reg32> desired) {
    if (desired) {
      const auto i = std::find(available_registers.begin(),
                               available_registers.end(), *desired);
      assert(i != available_registers.end());
      available_registers.erase(i);
      return *desired;
    } else {
      assert(!available_registers.empty());
      const reg32 x = available_registers.back();
      available_registers.pop_back();
      return x;
    }
  }
  void deallocate_register(reg32 x) {
    assert(std::find(available_registers.begin(), available_registers.end(),
                     x) == available_registers.end());
    available_registers.push_back(x);
  }
};

reg32 emit(emitter&, std::optional<reg32>, const ast::expr&);

reg32 emit(emitter& e, std::optional<reg32> desired, std::int32_t x) {
  const auto reg = e.allocate_register(desired);
  e.output << "  mov $" << x << ", " << reg << '\n';
  return reg;
}

reg32 emit(emitter& e, std::optional<reg32> desired, ast::local l) {
  const auto reg = e.allocate_register(desired);
  e.output << "  lea " << (int)l << '(' << reg_ebp << "), " << reg << '\n';
  return reg;
}

reg32 emit(emitter& e, std::optional<reg32> desired, ast::global g) {
  const auto reg = e.allocate_register(desired);
  e.output << "  mov $" << g.name << ", " << reg << '\n';
  return reg;
}

reg32 emit(emitter& e, std::optional<reg32> desired, const ast::loadw& l) {
  const auto reg = emit(e, desired, l.inner);
  e.output << "  mov (" << reg << "), " << reg << '\n';
  return reg;
}

reg32 emit(emitter& e, std::optional<reg32> desired, const ast::add& a) {
  const auto l = emit(e, desired, a.left);
  const auto r = emit(e, {}, a.right);
  e.output << "  add " << r << ", " << l << '\n';
  e.deallocate_register(r);
  return l;
}

reg32 emit_builtin(emitter& e, ast::builtin b,
                   const std::vector<ast::expr>& args) {
  switch (b) {
    case ast::builtin::write: {
      assert(args.size() == 3);
      emit(e, reg_edx, args[2]);
      emit(e, reg_ecx, args[1]);
      emit(e, reg_ebx, args[0]);
      emit(e, reg_eax, 4);
      e.output << "  int $0x80\n";
      e.deallocate_register(reg_ebx);
      e.deallocate_register(reg_ecx);
      e.deallocate_register(reg_edx);
      return reg_eax;
    }
    case ast::builtin::exit: {
      assert(args.size() == 1);
      emit(e, reg_ebx, args[0]);
      emit(e, reg_eax, 1);
      e.output << "  int $0x80\n";
      e.deallocate_register(reg_ebx);
      return reg_eax;
    }
  }
}

reg32 emit(emitter& e, std::optional<reg32> desired, const ast::callw& c) {
  if (auto* b = std::get_if<ast::builtin>(c.callee.value.get()))
    return emit_builtin(e, *b, c.args);
  // Save the existing value of eax if necessary.
  assert(e.available(reg_eax) || !desired || *desired != reg_eax);
  const bool must_save = !e.available(reg_eax);
  std::optional<reg32> output = reg_eax;
  if (must_save && !desired) output = {};
  if (desired) output = *desired;
  if (must_save) {
    e.output << "  push " << reg_eax << '\n';
    e.deallocate_register(reg_eax);
  }
  // Push the arguments.
  for (int i = c.args.size() - 1; i >= 0; i--) {
    const auto r = emit(e, {}, c.args[i]);
    e.output << "  push " << r << '\n';
    e.deallocate_register(r);
  }
  // Call the function.
  const auto r = emit(e, {}, c.callee);
  e.output << "  call " << r << '\n';
  e.deallocate_register(r);
  // Place the result in the right register.
  const auto o = e.allocate_register(output);
  if (o != reg_eax) e.output << "  mov " << reg_eax << ", " << o << '\n';
  if (must_save) e.output << "  pop " << reg_eax << '\n';
  return o;
}

reg32 emit(emitter&, ast::builtin) {
  // Builtins must be the immediate callee of a call/callw instruction.
  assert(false);
}

reg32 emit(emitter& e, std::optional<reg32> desired, const ast::expr& x) {
  assert(x.value);
  return std::visit([&](const auto& x) -> reg32 {
    return emit(e, desired, x);
  }, *x.value);
}

void emit(emitter& e, ast::label l) {
  e.output << l.name << ":\n";
}

void emit(emitter& e, const ast::jump& j) {
  e.output << "  jmp " << j.label << '\n';
}

void emit(emitter& e, const ast::jumpc& j) {
  const auto r = emit(e, {}, j.cond);
  e.output << "  test " << r << ", " << r << '\n'
           << "  jnz " << j.label << '\n';
}

void emit(emitter& e, const ast::call& c) {
  emit(e, {}, (const ast::callw&)c);
  e.deallocate_register(reg_eax);
}

void emit(emitter& e, const ast::retw& r) {
  emit(e, reg_eax, r.value);
  e.output << "  leave\n"
           << "  ret\n";
  e.deallocate_register(reg_eax);
}

void emit(emitter& e, const ast::storew& s) {
  const auto value = emit(e, {}, s.value);
  const auto destination = emit(e, {}, s.location);
  e.output << "  mov " << value << ", (" << destination << ")\n";
  e.deallocate_register(destination);
  e.deallocate_register(value);
}

void emit(emitter& e, ast::ret) {
  e.output << "  leave\n"
           << "  ret\n";
}

void emit(emitter& e, const ast::stmt& s) {
  assert(s.value);
  std::visit([&](const auto& x) { emit(e, x); }, *s.value);
}

///////////////////////////////////////////////////////////////////////////////

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

void emit(const ast::program& program) {
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
    emitter e{std::cout};
    e.output << "  mov " << reg_esp << ", " << reg_ebp << "\n";
    e.output << "  sub $" << function->frame_size << ", " << reg_esp << '\n';
    for (const auto& code : function->code) emit(e, code);
  }
}

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cerr << "Usage: bup <filename>\n";
    return 1;
  }
  emit(parse(argv[1]));
}
