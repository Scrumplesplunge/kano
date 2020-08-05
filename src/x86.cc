module;

#include <assert.h>

export module x86;

import ast;
import io;

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

// Any symbolic name in the output, e.g. a data or function label.
struct symbol { std::string_view name; };
using value = std::variant<std::int32_t, symbol>;
// Load an immediate constant.
struct immediate { value value; };
// Load a value at a constant address.
struct direct { immediate address; };
// Load a value at a constant offset from a register.
struct indirect { reg32 base; std::int32_t offset = 0; };
using operand = std::variant<immediate, reg32, direct, indirect>;

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

std::ostream& operator<<(std::ostream& output, symbol s) {
  return output << s.name;
}

std::ostream& operator<<(std::ostream& output, const value& v) {
  std::visit([&](const auto& x) { output << x; }, v);
  return output;
}

std::ostream& operator<<(std::ostream& output, const immediate& i) {
  return output << '$' << i.value;
}

std::ostream& operator<<(std::ostream& output, const direct& d) {
  return output << d.address;
}

std::ostream& operator<<(std::ostream& output, const indirect& i) {
  if (i.offset) {
    output << i.offset;
  }
  return output << '(' << i.base << ')';
}

std::ostream& operator<<(std::ostream& output, const operand& o) {
  std::visit([&](const auto& x) { output << x; }, o);
  return output;
}

class emitter {
 public:
  emitter(std::ostream& output) : output_(&output) {}

  void set_location(io::location location) { location_ = location; }

  bool is_available(reg32 r) const {
    return std::find(unused_.begin(), unused_.end(), r) != unused_.end();
  }

  // Allocate an available register, or exit if none are available.
  reg32 allocate_register() {
    if (unused_.empty()) die() << "no available registers.";
    const reg32 x = unused_.back();
    unused_.pop_back();
    return x;
  }

  // Allocate a specific register, or exit if it is not available.
  reg32 allocate_register(reg32 desired) {
    if (!is_available(desired)) {
      die() << "required register " << desired << " is already taken.";
    }
    const auto i = std::find(unused_.begin(), unused_.end(), desired);
    unused_.erase(i);
    return desired;
  }

  // Convenience wrapper that allows optionally specifying the output register.
  reg32 allocate_register(std::optional<reg32> desired) {
    return desired ? allocate_register(*desired) : allocate_register();
  }

  void deallocate_register(reg32 x) {
    assert(!is_available(x));
    unused_.push_back(x);
  }

  io::fatal_message die() const {
    return {"<unknown>", location_, io::message::error};
  }

  void emit_op(std::string_view op) {
    *output_ << "  " << op << '\n';
  }

  template <typename A>
  void emit_op(std::string_view op, const A& a) {
    *output_ << "  " << op << " " << a << '\n';
  }

  template <typename A, typename B>
  void emit_op(std::string_view op, const A& a, const B& b) {
    *output_ << "  " << op << " " << a << ", " << b << '\n';
  }

  void emit_label(std::string_view here) {
    *output_ << here << ":\n";
  }

  void emit_movl(operand from, operand to) { emit_op("movl", from, to); }
  void emit_lea(operand from, operand to) { emit_op("lea", from, to); }
  void emit_addl(operand from, operand to) { emit_op("addl", from, to); }
  void emit_subl(operand from, operand to) { emit_op("subl", from, to); }
  void emit_pushl(operand from) { emit_op("pushl", from); }
  void emit_popl(operand to) { emit_op("popl", to); }
  void emit_call(operand f) { emit_op("call", f); }
  void emit_jmp(std::string_view to) { emit_op("jmp", to); }
  void emit_jnz(std::string_view to) { emit_op("jnz", to); }
  void emit_test(operand a, operand b) { emit_op("test", a, b); }
  void emit_leave() { emit_op("leave"); }
  void emit_ret() { emit_op("ret"); }
  void emit_int(std::uint8_t x) { emit_op("int", immediate{x}); }

  // TODO: Improve the code generation for expressions. Currently every
  // expression yields a register operand when in reality some could produce
  // different expressions such as immediates or indirect operands.

  reg32 emit(std::optional<reg32> output, std::int32_t x) {
    const auto reg = allocate_register(output);
    emit_movl(immediate{x}, reg);
    return reg;
  }

  reg32 emit(std::optional<reg32> output, ast::local l) {
    const auto reg = allocate_register(output);
    emit_lea(indirect{reg_ebp, (int)l}, reg);
    return reg;
  }

  reg32 emit(std::optional<reg32> output, ast::global g) {
    const auto reg = allocate_register(output);
    emit_movl(immediate{symbol{g.name}}, reg);
    return reg;
  }

  reg32 emit(std::optional<reg32> output, const ast::loadw& l) {
    const auto reg = emit(output, l.inner);
    emit_movl(indirect{reg}, reg);
    return reg;
  }

  reg32 emit(std::optional<reg32> output, const ast::add& a) {
    const auto l = emit(output, a.left);
    const auto r = emit({}, a.right);
    emit_addl(r, l);
    deallocate_register(r);
    return l;
  }

  reg32 emit_builtin(ast::builtin b, const std::vector<ast::expr>& args) {
    switch (b) {
      case ast::builtin::write: {
        assert(args.size() == 3);
        emit(reg_edx, args[2]);
        emit(reg_ecx, args[1]);
        emit(reg_ebx, args[0]);
        emit(reg_eax, 4);
        emit_int(0x80);
        deallocate_register(reg_ebx);
        deallocate_register(reg_ecx);
        deallocate_register(reg_edx);
        return reg_eax;
      }
      case ast::builtin::exit: {
        assert(args.size() == 1);
        emit(reg_ebx, args[0]);
        emit(reg_eax, 1);
        emit_int(0x80);
        deallocate_register(reg_ebx);
        return reg_eax;
      }
    }
  }

  reg32 emit(std::optional<reg32> desired, const ast::callw& c) {
    if (auto* b = std::get_if<ast::builtin>(c.callee.value.get())) {
      return emit_builtin(*b, c.args);
    }
    // Save the existing value of eax if necessary.
    const bool must_save = !is_available(reg_eax);
    std::optional<reg32> output = reg_eax;
    if (must_save && !output) output = {};
    if (desired) output = *desired;
    if (must_save) {
      emit_pushl(reg_eax);
      deallocate_register(reg_eax);
    }
    // Push the arguments.
    for (int i = c.args.size() - 1; i >= 0; i--) {
      const auto r = emit({}, c.args[i]);
      emit_pushl(r);
      deallocate_register(r);
    }
    // Call the function.
    const auto r = emit({}, c.callee);
    emit_call(r);
    deallocate_register(r);
    // Place the result in the right register.
    const auto o = allocate_register(output);
    if (o != reg_eax) emit_movl(reg_eax, o);
    if (must_save) emit_popl(reg_eax);
    return o;
  }

  reg32 emit(std::optional<reg32> output, const ast::expr& x) {
    assert(x.value);
    return std::visit([&](const auto& x) -> reg32 { return emit(output, x); },
                      *x.value);
  }

  void emit(ast::label l) { emit_label(l.name); }

  void emit(const ast::jump& j) { emit_jmp(j.label); }

  void emit(const ast::jumpc& j) {
    const auto r = emit({}, j.cond);
    emit_test(r, r);
    deallocate_register(r);
    emit_jnz(j.label);
  }

  void emit(const ast::call& c) {
    const auto r = emit({}, (const ast::callw&)c);
    deallocate_register(r);
  }

  void emit(const ast::retw& r) {
    emit(reg_eax, r.value);
    emit_leave();
    emit_ret();
    deallocate_register(reg_eax);
  }

  void emit(const ast::storew& s) {
    const auto value = emit({}, s.value);
    const auto destination = emit({}, s.location);
    emit_movl(value, indirect{destination});
    deallocate_register(destination);
    deallocate_register(value);
  }

  void emit(ast::ret) {
    emit_leave();
    emit_ret();
  }

  void emit(const ast::stmt& s) {
    assert(s.value);
    std::visit([&](const auto& x) {
      set_location(s.location);
      emit(x);
    }, *s.value);
  }

 private:
  io::location location_;
  std::ostream* output_;
  std::vector<reg32> unused_ = {reg_eax, reg_ebx, reg_ecx,
                                reg_edx, reg_esi, reg_edi};
};

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

export void emit(std::ostream& output, const ast::program& program) {
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
  if (!bss.empty()) output << ".section .bss\n";
  for (const auto& [name, size] : bss) {
    output << name << ": .space " << (int)*size << '\n';
  }
  if (!rodata.empty()) output << ".section .rodata\n";
  for (const auto& [name, value] : rodata) {
    output << name << ": ";
    switch (value->index()) {
      case 0:
        output << ".long " << std::get<0>(*value) << '\n';
        break;
      case 1:
        output << ".ascii " << escaped{std::get<1>(*value)} << '\n';
        break;
    }
  }
  if (!text.empty()) output << ".section .text\n";
  for (const auto& [name, function] : text) {
    output << ".global " << name << '\n';
    output << name << ":\n";
    emitter e{output};
    e.emit_movl(reg_esp, reg_ebp);
    e.emit_subl(immediate{function->frame_size}, reg_esp);
    for (const auto& code : function->code) e.emit(code);
  }
}
