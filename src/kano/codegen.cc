module;

#include <assert.h>

export module kano.codegen;

import kano.ast;
import ir.ast;
import io;
import <iostream>;
import <map>;
import <type_traits>;

namespace kano {

template <typename... Ts> struct overload : Ts... { using Ts::operator()...; };
template <typename... Ts> overload(Ts...) -> overload<Ts...>;

struct void_type {};
struct boolean_type {};
struct integer_type {};
struct pointer_type;
struct function_type;

using type_type = std::variant<
    void_type, boolean_type, integer_type, pointer_type, function_type>;

struct type {
  type() = default;
  type(const type&);
  type(type&& other) noexcept : value(std::move(other.value)) {}
  template <typename T, typename = typename std::enable_if_t<
      !std::is_same_v<type, std::decay_t<T>>>>
  type(T&& value);
  std::unique_ptr<type_type> value;
};

struct pointer_type { type pointee; };

struct function_type {
  type return_type;
  std::vector<type> parameters;
};

type::type(const type& other) {
  if (other.value) value = std::make_unique<type_type>(*other.value);
}

template <typename T, typename>
type::type(T&& value) : value(new type_type{std::forward<T>(value)}) {}

bool operator==(const type& l, const type& r);
bool operator==(void_type, void_type) { return true; }
bool operator==(boolean_type, boolean_type) { return true; }
bool operator==(integer_type, integer_type) { return true; }
bool operator==(const pointer_type& l, const pointer_type& r) {
  return l.pointee == r.pointee;
}
bool operator==(const function_type& l, const function_type& r) {
  return l.return_type == r.return_type && l.parameters == r.parameters;
}
bool operator==(const type& l, const type& r) {
  return (!l.value && !r.value) || *l.value == *r.value;
}

int size(void_type) { return 0; }
int size(boolean_type) { return 1; }
int size(integer_type) { return 4; }
int size(const pointer_type&) { return 4; }
int size(const function_type&) {
  std::cerr << "requesting size of function.";
  std::exit(1);
}
int size(const type& t) {
  assert(t.value);
  return std::visit([](const auto& x) { return size(x); }, *t.value);
}

std::ostream& operator<<(std::ostream& output, const type& t);

std::ostream& operator<<(std::ostream& output, void_type) {
  return output << "void";
}

std::ostream& operator<<(std::ostream& output, boolean_type) {
  return output << "boolean";
}

std::ostream& operator<<(std::ostream& output, integer_type) {
  return output << "integer";
}

std::ostream& operator<<(std::ostream& output, const pointer_type& p) {
  return output << "*" << p.pointee;
}

std::ostream& operator<<(std::ostream& output, const function_type& f) {
  output << '(';
  bool first = true;
  for (const auto& p : f.parameters) {
    if (first) {
      first = false;
    } else {
      output << ", ";
    }
    output << "_ : " << p;
  }
  output << ") : " << f.return_type;
  return output;
}

std::ostream& operator<<(std::ostream& output, const type& t) {
  assert(t.value);
  std::visit([&](const auto& x) { output << x; }, *t.value);
  return output;
}

bool is_function(const type& t) {
  return t.value && std::holds_alternative<function_type>(*t.value);
}

struct symbol { std::string name; };

struct global {
  io::location location;
  symbol symbol;
  type type;
};

struct program_context {
  std::map<std::string_view, int> symbols;
  ir::ast::program output;
  symbol symbol(std::string_view name) {
    int id = symbols[name]++;
    return {std::string(name) + "_" + std::to_string(id)};
  }
  void compile(const ast::module&);
};

struct module_context {
  program_context* program;
  std::string_view filename;
  std::map<std::string_view, global> globals = {};
  auto die(io::location location) const {
    return io::fatal_message(filename, location, io::message::error);
  }
  type check_type(io::location location, kano::ast::types::name name) const {
    // TODO: Add support for custom types.
    if (name.value == "void") return void_type{};
    if (name.value == "boolean") return boolean_type{};
    if (name.value == "integer") return integer_type{};
    die(location) << "no such type '" << name.value << "'.";
  }
  type check_type(io::location, const kano::ast::types::pointer& p) const {
    return pointer_type{check_type(p.pointee)};
  }
  type check_type(const kano::ast::types::type& type) const {
    assert(type.value);
    return std::visit([&](const auto& x) -> struct type {
      return check_type(type.location, x);
    }, *type.value);
  }
  std::string_view get_name(const ast::definition& d) const {
    return std::visit([&](const auto& x) { return x.name; }, d.value);
  }
  type get_type(const ast::variable_declaration& v) const {
    return check_type(v.type);
  }
  type get_type(const ast::function& f) const {
    type return_type = check_type(f.return_type);
    std::vector<type> parameter_types;
    for (const auto& parameter : f.parameters) {
      parameter_types.push_back(get_type(parameter));
    }
    return function_type{std::move(return_type), std::move(parameter_types)};
  }
  type get_type(const ast::definition& d) const {
    return std::visit([&](const auto& x) { return get_type(x); }, d.value);
  }
  ir::ast::zeros compile(io::location l, const ast::variable_declaration&) {
    die(l) << "global variable declarations are unimplemented.";
  }
  ir::ast::function compile(io::location, const ast::function& f);
  ir::ast::definition compile(const ast::definition& d) {
    return std::visit([&](const auto& x) -> ir::ast::definition {
      return {d.location, compile(d.location, x)};
    }, d.value);
  }
  void compile(const ast::module& module) {
    // Populate the map of global variables and function declarations.
    for (const auto& definition : module.definitions) {
      std::string_view name = get_name(definition);
      type type = get_type(definition);
      auto [i, is_new] = globals.emplace(
          name,
          global{definition.location, program->symbol(name), std::move(type)});
      if (!is_new) {
        io::message(filename, definition.location, io::message::error)
            << "redefinition of '" << name << "'.";
        io::message(filename, definition.location, io::message::note)
            << "previous definition is here.";
        std::exit(1);
      }
    }
    // Compile each definition.
    for (const auto& definition : module.definitions) {
      const auto s = globals.at(get_name(definition)).symbol;
      program->output.emplace(s.name, compile(definition));
    }
    // Emit the startup code.
    auto main = globals.find("main");
    if (main == globals.end()) die({}) << "no main function.";
    const type main_type = function_type{integer_type{}, {}};
    if (main->second.type != main_type) {
      die(main->second.location) << "main should be of type 'integer()'.";
    }
    std::vector<ir::ast::expr> exit_args;
    exit_args.push_back(
        ir::ast::callw{ir::ast::global{main->second.symbol.name}, {}});
    std::vector<ir::ast::stmt> code;
    code.push_back(
        {{}, ir::ast::call{{ir::ast::builtin::exit, std::move(exit_args)}}});
    program->output.emplace(
        "_start",
        ir::ast::definition{{}, ir::ast::function{0, std::move(code)}});
  }
};

void program_context::compile(const ast::module& module) {
  module_context{this, module.filename}.compile(module);
}

struct local {
  io::location location;
  int offset;
  type type;
};

struct scope {
  std::map<std::string_view, local> variables;
  std::optional<symbol> break_label, continue_label;
  int frame_size = 0;
};

struct function_context {
  function_context(module_context* module, io::location location,
                   const ast::function& f)
      : module(module),
        function(&f),
        location(location),
        return_type(module->check_type(f.return_type)) {}
  module_context* module;
  const ast::function* function;
  io::location location;
  type return_type;
  std::vector<scope> locals;
  int frame_size = 0;
  std::vector<ir::ast::stmt> code;
  const symbol& break_label(io::location l) {
    for (int i = locals.size() - 1; i >= 0; i--) {
      if (locals[i].break_label) return *locals[i].break_label;
    }
    module->die(l) << "cannot break in this context.";
  }
  const symbol& continue_label(io::location l) {
    for (int i = locals.size() - 1; i >= 0; i--) {
      if (locals[i].continue_label) return *locals[i].continue_label;
    }
    module->die(l) << "cannot continue in this context.";
  }
  local* lookup_local(std::string_view name) {
    for (int i = locals.size() - 1; i >= 0; i--) {
      auto entry = locals[i].variables.find(name);
      if (entry != locals[i].variables.end()) return &entry->second;
    }
    return nullptr;
  }
  global* lookup_global(std::string_view name) {
    auto entry = module->globals.find(name);
    if (entry != module->globals.end()) return &entry->second;
    return nullptr;
  }
  std::variant<local*, global*> lookup(io::location l, std::string_view name) {
    if (auto* local = lookup_local(name)) return local;
    if (auto* global = lookup_global(name)) return global;
    module->die(l) << "no such name '" << name << "'.";
  }
  using typed_expr = std::pair<ir::ast::expr, type>;
  typed_expr gen_expr(io::location, ast::literal x) {
    return {x, integer_type{}};
  }
  typed_expr gen_expr(io::location l, ast::name n) {
    if (auto* local = lookup_local(n.value)) {
      return {ir::ast::loadw{ir::ast::local{local->offset}}, local->type};
    }
    if (auto* global = lookup_global(n.value)) {
      if (is_function(global->type)) {
        return {ir::ast::global{global->symbol.name}, global->type};
      } else {
        return {ir::ast::loadw{ir::ast::global{global->symbol.name}},
                global->type};
      }
    }
    module->die(l) << "no such name '" << n.value << "'.";
  }
  // TODO: Add type checking in expressions.
  typed_expr gen_expr(
      io::location l, const ast::dereference& d) {
    auto [expr, inner_type] = gen_expr(d.inner);
    if (auto* type = std::get_if<pointer_type>(inner_type.value.get())) {
      return {ir::ast::loadw{std::move(expr)}, std::move(type->pointee)};
    } else {
      module->die(l) << "cannot dereference expression of type " << inner_type
                     << ".";
    }
  }
  typed_expr gen_expr(io::location, const ast::address_of& a) {
    return gen_addr(a.inner);
  }
  typed_expr gen_expr(io::location location, const ast::add& a) {
    auto l = gen_expr(a.left);
    auto r = gen_expr(a.right);
    return std::visit(overload{
      [&](integer_type, integer_type) -> typed_expr {
        return {ir::ast::add{std::move(l.first), std::move(r.first)},
                integer_type{}};
      },
      [&](pointer_type& p, integer_type) -> typed_expr {
        auto offset = ir::ast::mul{std::move(r.first), size(p.pointee)};
        return {ir::ast::add{std::move(l.first), std::move(offset)},
                std::move(p)};
      },
      [&](integer_type, pointer_type& p) -> typed_expr {
        auto offset = ir::ast::mul{std::move(l.first), size(p.pointee)};
        return {ir::ast::add{std::move(offset), std::move(r.first)},
                std::move(p)};
      },
      [&](const auto& lt, const auto& rt) -> typed_expr {
        module->die(location) << "cannot add " << lt << " and " << rt << ".";
      },
    }, *l.second.value, *r.second.value);
  }
  typed_expr gen_expr(io::location location, const ast::sub& s) {
    auto l = gen_expr(s.left);
    auto r = gen_expr(s.right);
    return std::visit(overload{
      [&](integer_type, integer_type) -> typed_expr {
        return {ir::ast::sub{std::move(l.first), std::move(r.first)},
                integer_type{}};
      },
      [&](pointer_type& p, integer_type) -> typed_expr {
        auto offset = ir::ast::mul{std::move(r.first), size(p.pointee)};
        return {ir::ast::sub{std::move(l.first), std::move(offset)},
                std::move(p)};
      },
      [&](const auto& lt, const auto& rt) -> typed_expr {
        module->die(location) << "cannot sub " << lt << " and " << rt << ".";
      },
    }, *l.second.value, *r.second.value);
  }
  typed_expr gen_expr(io::location location, const ast::cmp_eq& c) {
    auto l = gen_expr(c.left);
    auto r = gen_expr(c.right);
    if (l.second != r.second) {
      module->die(location) << "cannot compare " << l.second << " against "
                            << r.second << ".";
    }
    return {ir::ast::cmp_eq{std::move(l.first), std::move(r.first)},
            boolean_type{}};
  }
  typed_expr gen_expr(io::location location, const ast::cmp_lt& c) {
    auto l = gen_expr(c.left);
    auto r = gen_expr(c.right);
    if (l.second != r.second) {
      module->die(location) << "cannot compare " << l.second << " against "
                            << r.second << ".";
    }
    std::visit(overload{
      [&](integer_type) {},
      [&](pointer_type&) {},
      [&](const auto&) {
        module->die(location) << l.second << " is not an ordered type.";
      },
    }, *l.second.value);
    return {ir::ast::cmp_lt{std::move(l.first), std::move(r.first)},
            boolean_type{}};
  }
  typed_expr gen_expr(io::location l, const ast::call& c) {
    std::vector<ir::ast::expr> arguments;
    std::vector<type> argument_types;
    auto callee = gen_expr(c.callee);
    for (const auto& x : c.arguments) {
      auto [expr, type] = gen_expr(x);
      arguments.push_back(std::move(expr));
      argument_types.push_back(std::move(type));
    }
    if (auto* f = std::get_if<function_type>(callee.second.value.get())) {
      if (f->parameters.size() != arguments.size()) {
        module->die(l) << "number of parameters does not match: expected "
                       << f->parameters.size() << ", but got "
                       << arguments.size() << ".";
      }
      for (int i = 0, n = arguments.size(); i < n; i++) {
        if (f->parameters[i] != argument_types[i]) {
          module->die(l) << "parameter " << (i + 1)
                         << " for function call is of the wrong type: expected "
                         << f->parameters[i] << " but got "
                         << argument_types[i] << ".";
        }
      }
      return {ir::ast::callw{std::move(callee.first), std::move(arguments)},
              std::move(f->return_type)};
    } else {
      module->die(l) << "cannot invoke " << callee.second << ".";
    }
  }
  typed_expr gen_expr(const ast::expression& e) {
    assert(e.value);
    return std::visit([&](const auto& x) { return gen_expr(e.location, x); },
                      *e.value);
  }
  typed_expr gen_addr(io::location l, ast::name n) {
    if (auto* local = lookup_local(n.value)) {
      return {ir::ast::local{local->offset}, pointer_type{local->type}};
    }
    if (auto* global = lookup_global(n.value)) {
      return {ir::ast::global{global->symbol.name}, pointer_type{global->type}};
    }
    module->die(l) << "no such name '" << n.value << "'.";
  }
  typed_expr gen_addr(const ast::expression& e) {
    if (auto* name = std::get_if<ast::name>(e.value.get())) {
      return gen_addr(e.location, *name);
    } else {
      // TODO: Improve the error message when this branch is reached on the left
      // hand side of an assignment statement.
      module->die(e.location) << "cannot take address of temporary.";
    }
  }
  void compile_assignment(const ast::expression& destination,
                          const ast::expression& value) {
    auto addr = gen_addr(destination);
    assert(std::holds_alternative<pointer_type>(*addr.second.value));
    auto pointee_type =
        std::move(std::get<pointer_type>(*addr.second.value).pointee);
    auto expr = gen_expr(value);
    if (expr.second != pointee_type) {
      module->die(destination.location)
          << "cannot assign value of type " << expr.second
          << " to destination of type " << pointee_type << ".";
    }
    code.push_back({destination.location,
                    ir::ast::storew{std::move(addr.first),
                                    std::move(expr.first)}});
  }
  void compile(io::location l, const ast::variable_declaration& v) {
    if (auto* local = lookup_local(v.name)) {
      io::message(module->filename, l, io::message::error)
          << "redefinition of '" << v.name << "'.";
      io::message(module->filename, l, io::message::note)
          << "previous definition is here.";
      std::exit(1);
    }
    scope& scope = locals.back();
    // TODO: Add support for other types.
    scope.frame_size += 4;
    if (scope.frame_size > frame_size) frame_size = scope.frame_size;
    scope.variables.emplace(
        v.name, local{l, -scope.frame_size, module->check_type(v.type)});
    if (v.initializer) {
      compile_assignment({l, ast::name{v.name}}, *v.initializer);
    }
  }
  void compile(io::location, const ast::assignment& a) {
    return compile_assignment(a.destination, a.value);
  }
  void compile(io::location l, const ast::if_statement& s) {
    const auto if_end_label = module->program->symbol("if_end");
    auto [condition, type] = gen_expr(s.condition);
    if (type != boolean_type{}) {
      module->die(l) << "invalid type for branch condition: expected "
                     << boolean_type{} << ", got " << type << ".";
    }
    if (s.else_branch) {
      const auto else_label = module->program->symbol("else");
      code.push_back({l, ir::ast::jz{std::move(condition), else_label.name}});
      compile(s.then_branch);
      code.push_back(
          {s.else_branch->location, ir::ast::jump{if_end_label.name}});
      code.push_back(
          {s.else_branch->location, ir::ast::label{else_label.name}});
      compile(*s.else_branch);
      code.push_back({l, ir::ast::label{if_end_label.name}});
    } else {
      code.push_back(
          {l, ir::ast::jz{std::move(condition), if_end_label.name}});
      compile(s.then_branch);
      code.push_back({l, ir::ast::label{if_end_label.name}});
    }
  }
  void compile(io::location l, const ast::while_statement& s) {
    const auto while_start_label = module->program->symbol("while_start");
    const auto while_condition_label =
        module->program->symbol("while_condition");
    const auto while_end_label = module->program->symbol("while_end");
    code.push_back({l, ir::ast::jump{while_condition_label.name}});
    code.push_back({l, ir::ast::label{while_start_label.name}});
    locals.push_back({});
    locals.back().continue_label = while_condition_label;
    locals.back().break_label = while_end_label;
    compile(s.body);
    code.push_back({l, ir::ast::label{while_condition_label.name}});
    auto [condition, type] = gen_expr(s.condition);
    if (type != boolean_type{}) {
      module->die(l) << "invalid type for branch condition: expected "
                     << boolean_type{} << ", got " << type << ".";
    }
    code.push_back(
        {l, ir::ast::jnz{std::move(condition), while_start_label.name}});
    code.push_back({l, ir::ast::label{while_end_label.name}});
  }
  void compile(io::location l, const ast::break_statement&) {
    code.push_back({l, ir::ast::jump{break_label(l).name}});
  }
  void compile(io::location l, const ast::continue_statement&) {
    code.push_back({l, ir::ast::jump{continue_label(l).name}});
  }
  void compile(io::location l, const ast::return_statement& s) {
    if (s.value) {
      auto [result, type] = gen_expr(*s.value);
      if (type != return_type) {
        module->die(l) << "return type mismatch: expected " << return_type
                       << ", got " << type << ".";
      }
      code.push_back({l, ir::ast::retw{std::move(result)}});
    } else {
      code.push_back({l, ir::ast::ret{}});
    }
  }
  void compile(io::location, const std::vector<ast::statement>& block) {
    locals.push_back({});
    for (const auto& s : block) compile(s);
    locals.pop_back();
  }
  void compile(const ast::statement& s) {
    assert(s.value);
    std::visit([&](const auto& x) { compile(s.location, x); }, *s.value);
  }
  ir::ast::function compile() {
    int offset = 8;  // Past the stored stack frame and return address.
    locals.push_back({});
    for (const auto& p : function->parameters) {
      assert(!p.initializer);
      locals[0].variables.emplace(
          p.name, local{location, offset, module->check_type(p.type)});
      // TODO: Add support for other types.
      offset += 4;
    }
    for (const auto& s : function->body) compile(s);
    return {frame_size, std::move(code)};
  }
};

ir::ast::function module_context::compile(io::location location,
                                          const ast::function& f) {
  return function_context{this, location, f}.compile();
}

export ir::ast::program compile(const kano::ast::module& module) {
  program_context context;
  context.compile(module);
  return std::move(context.output);
}

}  // namespace kano
