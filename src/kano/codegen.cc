module;

#include <assert.h>

export module kano.codegen;

import kano.ast;
import ir.ast;
import io;
import <map>;

namespace kano {

enum class builtin_type {
  void_type,
  boolean,
  int32,
};

struct pointer_type;
struct function_type;

using type_type = std::variant<builtin_type, pointer_type, function_type>;

struct type {
  template <typename T> type(T&& value);
  std::unique_ptr<type_type> value;
};

bool operator==(const type& l, const type& r);

struct pointer_type { type pointee; };

bool operator==(const pointer_type& l, const pointer_type& r) {
  return l.pointee == r.pointee;
}

struct function_type {
  type return_type;
  std::vector<type> parameters;
};

bool operator==(const function_type& l, const function_type& r) {
  return l.return_type == r.return_type && l.parameters == r.parameters;
}

template <typename T>
type::type(T&& value) : value(new type_type{std::forward<T>(value)}) {}

bool operator==(const type& l, const type& r) {
  return (!l.value && !r.value) || *l.value == *r.value;
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
    if (name.value == "void") return builtin_type::void_type;
    if (name.value == "boolean") return builtin_type::boolean;
    if (name.value == "integer") return builtin_type::int32;
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
    const type main_type = function_type{builtin_type::int32, {}};
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
  ir::ast::expr gen_expr(io::location, ast::literal x) {
    return std::visit([](auto x) { return (std::int32_t)x; }, x);
  }
  ir::ast::expr gen_expr(io::location l, ast::name n) {
    if (auto* local = lookup_local(n.value)) {
      return ir::ast::loadw{ir::ast::local{local->offset}};
    }
    if (auto* global = lookup_global(n.value)) {
      if (is_function(global->type)) {
        return ir::ast::global{global->symbol.name};
      } else {
        return ir::ast::loadw{ir::ast::global{global->symbol.name}};
      }
    }
    module->die(l) << "no such name '" << n.value << "'.";
    return ir::ast::loadw{gen_addr(l, n)};
  }
  // TODO: Add type checking in expressions.
  ir::ast::expr gen_expr(io::location, const ast::dereference& d) {
    return ir::ast::loadw{gen_expr(d.inner)};
  }
  ir::ast::expr gen_expr(io::location, const ast::address_of& a) {
    return gen_addr(a.inner);
  }
  ir::ast::expr gen_expr(io::location, const ast::add& a) {
    return ir::ast::add{gen_expr(a.left), gen_expr(a.right)};
  }
  ir::ast::expr gen_expr(io::location, const ast::sub& s) {
    return ir::ast::sub{gen_expr(s.left), gen_expr(s.right)};
  }
  ir::ast::expr gen_expr(io::location, const ast::call& c) {
    std::vector<ir::ast::expr> arguments;
    auto callee = gen_expr(c.callee);
    for (const auto& x : c.arguments) arguments.push_back(gen_expr(x));
    return ir::ast::callw{std::move(callee), std::move(arguments)};
  }
  ir::ast::expr gen_expr(const ast::expression& e) {
    assert(e.value);
    return std::visit([&](const auto& x) { return gen_expr(e.location, x); },
                      *e.value);
  }
  ir::ast::expr gen_addr(io::location l, ast::name n) {
    if (auto* local = lookup_local(n.value)) {
      return ir::ast::local{local->offset};
    }
    if (auto* global = lookup_global(n.value)) {
      return ir::ast::global{global->symbol.name};
    }
    module->die(l) << "no such name '" << n.value << "'.";
  }
  ir::ast::expr gen_addr(const ast::expression& e) {
    if (auto* name = std::get_if<ast::name>(e.value.get())) {
      return gen_addr(e.location, *name);
    } else {
      module->die(e.location) << "cannot take address of temporary.";
    }
  }
  void compile_assignment(const ast::expression& destination,
                          const ast::expression& value) {
    code.push_back({destination.location,
                    ir::ast::storew{gen_addr(destination), gen_expr(value)}});
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
    if (s.else_branch) {
      const auto else_label = module->program->symbol("else");
      code.push_back({l, ir::ast::jz{gen_expr(s.condition), else_label.name}});
      compile(s.then_branch);
      code.push_back(
          {s.else_branch->location, ir::ast::jump{if_end_label.name}});
      code.push_back(
          {s.else_branch->location, ir::ast::label{else_label.name}});
      compile(*s.else_branch);
      code.push_back({l, ir::ast::label{if_end_label.name}});
    } else {
      code.push_back(
          {l, ir::ast::jz{gen_expr(s.condition), if_end_label.name}});
      compile(s.then_branch);
      code.push_back({l, ir::ast::label{if_end_label.name}});
    }
  }
  void compile(io::location l, const ast::while_statement& s) {
    const auto while_start_label = module->program->symbol("while_start");
    const auto while_condition_label =
        module->program->symbol("while_condition");
    code.push_back({l, ir::ast::jump{while_condition_label.name}});
    code.push_back({l, ir::ast::label{while_start_label.name}});
    compile(s.body);
    code.push_back({l, ir::ast::label{while_condition_label.name}});
    code.push_back(
        {l, ir::ast::jnz{gen_expr(s.condition), while_start_label.name}});
  }
  void compile(io::location l, const ast::break_statement&) {
    module->die(l) << "break statements are unimplemented.";
  }
  void compile(io::location l, const ast::continue_statement&) {
    module->die(l) << "continue statements are unimplemented.";
  }
  void compile(io::location l, const ast::return_statement& s) {
    // TODO: Type-check the return type.
    if (s.value) {
      code.push_back({l, ir::ast::retw{gen_expr(*s.value)}});
    } else {
      code.push_back({l, ir::ast::ret{}});
    }
  }
  void compile(io::location, const std::vector<ast::statement>& block) {
    for (const auto& s : block) compile(s);
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
