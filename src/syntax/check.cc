// Check Kano source code, producing Kano IR.

module;

#include <cassert>

export module syntax.check;

import overload;
import syntax.parse;
export import semantics.ir;
import <iostream>;
import <filesystem>;

namespace syntax {

struct environment;

struct module_type {
  const environment* exports;
};

// Used for names which represent types instead of names which represent values
// of a given type.
struct type_type {
  semantics::ir::data_type type;
};

using name_type =
    std::variant<semantics::ir::data_type, semantics::ir::function_type,
                 module_type, type_type>;

struct definition {
  // TODO: Add guts.
};

struct module_checker;

// An environment keeps track of what names are currently in scope, and what
// symbols they map to.
struct environment {
  environment* parent = nullptr;
  struct name_info {
    std::string file;
    io::location location;
    semantics::ir::symbol symbol;
    std::string name;
    name_type type;
  };
  std::map<std::string, name_info, std::less<>> names = {};

  // Resolve a name within the environment, searching upwards through the
  // lexical scope for its definition.
  const name_info& lookup(module_checker& program, io::location location,
                          std::string_view name) const;

  // Define a name within the current scope.
  const name_info& define(module_checker& program, io::location location,
                          std::string name, name_type type,
                          semantics::ir::symbol symbol);
  const name_info& define(module_checker& program, io::location location,
                          std::string name, name_type type);
};

struct module_data {
  bool checked = false;
  std::filesystem::path path;
  environment exports;
};

struct checker {
  semantics::ir::symbol next = {};
  std::map<std::filesystem::path, module_data> modules;
  environment builtins = {
      .names = {
          {"bool",
           {"builtin",
            {},
            symbol(),
            "bool",
            type_type{{{}, semantics::ir::builtin_type::bool_type}}}},
          {"int32",
           {"builtin",
            {},
            symbol(),
            "int32",
            type_type{{{}, semantics::ir::builtin_type::int32_type}}}},
          {"void",
           {"builtin",
            {},
            symbol(),
            "void",
            type_type{{{}, semantics::ir::builtin_type::void_type}}}},
      },
  };

  // Generate a new unique symbol for some exported artefact.
  semantics::ir::symbol symbol();

  // Fetches data about a module. If the module is not yet processed, open the
  // file and process it. If it is already processed, return the existing data.
  const module_data& get_module(const std::filesystem::path& path);
};

struct module_checker {
  checker& program;
  const std::filesystem::path& path;
  module_data& module;
  environment environment = {&program.builtins};

  // Returns a visually pleasing version of the filename for use in messages.
  std::string name() const;

  // Check the module.
  void check();

  void check(io::location, const ast::import_statement&);

  // TODO: Split checking into different stages and sequence the checking based
  // on dependencies between symbols. This way, we can remove the requirement
  // for things to be declared further up in the file than where they are used.
  const environment::name_info& check(io::location,
                                      const ast::variable_definition&);
  const environment::name_info& check(io::location,
                                      const ast::alias_definition&);
  const environment::name_info& check(io::location,
                                      const ast::function_definition&);
  const environment::name_info& check(io::location,
                                      const ast::class_definition&);
  const environment::name_info& check(const ast::definition&);
  const environment::name_info& check(const ast::exported_definition&);

  semantics::ir::data_type check_type(const ast::expression&);

  const environment::name_info& resolve(const ast::expression&);
};

const environment::name_info& environment::lookup(module_checker& module,
                                                  io::location location,
                                                  std::string_view name) const {
  auto i = names.find(name);
  if (i != names.end()) return i->second;
  if (parent) return parent->lookup(module, location, name);
  io::fatal_message{module.name(), location, io::message::error}
      << "undefined name " << std::quoted(name) << ".";
}

const environment::name_info& environment::define(
    module_checker& module, io::location l, std::string id, name_type type,
    semantics::ir::symbol symbol) {
  auto [i, is_new] = names.emplace(
      id, name_info{module.name(), l, symbol, id, std::move(type)});
  if (!is_new) {
    const auto file = module.name();
    io::message{file, l, io::message::error}
        << "redeclaration of variable " << std::quoted(id) << ".";
    io::fatal_message{file, i->second.location, io::message::note}
        << "previously declared here.";
  }
  return i->second;
}

const environment::name_info& environment::define(
    module_checker& module, io::location l, std::string id, name_type type) {
  return define(module, l, std::move(id), std::move(type),
                module.program.symbol());
}

semantics::ir::symbol checker::symbol() {
  const auto result = next;
  next = semantics::ir::symbol((int)next + 1);
  return result;
}

const module_data& checker::get_module(const std::filesystem::path& path) {
  const auto name = std::filesystem::relative(path).native();
  std::cerr << name << "...\n";
  auto [i, is_new] = modules.emplace(path, module_data{});
  if (!is_new) return i->second;
  module_checker{*this, i->first, i->second}.check();
  return i->second;
}

std::string module_checker::name() const {
  return std::filesystem::relative(path).native();
}

void module_checker::check() {
  assert(!module.checked);
  const ast::module source = parse(path.c_str());
  auto i = source.statements.begin();
  const auto end = source.statements.end();
  // Handle import statements.
  while (i != end && i->is<ast::import_statement>()) {
    check(i->location(), *i->get<ast::import_statement>());
    ++i;
  }
  if (i == end) return;
  const auto prelude_end = i->location();
  // Collect declarations for all variables.
  while (i != end) {
    if (i->is<ast::import_statement>()) {
      const auto f = name();
      io::message{f, i->location(), io::message::error}
          << "cannot have an import here.";
      io::fatal_message{f, prelude_end, io::message::note}
          << "module prelude ended here.";
    }
    if (auto* d = i->get<ast::definition>()) {
      check(*d);
    } else if (auto* e = i->get<ast::exported_definition>()) {
      check(*e);
    } else {
      io::fatal_message{name(), i->location(), io::message::error}
          << "only definitions may appear at the top-level.";
    }
    ++i;
  }
  module.checked = true;
}

void module_checker::check(io::location l, const ast::import_statement& i) {
  std::filesystem::path module_path = path.parent_path();
  for (auto& x : i.path) module_path /= x;
  module_path += ".kano";
  const auto& m = program.get_module(module_path);
  if (!m.checked) {
    // TODO: Improve the error message for this case. It should be fairly easy
    // to display the cycle.
    io::fatal_message{name(), l, io::message::error} << "cyclic import.";
  }
  environment.define(*this, l, i.path.back(), module_type{&m.exports});
}

const environment::name_info& module_checker::check(
    io::location l, const ast::variable_definition& v) {
  // TODO: Check the initializer.
  return environment.define(*this, l, v.id.value, check_type(v.type));
}

const environment::name_info& module_checker::check(io::location l,
                                            const ast::alias_definition& a) {
  return environment.define(*this, l, a.id.value,
                            type_type{check_type(a.type)});
}

const environment::name_info& module_checker::check(
    io::location l, const ast::function_definition& f) {
  auto return_type = check_type(f.return_type);
  std::vector<semantics::ir::data_type> parameters;
  for (const auto& parameter : f.parameters) {
    // TODO: Check that parameter names are not duplicated.
    parameters.push_back(check_type(parameter.type));
  }
  // TODO: Check the function body.
  return environment.define(*this, l, f.id.value,
                            semantics::ir::function_type{
                                std::move(return_type), std::move(parameters)});
}

const environment::name_info& module_checker::check(
    io::location l, const ast::class_definition& c) {
  // TODO: Put the body of the class aside for subsequent checking after all
  // top-level declarations have been handled.
  const auto symbol = program.symbol();
  return environment.define(
      *this, l, c.id.value,
      type_type{{l, semantics::ir::user_defined_type{symbol}}}, symbol);
}

const environment::name_info& module_checker::check(const ast::definition& e) {
  return e.visit([&](const auto& x) -> auto& {
    return check(e.location(), x);
  });
}

const environment::name_info& module_checker::check(
    const ast::exported_definition& e) {
  const auto& info = check(e.value);
  module.exports.names.emplace(info.name, info);
  return info;
}

semantics::ir::data_type module_checker::check_type(
    const ast::expression& e) {
  if (const auto* i = e.get<ast::identifier>()) {
    const auto& info = environment.lookup(*this, e.location(), i->value);
    if (const auto* type = std::get_if<type_type>(&info.type)) {
      return type->type;
    } else {
      const auto file = name();
      io::message{file, e.location(), io::message::error}
          << std::quoted(i->value) << " does not represent a type.";
      io::fatal_message{info.file, info.location, io::message::note}
          << "defined here.";
    }
  }
  if (const auto* a = e.get<ast::array_type>()) {
    // TODO: Support nontrivial size expressions.
    const auto* i = a->size.get<ast::literal_integer>();
    if (!i) {
      io::fatal_message{name(), a->size.location(), io::message::error}
          << "support for nontrivial size expressions is unimplemented.";
    }
    auto element = check_type(a->element);
    return {e.location(),
            semantics::ir::array_type{i->value, std::move(element)}};
  }
  if (const auto* d = e.get<ast::dereference>()) {
    // TODO: Add support for function pointer types once they have syntax.
    auto pointee = check_type(d->from);
    return {e.location(), semantics::ir::pointer_type{std::move(pointee)}};
  }
  if (const auto* d = e.get<ast::dot>()) {
    const auto& m = resolve(e);
    if (auto* type = std::get_if<type_type>(&m.type)) {
      return type->type;
    } else {
      // TODO: Improve this error message to show what it actually is.
      io::fatal_message{name(), e.location(), io::message::error}
          << "expected type.";
    }
  }
  io::fatal_message{name(), e.location(), io::message::error}
      << "unsupported type expression.";
}

const environment::name_info& module_checker::resolve(
    const ast::expression& e) {
  if (const auto* i = e.get<ast::identifier>()) {
    return environment.lookup(*this, e.location(), i->value);
  }
  if (const auto* d = e.get<ast::dot>()) {
    const auto& lhs = resolve(d->from);
    if (const auto* m = std::get_if<module_type>(&lhs.type)) {
      // TODO: Improve the error message for unknown names here. It might be
      // nice to point the user at the definition for the LHS in this case.
      return m->exports->lookup(*this, e.location(), d->id.value);
    }
    io::fatal_message{name(), e.location(), io::message::error}
        << "expected module on left hand side of '.'.";
  }
  // TODO: Implement support for nested types.
  io::fatal_message{name(), e.location(), io::message::error}
      << "unsupported scope expression.";
}

export void check(const char* filename) {
  checker{}.get_module(std::filesystem::absolute(filename));
}

}  // namespace syntax
