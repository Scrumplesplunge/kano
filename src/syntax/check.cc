// Check Kano source code, producing Kano IR.

module;

#include <cassert>

export module syntax.check;

import syntax.parse;
export import semantics.ir;
import <iostream>;
import <filesystem>;

namespace syntax {

struct definition {
  // TODO: Add guts.
};

struct module_interface {
  std::map<semantics::ir::symbol, semantics::ir::type> exports;
};

struct module_implementation {
  std::map<semantics::ir::symbol, semantics::ir::type> declarations;
  std::map<semantics::ir::symbol, definition> definitions;
};

struct module_data {
  bool checked = false;
  std::filesystem::path path;
  module_interface interface;
  module_implementation implementation;
};

struct checker {
  semantics::ir::symbol next = {};
  std::map<std::filesystem::path, module_data> modules;

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
  std::map<std::string, io::location> reserved = {};

  // Returns a visually pleasing version of the filename for use in messages.
  std::string name() const;

  // Check the module.
  void check();

  // Reserve an identifier at the top level of this module.
  void reserve(io::location, const std::string&);

  semantics::ir::symbol declare(io::location, const ast::variable_definition&);
  semantics::ir::symbol declare(io::location, const ast::alias_definition&);
  semantics::ir::symbol declare(io::location, const ast::function_definition&);
  semantics::ir::symbol declare(io::location, const ast::class_definition&);
  semantics::ir::symbol declare(const ast::definition&);
  semantics::ir::symbol declare(const ast::exported_definition&);
};

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
  std::map<std::string, const module_data*> imports;
  auto i = source.statements.begin();
  const auto end = source.statements.end();
  // Handle import statements.
  while (i != end && i->is<ast::import_statement>()) {
    const auto& import = *i->get<ast::import_statement>();
    std::filesystem::path module_path = path.parent_path();
    for (auto& x : import.path) module_path /= x;
    module_path += ".kano";
    const auto& m = program.get_module(module_path);
    if (!m.checked) {
      // TODO: Improve the error message for this case. It should be fairly easy
      // to display the cycle.
      io::fatal_message{name(), i->location(), io::message::error}
          << "cyclic import.";
    }
    imports.emplace(import.path.back(), &m);
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
      declare(*d);
    } else if (auto* e = i->get<ast::exported_definition>()) {
      declare(*e);
    } else {
      io::fatal_message{name(), i->location(), io::message::error}
          << "only definitions may appear at the top-level.";
    }
    ++i;
  }
  module.checked = true;
}

void module_checker::reserve(io::location l, const std::string& id) {
  auto [i, is_new] = reserved.emplace(id, l);
  if (!is_new) {
    io::message{name(), l, io::message::error}
        << "redeclaration of variable " << std::quoted(id) << ".";
    io::fatal_message{name(), i->second, io::message::note}
        << "previously declared here.";
  }
}

semantics::ir::symbol module_checker::declare(
    io::location l, const ast::variable_definition& v) {
  reserve(l, v.id.value);
  // TODO: Check the type of the variable and create a declaration.
  return program.symbol();
}

semantics::ir::symbol module_checker::declare(io::location l,
                                              const ast::alias_definition& a) {
  reserve(l, a.id.value);
  const auto id = program.symbol();
  module.implementation.declarations.emplace(
      id, semantics::ir::data_type{l, semantics::ir::user_defined_type{id}});
  return id;
}

semantics::ir::symbol module_checker::declare(
    io::location l, const ast::function_definition& f) {
  reserve(l, f.id.value);
  // TODO: Check the return type and all of the parameter types, and use them to
  // define the function type.
  return program.symbol();
}

semantics::ir::symbol module_checker::declare(io::location l,
                                              const ast::class_definition& c) {
  reserve(l, c.id.value);
  // TODO: Put the body of the class aside for subsequent checking after all
  // top-level declarations have been handled.
  const auto id = program.symbol();
  module.implementation.declarations.emplace(
      id, semantics::ir::data_type{l, semantics::ir::user_defined_type{id}});
  return id;
}

semantics::ir::symbol module_checker::declare(
    const ast::definition& e) {
  return e.visit([&](const auto& x) { return declare(e.location(), x); });
}

semantics::ir::symbol module_checker::declare(
    const ast::exported_definition& e) {
  const auto id = declare(e.value);
  module.interface.exports.emplace(id,
                                   module.implementation.declarations.at(id));
  return id;
}

export void check(const char* filename) {
  checker{}.get_module(std::filesystem::absolute(filename));
}

}  // namespace syntax
