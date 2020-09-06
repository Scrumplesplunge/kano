// Check Kano source code, producing Kano IR.

module;

#include <cassert>

export module syntax.check;

import syntax.parse;
export import semantics.ir;
import <iostream>;
import <filesystem>;

namespace syntax {

struct environment;

struct module_type {
  const environment* exports;
};

struct global {
  semantics::ir::data_type type;
};

struct local {
  semantics::ir::data_type type;
};

// Used for names which represent types instead of names which represent values
// of a given type.
struct type_type {
  semantics::ir::data_type type;
};

using name_type = std::variant<semantics::ir::function_type, module_type,
                               global, local, type_type>;

struct checker;
struct module_checker;

// An environment keeps track of what names are currently in scope, and what
// symbols they map to.
struct environment {
  environment* parent = nullptr;
  struct name_info {
    io::location location;
    semantics::ir::symbol symbol;
    std::string name;
    name_type type;
  };
  std::map<std::string, name_info, std::less<>> names = {};

  // Resolve a name within the environment, searching upwards through the
  // lexical scope for its definition.
  const name_info& lookup(io::location location, std::string_view name) const;

  // Define a name within the current scope.
  const name_info& define(io::location location, std::string name,
                          name_type type, semantics::ir::symbol symbol);

  semantics::ir::data_type check_type(const ast::expression&) const;
  const environment::name_info& resolve(const ast::expression&) const;
};

struct expression_checker {
  checker& program;
  module_checker& module;
  const environment& environment;
  semantics::ir::expression result = {};

  using local_info =
      std::pair<const semantics::ir::local, semantics::ir::data_type>;

  struct info {
    // Every value has a category which describes how that value can be used.
    // This is distinct from the notion of a type, as the category is always
    // implicit and never composes.
    //
    // An lvalue is a named quantity. The name can be read as `left value`, as
    // if to say that an lvalue may appear on the left side of an assignment.
    // However, this does not mean that all lvalues can be assigned to: an
    // lvalue may still be of an unassignable type, such as a function. These
    // are always represented in the IR as a pointer to a memory location.
    //
    // An rvalue is a pure value. For example, a literal integer is an rvalue.
    // These are represented as inline values, so they can only be
    // register-sized.
    //
    // An xvalue is an expiring lvalue. That is, it's also represented as
    // a pointer to a memory location, but unlike an lvalue it is to be
    // considered movable.
    enum { lvalue, rvalue, xvalue } category;
    const local_info* result;
  };

  const semantics::ir::data_type& effective_type(const info&);

  const local_info& add(semantics::ir::data_type, semantics::ir::action);
  const local_info& add(semantics::ir::value);
  const local_info& alloc(semantics::ir::data_type);
  // Given a pointer to an indexable object (i.e. [n]T) and an index i, produce
  // a pointer to the ith element of the object.
  const local_info& index(io::location, const local_info&, const local_info&);
  const local_info& ensure_loaded(const info&);
  const local_info& load(const local_info&);
  void label(semantics::ir::symbol);
  void conditional_jump(io::location, const local_info&, semantics::ir::symbol);
  void construct_into(const local_info&, const info&);

  info generate(io::location, const environment::name_info&);
  info generate(io::location, const ast::identifier&);
  info generate(io::location, const ast::literal_integer&);
  info generate(io::location, const ast::literal_string&);
  info generate(io::location, const ast::literal_aggregate&,
                const semantics::ir::array_type&);
  info generate(io::location, const ast::literal_aggregate&);
  info generate(io::location, const ast::array_type&);
  info generate(io::location, const ast::dot&);
  info generate(io::location, const ast::dereference&);
  info generate(io::location, const ast::address_of&);
  info generate(io::location, const ast::index&);
  info generate(io::location, const ast::negate&);
  info generate(io::location, const ast::add&);
  info generate(io::location, const ast::subtract&);
  info generate(io::location, const ast::multiply&);
  info generate(io::location, const ast::divide&);
  info generate(io::location, const ast::modulo&);
  info generate(io::location, const ast::compare_eq&);
  info generate(io::location, const ast::compare_ne&);
  info generate(io::location, const ast::compare_lt&);
  info generate(io::location, const ast::compare_le&);
  info generate(io::location, const ast::compare_gt&);
  info generate(io::location, const ast::compare_ge&);
  info generate(io::location, const ast::logical_and&);
  info generate(io::location, const ast::logical_or&);
  info generate(io::location, const ast::logical_not&);
  info generate(io::location, const ast::call&);
  info generate(const ast::expression&);

  // Like generate, but instead of generating the value into a local, generate
  // and store the value at the given address.
  void generate_into(const local_info&, io::location,
                     const ast::literal_aggregate&,
                     const semantics::ir::array_type&);
  void generate_into(const local_info&, io::location,
                     const ast::literal_aggregate&);
  void generate_into(const local_info&, io::location, const ast::logical_and&);
  void generate_into(const local_info&, io::location, const ast::logical_or&);
  template <typename T>
  void generate_into(const local_info& address, io::location l, const T& x) {
    construct_into(address, generate(l, x));
  }
  void generate_into(const local_info&, const ast::expression&);
};

struct module_data {
  bool checked = false;
  std::filesystem::path path;
  environment exports;
};

struct type_info {
  // function copy(destination : *T, source : *T) : void { ... }
  semantics::ir::symbol copy;
  // function move(destination : *T, source : *T) : void { ... }
  semantics::ir::symbol move;
  // function equal(l : *T, r : *T) : bool { ... }
  semantics::ir::symbol equal;
  // function compare(l : *T, r : *T) : int32 { ... }
  semantics::ir::symbol compare;
};

struct checker {
  semantics::ir::symbol next_symbol = semantics::ir::symbol::first_user_symbol;
  semantics::ir::local next_local = {};
  std::map<std::filesystem::path, module_data> modules;
  // Map from structural type to its symbolic name. This is used for looking up
  // operators for moving values of this type around.
  std::map<semantics::ir::data_type, semantics::ir::symbol> type_by_structure;
  // Map from symbolic type names to the table of operators for the type.
  std::map<semantics::ir::symbol, type_info> types = {};
  environment builtins = {
      .names =
          {
              {"bool",
               {{},
                semantics::ir::symbol::builtin_bool,
                "bool",
                type_type{{{}, semantics::ir::builtin_type::bool_type}}}},
              {"int32",
               {{},
                semantics::ir::symbol::builtin_int32,
                "int32",
                type_type{{{}, semantics::ir::builtin_type::int32_type}}}},
              {"void",
               {{},
                semantics::ir::symbol::builtin_void,
                "void",
                type_type{{{}, semantics::ir::builtin_type::void_type}}}},
          },
  };

  // Generate a new unique symbol for some exported artefact.
  semantics::ir::symbol symbol();

  // Generate a new unique id for some local variable.
  semantics::ir::local local();

  // Fetches data about a module. If the module is not yet processed, open the
  // file and process it. If it is already processed, return the existing data.
  const module_data& get_module(const std::filesystem::path& path);

  const type_info& lookup_type(const semantics::ir::data_type&);
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
};

const environment::name_info& environment::lookup(io::location location,
                                                  std::string_view name) const {
  auto i = names.find(name);
  if (i != names.end()) return i->second;
  if (parent) return parent->lookup(location, name);
  io::fatal_message{location, io::message::error} << "undefined name "
                                                  << std::quoted(name) << ".";
}

const environment::name_info& environment::define(
    io::location l, std::string id, name_type type,
    semantics::ir::symbol symbol) {
  auto [i, is_new] =
      names.emplace(id, name_info{l, symbol, id, std::move(type)});
  if (!is_new) {
    io::message{l, io::message::error} << "redeclaration of variable "
                                       << std::quoted(id) << ".";
    io::fatal_message{i->second.location, io::message::note}
        << "previously declared here.";
  }
  return i->second;
}

semantics::ir::symbol checker::symbol() {
  const auto result = next_symbol;
  next_symbol = semantics::ir::symbol((int)next_symbol + 1);
  return result;
}

semantics::ir::local checker::local() {
  const auto result = next_local;
  next_local = semantics::ir::local((int)next_local + 1);
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

const type_info& checker::lookup_type(const semantics::ir::data_type& d) {
  const auto [i, is_new] =
      type_by_structure.emplace(d, semantics::ir::symbol::none);
  if (!is_new) return types.at(i->second);
  i->second = symbol();
  // TODO: Generate definitions for copy and move in terms of sub-types. These
  // should be omitted if the sub-types are not copyable or movable,
  // respectively. For now, treat everything as non-copyable and non-movable.
  return types.emplace(i->second, type_info{}).first->second;
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
      io::message{i->location(), io::message::error}
          << "cannot have an import here.";
      io::fatal_message{prelude_end, io::message::note}
          << "module prelude ended here.";
    }
    if (auto* d = i->get<ast::definition>()) {
      check(*d);
    } else if (auto* e = i->get<ast::exported_definition>()) {
      check(*e);
    } else {
      io::fatal_message{i->location(), io::message::error}
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
    io::fatal_message{l, io::message::error} << "cyclic import.";
  }
  environment.define(l, i.path.back(), module_type{&m.exports},
                     program.symbol());
}

const environment::name_info& module_checker::check(
    io::location l, const ast::variable_definition& v) {
  const auto type = environment.check_type(v.type);
  const auto& info =
      environment.define(l, v.id.value, global{type}, program.symbol());
  if (v.initializer) {
    expression_checker checker{program, *this, environment};
    const auto& lhs =
        checker.add({l, semantics::ir::pointer{info.symbol, type}});
    checker.generate_into(lhs, *v.initializer);
    // TODO: Consume the resulting expression.
  }
  return info;
}

const environment::name_info& module_checker::check(io::location l,
                                            const ast::alias_definition& a) {
  return environment.define(l, a.id.value,
                            type_type{environment.check_type(a.type)},
                            program.symbol());
}

const environment::name_info& module_checker::check(
    io::location l, const ast::function_definition& f) {
  auto return_type = environment.check_type(f.return_type);
  std::vector<semantics::ir::data_type> parameters;
  for (const auto& parameter : f.parameters) {
    // TODO: Check that parameter names are not duplicated.
    parameters.push_back(environment.check_type(parameter.type));
  }
  // TODO: Check the function body.
  return environment.define(l, f.id.value,
                            semantics::ir::function_type{std::move(return_type),
                                                         std::move(parameters)},
                            program.symbol());
}

const environment::name_info& module_checker::check(
    io::location l, const ast::class_definition& c) {
  // TODO: Put the body of the class aside for subsequent checking after all
  // top-level declarations have been handled.
  const auto symbol = program.symbol();
  return environment.define(
      l, c.id.value, type_type{{l, semantics::ir::user_defined_type{symbol}}},
      symbol);
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

const environment::name_info& environment::resolve(
    const ast::expression& e) const {
  if (const auto* i = e.get<ast::identifier>()) {
    return lookup(e.location(), i->value);
  }
  if (const auto* d = e.get<ast::dot>()) {
    const auto& lhs = resolve(d->from);
    if (const auto* m = std::get_if<module_type>(&lhs.type)) {
      // TODO: Improve the error message for unknown names here. It might be
      // nice to point the user at the definition for the LHS in this case.
      return m->exports->lookup(e.location(), d->id.value);
    }
    io::fatal_message{e.location(), io::message::error}
        << "expected module on left hand side of '.'.";
  }
  // TODO: Implement support for nested types.
  io::fatal_message{e.location(), io::message::error}
      << "unsupported scope expression.";
}

semantics::ir::data_type environment::check_type(
    const ast::expression& e) const {
  if (const auto* i = e.get<ast::identifier>()) {
    const auto& info = lookup(e.location(), i->value);
    if (const auto* type = std::get_if<type_type>(&info.type)) {
      return type->type;
    } else {
      io::message{e.location(), io::message::error}
          << std::quoted(i->value) << " does not represent a type.";
      io::fatal_message{info.location, io::message::note} << "defined here.";
    }
  }
  if (const auto* a = e.get<ast::array_type>()) {
    const auto* i = a->size.get<ast::literal_integer>();
    if (!i) {
      // TODO: Support nontrivial size expressions.
      io::fatal_message{a->size.location(), io::message::error}
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
      io::fatal_message{e.location(), io::message::error} << "expected type.";
    }
  }
  io::fatal_message{e.location(), io::message::error}
      << "unsupported type expression.";
}

const semantics::ir::data_type& expression_checker::effective_type(
    const info& info) {
  switch (info.category) {
    case info::rvalue: return info.result->second;
    case info::lvalue:
    case info::xvalue: {
      const auto* p = info.result->second.get<semantics::ir::pointer_type>();
      assert(p);
      return p->pointee;
    }
  }
}

const expression_checker::local_info& expression_checker::add(
    semantics::ir::data_type type, semantics::ir::action action) {
  const auto id = program.local();
  const auto [i, is_new] = result.locals.emplace(id, std::move(type));
  result.steps.emplace_back(semantics::ir::step{id, std::move(action)});
  return *i;
}

const expression_checker::local_info& expression_checker::add(
    semantics::ir::value value) {
  const auto location = value.location();
  auto type = type_of(value);
  return add(std::move(type),
             {location, semantics::ir::constant{std::move(value)}});
}

const expression_checker::local_info& expression_checker::alloc(
    semantics::ir::data_type type) {
  return add(type, {type.location(), semantics::ir::stack_allocate{}});
}

const expression_checker::local_info& expression_checker::index(
    io::location location, const local_info& address,
    const local_info& offset) {
  if (offset.second !=
      semantics::ir::data_type{{}, semantics::ir::builtin_type::int32_type}) {
    io::fatal_message{location, io::message::error}
        << "index offset must be integral.";
  }
  // TODO: Make index() support **T as well.
  auto* p = address.second.get<semantics::ir::pointer_type>();
  if (!p) {
    io::fatal_message{location, io::message::error}
        << "index address must be *[n]T, but got " << address.second << ".";
  }
  auto* a = p->pointee.get<semantics::ir::array_type>();
  if (!a) {
    io::fatal_message{location, io::message::error}
        << "index address must be *[n]T, but got " << address.second << ".";
  }
  return add({location, semantics::ir::pointer_type{a->element}},
             {location, semantics::ir::index{address.first, offset.first}});
}

const expression_checker::local_info& expression_checker::ensure_loaded(
    const info& x) {
  switch (x.category) {
    case info::rvalue:
      return *x.result;
    case info::lvalue:
    case info::xvalue:
      assert(x.result->second.is<semantics::ir::pointer_type>());
      return load(*x.result);
  }
}

const expression_checker::local_info& expression_checker::load(
    const local_info& address) {
  const auto& [a, type] = address;
  if (auto* p = type.get<semantics::ir::pointer_type>()) {
    return add(p->pointee, {type.location(), semantics::ir::load{a}});
  } else {
    io::fatal_message{type.location(), io::message::error}
        << "cannot load from expression of type " << type << ".";
  }
}

void expression_checker::label(semantics::ir::symbol s) {
  auto [i, is_new] = result.labels.emplace(s, result.steps.size());
  assert(is_new);
}

constexpr bool is_bool(const semantics::ir::data_type& t) {
  const auto* b = t.get<semantics::ir::builtin_type>();
  return b && *b == semantics::ir::bool_type;
}

void expression_checker::conditional_jump(io::location location,
                                          const local_info& condition,
                                          semantics::ir::symbol target) {
  assert(is_bool(condition.second));
  add({location, semantics::ir::void_type},
      {location, semantics::ir::conditional_jump{condition.first, target}});
}

void expression_checker::construct_into(const local_info& address,
                                        const info& value) {
  const auto& [destination, destination_type] = address;
  auto* const p = destination_type.get<semantics::ir::pointer_type>();
  if (!p) {
    io::fatal_message{destination_type.location(), io::message::error}
        << "cannot store to address expression of type " << destination_type
        << ".";
  }
  const auto [category, value_info] = value;
  const auto& [source, source_type] = *value_info;
  switch (category) {
    case info::rvalue: {
      if (p->pointee != source_type) {
        io::fatal_message{destination_type.location(), io::message::error}
            << "cannot store expression of type " << source_type
            << " to address expression of type " << destination_type << ".";
      }
      add(p->pointee,
          {source_type.location(), semantics::ir::store{destination, source}});
      break;
    }
    case info::lvalue: {
      if (destination_type != source_type) {
        io::fatal_message{destination_type.location(), io::message::error}
            << "cannot copy-construct expression of type " << source_type
            << " to address expression of type " << destination_type << ".";
      }
      const auto& type_info = program.lookup_type(destination_type);
      if (type_info.copy == semantics::ir::none) {
        io::fatal_message{destination_type.location(), io::message::error}
            << p->pointee << " is not known to be copyable.";
      }
      semantics::ir::function_type copy_type{
          .return_type = {destination_type.location(),
                          semantics::ir::builtin_type::void_type},
          .parameters = {destination_type, source_type},
      };
      add({destination_type.location(),
           semantics::ir::function_pointer{type_info.copy,
                                           std::move(copy_type)}});
      break;
    }
    case info::xvalue: {
      if (p->pointee != source_type) {
        io::fatal_message{destination_type.location(), io::message::error}
            << "cannot move-construct expression of type " << source_type
            << " to address expression of type " << destination_type << ".";
      }
      const auto& type_info = program.lookup_type(destination_type);
      if (type_info.move == semantics::ir::none) {
        io::fatal_message{destination_type.location(), io::message::error}
            << p->pointee << " is not known to be movable.";
      }
      semantics::ir::function_type move_type{
          .return_type = {destination_type.location(),
                          semantics::ir::builtin_type::void_type},
          .parameters = {destination_type, source_type},
      };
      add({destination_type.location(),
           semantics::ir::function_pointer{type_info.move,
                                           std::move(move_type)}});
      break;
    }
  }
}

expression_checker::info expression_checker::generate(
    io::location location, const environment::name_info& info) {
  if (const auto* f = std::get_if<semantics::ir::function_type>(&info.type)) {
    const auto& result =
        add({location, semantics::ir::function_pointer{info.symbol, *f}});
    return {.category = info::lvalue, .result = &result};
  }
  if (const auto* g = std::get_if<global>(&info.type)) {
    const auto& result =
        add({location, semantics::ir::pointer{info.symbol, g->type}});
    return {.category = info::lvalue, .result = &result};
  }
  if (const auto* l = std::get_if<local>(&info.type)) {
    // TODO: Implement code generation for accessing local variables.
    io::fatal_message{location, io::message::error}
        << "local variables are unimplemented.";
  }
  if (const auto* t = std::get_if<type_type>(&info.type)) {
    io::message{location, io::message::error}
        << "a type may not appear in an expression.";
    io::fatal_message{location, io::message::note}
        << "a value literal would have the syntax `type{...}`.";
  }
  // TODO: Find a nice way to ensure that this function is exhaustative.
  io::fatal_message{location, io::message::error} << "unimplemented name type.";
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::identifier& i) {
  // In the IR, we will represent variable references as pointers with an lvalue
  // category.
  return generate(location, environment.lookup(location, i.value));
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::literal_integer& i) {
  if (i.value > std::numeric_limits<std::int32_t>::max()) {
    io::fatal_message{location, io::message::error}
        << "integer literal exceeds the maximum allowed value for int32.";
  }
  const auto& result = add({location, std::int32_t(i.value)});
  return {.category = info::rvalue, .result = &result};
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::literal_string&) {
  // TODO: Implement string literals.
  io::fatal_message{location, io::message::error}
      << "string literals are unimplemented.";
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::literal_aggregate& a,
    const semantics::ir::array_type& array) {
  const auto& mem = alloc({location, array});
  generate_into(mem, location, a, array);
  return {.category = info::xvalue, .result = &mem};
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::literal_aggregate& a) {
  const auto type = environment.check_type(a.type);
  if (const auto* array = type.get<semantics::ir::array_type>()) {
    return generate(location, a, *array);
  }
  // TODO: Implement object literals.
  io::fatal_message{location, io::message::error}
      << "unimplemented aggregate literal type.";
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::array_type&) {
  io::message{location, io::message::error} << "unexpected type in expression.";
  io::fatal_message{location, io::message::note}
      << "types may only appear in expressions as part of aggregate "
         "initializers, which have the syntax `type{...}`.";
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::dot& d) {
  // Normally, `<expr>.bar` is indirection into a class. As a special case,
  // `foo.bar` may instead mean accessing the name `bar` from the imported
  // module `some.path.foo`.
  if (auto* i = d.from.get<ast::identifier>()) {
    const auto& lhs = environment.lookup(location, i->value);
    if (const auto* m = std::get_if<module_type>(&lhs.type)) {
      return generate(location, m->exports->lookup(location, d.id.value));
    }
  }
  // TODO: Implement access into objects.
  io::fatal_message{location, io::message::error}
      << "object access is unimplemented.";
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::dereference& d) {
  auto inner = generate(d.from);
  if (auto* p = inner.result->second.get<semantics::ir::pointer_type>()) {
    return {.category = info::lvalue, .result = inner.result};
  } else {
    io::fatal_message{location, io::message::error}
        << "cannot dereference expression of type " << inner.result->second
        << ".";
  }
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::address_of& a) {
  auto inner = generate(a.inner);
  if (inner.category != info::lvalue) {
    io::fatal_message{location, io::message::error}
        << "cannot take the address of a temporary.";
  }
  return {.category = info::rvalue, .result = inner.result};
}

expression_checker::info expression_checker::generate(io::location location,
                                                      const ast::index& i) {
  const auto& result =
      index(location, *generate(i.from).result, *generate(i.index).result);
  return {.category = info::lvalue, .result = &result};
}

constexpr bool is_integral(const semantics::ir::data_type& t) {
  const auto* b = t.get<semantics::ir::builtin_type>();
  return b && *b == semantics::ir::int32_type;
}

// TODO: There is a load of duplication for the functions handling different
// arithmetic operators. Figure out a nice way of removing all the duplication.

expression_checker::info expression_checker::generate(
    io::location location, const ast::negate& n) {
  const auto& l = ensure_loaded(generate(n.inner));
  if (!is_integral(l.second)) {
    io::fatal_message{location, io::message::error}
        << "can't negate expression of type " << l.second << '.';
  }
  const auto& out = add({location, semantics::ir::int32_type},
                        {location, semantics::ir::negate{l.first}});
  return {.category = info::rvalue, .result = &out};
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::add& a) {
  const auto& l = ensure_loaded(generate(a.left));
  const auto& r = ensure_loaded(generate(a.right));
  // TODO: Implement pointer arithmetic.
  if (!is_integral(l.second)) {
    io::fatal_message{a.left.location(), io::message::error}
        << "can't add expression of type " << l.second << '.';
  }
  if (!is_integral(r.second)) {
    io::fatal_message{a.right.location(), io::message::error}
        << "can't add expression of type " << r.second << '.';
  }
  const auto& out = add({location, semantics::ir::int32_type},
                        {location, semantics::ir::add{l.first, r.first}});
  return {.category = info::rvalue, .result = &out};
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::subtract& s) {
  const auto& l = ensure_loaded(generate(s.left));
  const auto& r = ensure_loaded(generate(s.right));
  // TODO: Implement pointer arithmetic.
  if (!is_integral(l.second)) {
    io::fatal_message{s.left.location(), io::message::error}
        << "can't add expression of type " << l.second << '.';
  }
  if (!is_integral(r.second)) {
    io::fatal_message{s.right.location(), io::message::error}
        << "can't add expression of type " << r.second << '.';
  }
  const auto& out = add({location, semantics::ir::int32_type},
                        {location, semantics::ir::subtract{l.first, r.first}});
  return {.category = info::rvalue, .result = &out};
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::multiply& m) {
  const auto& l = ensure_loaded(generate(m.left));
  const auto& r = ensure_loaded(generate(m.right));
  if (!is_integral(l.second)) {
    io::fatal_message{m.left.location(), io::message::error}
        << "can't add expression of type " << l.second << '.';
  }
  if (!is_integral(r.second)) {
    io::fatal_message{m.right.location(), io::message::error}
        << "can't add expression of type " << r.second << '.';
  }
  const auto& out = add({location, semantics::ir::int32_type},
                        {location, semantics::ir::multiply{l.first, r.first}});
  return {.category = info::rvalue, .result = &out};
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::divide& d) {
  const auto& l = ensure_loaded(generate(d.left));
  const auto& r = ensure_loaded(generate(d.right));
  if (!is_integral(l.second)) {
    io::fatal_message{d.left.location(), io::message::error}
        << "can't add expression of type " << l.second << '.';
  }
  if (!is_integral(r.second)) {
    io::fatal_message{d.right.location(), io::message::error}
        << "can't add expression of type " << r.second << '.';
  }
  const auto& out = add({location, semantics::ir::int32_type},
                        {location, semantics::ir::divide{l.first, r.first}});
  return {.category = info::rvalue, .result = &out};
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::modulo& m) {
  const auto& l = ensure_loaded(generate(m.left));
  const auto& r = ensure_loaded(generate(m.right));
  if (!is_integral(l.second)) {
    io::fatal_message{m.left.location(), io::message::error}
        << "can't add expression of type " << l.second << '.';
  }
  if (!is_integral(r.second)) {
    io::fatal_message{m.right.location(), io::message::error}
        << "can't add expression of type " << r.second << '.';
  }
  const auto& out = add({location, semantics::ir::int32_type},
                        {location, semantics::ir::modulo{l.first, r.first}});
  return {.category = info::rvalue, .result = &out};
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::compare_eq& c) {
  const info l = generate(c.left);
  const info r = generate(c.right);
  const auto& ltype = effective_type(l);
  const auto& rtype = effective_type(r);
  if (ltype != rtype) {
    io::fatal_message{location, io::message::error}
        << "type mismatch in comparison: " << ltype << " vs. " << rtype << ".";
  }
  if (auto* b = ltype.get<semantics::ir::builtin_type>()) {
    const auto& l2 = ensure_loaded(l);
    const auto& r2 = ensure_loaded(r);
    switch (*b) {
      case semantics::ir::void_type:
        // void values are unconditionally equal to each other.
        return {.category = info::rvalue, .result = &add({location, true})};
      case semantics::ir::bool_type:
      case semantics::ir::int32_type:
        return {.category = info::rvalue,
                .result = &add(
                    {location, semantics::ir::bool_type},
                    {location, semantics::ir::compare_eq{l2.first, r2.first}})};
    }
  }
  // TODO: Implement comparison of other types.
  io::fatal_message{location, io::message::error}
      << "comparison between objects of type " << ltype << " is unimplemented.";
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::compare_ne& c) {
  const info l = generate(c.left);
  const info r = generate(c.right);
  const auto& ltype = effective_type(l);
  const auto& rtype = effective_type(r);
  if (ltype != rtype) {
    io::fatal_message{location, io::message::error}
        << "type mismatch in comparison: " << ltype << " vs. " << rtype << ".";
  }
  if (auto* b = ltype.get<semantics::ir::builtin_type>()) {
    const auto& l2 = ensure_loaded(l);
    const auto& r2 = ensure_loaded(r);
    switch (*b) {
      case semantics::ir::void_type:
        // void values are unconditionally equal to each other.
        return {.category = info::rvalue, .result = &add({location, true})};
      case semantics::ir::bool_type:
      case semantics::ir::int32_type:
        return {.category = info::rvalue,
                .result = &add(
                    {location, semantics::ir::bool_type},
                    {location, semantics::ir::compare_ne{l2.first, r2.first}})};
    }
  }
  // TODO: Implement comparison of other types.
  io::fatal_message{location, io::message::error}
      << "comparison between objects of type " << ltype << " is unimplemented.";
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::compare_lt& c) {
  const info l = generate(c.left);
  const info r = generate(c.right);
  const auto& ltype = effective_type(l);
  const auto& rtype = effective_type(r);
  if (ltype != rtype) {
    io::fatal_message{location, io::message::error}
        << "type mismatch in comparison: " << ltype << " vs. " << rtype << ".";
  }
  if (auto* b = ltype.get<semantics::ir::builtin_type>()) {
    const auto& l2 = ensure_loaded(l);
    const auto& r2 = ensure_loaded(r);
    switch (*b) {
      case semantics::ir::void_type:
        // void values are unconditionally equal to each other.
        return {.category = info::rvalue, .result = &add({location, true})};
      case semantics::ir::bool_type:
      case semantics::ir::int32_type:
        return {.category = info::rvalue,
                .result = &add(
                    {location, semantics::ir::bool_type},
                    {location, semantics::ir::compare_lt{l2.first, r2.first}})};
    }
  }
  // TODO: Implement comparison of other types.
  io::fatal_message{location, io::message::error}
      << "comparison between objects of type " << ltype << " is unimplemented.";
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::compare_le& c) {
  const info l = generate(c.left);
  const info r = generate(c.right);
  const auto& ltype = effective_type(l);
  const auto& rtype = effective_type(r);
  if (ltype != rtype) {
    io::fatal_message{location, io::message::error}
        << "type mismatch in comparison: " << ltype << " vs. " << rtype << ".";
  }
  if (auto* b = ltype.get<semantics::ir::builtin_type>()) {
    const auto& l2 = ensure_loaded(l);
    const auto& r2 = ensure_loaded(r);
    switch (*b) {
      case semantics::ir::void_type:
        // void values are unconditionally equal to each other.
        return {.category = info::rvalue, .result = &add({location, true})};
      case semantics::ir::bool_type:
      case semantics::ir::int32_type:
        return {.category = info::rvalue,
                .result = &add(
                    {location, semantics::ir::bool_type},
                    {location, semantics::ir::compare_le{l2.first, r2.first}})};
    }
  }
  // TODO: Implement comparison of other types.
  io::fatal_message{location, io::message::error}
      << "comparison between objects of type " << ltype << " is unimplemented.";
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::compare_gt& c) {
  const info l = generate(c.left);
  const info r = generate(c.right);
  const auto& ltype = effective_type(l);
  const auto& rtype = effective_type(r);
  if (ltype != rtype) {
    io::fatal_message{location, io::message::error}
        << "type mismatch in comparison: " << ltype << " vs. " << rtype << ".";
  }
  if (auto* b = ltype.get<semantics::ir::builtin_type>()) {
    const auto& l2 = ensure_loaded(l);
    const auto& r2 = ensure_loaded(r);
    switch (*b) {
      case semantics::ir::void_type:
        // void values are unconditionally equal to each other.
        return {.category = info::rvalue, .result = &add({location, true})};
      case semantics::ir::bool_type:
      case semantics::ir::int32_type:
        return {.category = info::rvalue,
                .result = &add(
                    {location, semantics::ir::bool_type},
                    {location, semantics::ir::compare_gt{l2.first, r2.first}})};
    }
  }
  // TODO: Implement comparison of other types.
  io::fatal_message{location, io::message::error}
      << "comparison between objects of type " << ltype << " is unimplemented.";
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::compare_ge& c) {
  const info l = generate(c.left);
  const info r = generate(c.right);
  const auto& ltype = effective_type(l);
  const auto& rtype = effective_type(r);
  if (ltype != rtype) {
    io::fatal_message{location, io::message::error}
        << "type mismatch in comparison: " << ltype << " vs. " << rtype << ".";
  }
  if (auto* b = ltype.get<semantics::ir::builtin_type>()) {
    const auto& l2 = ensure_loaded(l);
    const auto& r2 = ensure_loaded(r);
    switch (*b) {
      case semantics::ir::void_type:
        // void values are unconditionally equal to each other.
        return {.category = info::rvalue, .result = &add({location, true})};
      case semantics::ir::bool_type:
      case semantics::ir::int32_type:
        return {.category = info::rvalue,
                .result = &add(
                    {location, semantics::ir::bool_type},
                    {location, semantics::ir::compare_ge{l2.first, r2.first}})};
    }
  }
  // TODO: Implement comparison of other types.
  io::fatal_message{location, io::message::error}
      << "comparison between objects of type " << ltype << " is unimplemented.";
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::logical_and& a) {
  const auto& mem = alloc({location, semantics::ir::bool_type});
  generate_into(mem, location, a);
  return {.category = info::lvalue, .result = &mem};
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::logical_or& o) {
  const auto& mem = alloc({location, semantics::ir::bool_type});
  generate_into(mem, location, o);
  return {.category = info::xvalue, .result = &mem};
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::logical_not& n) {
  const auto inner = generate(n.inner);
  if (!is_bool(inner.result->second)) {
    io::fatal_message{location, io::message::error}
        << "cannot logically negate expression of type " << inner.result->second
        << ".";
  }
  const auto& result =
      add({location, semantics::ir::bool_type},
          {location, semantics::ir::logical_not{inner.result->first}});
  return {.category = info::rvalue, .result = &result};
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::call& c) {
  generate(c.callee);
  for (const auto& argument : c.arguments) generate(argument);
  // TODO: Implement function calls. This will require deciding how to pass each
  // value type. Probably a simple option is to pass all builtins in registers
  // and all aggregate types by reference.
  io::fatal_message{location, io::message::error}
      << "function calls are unimplemented.";
}

expression_checker::info expression_checker::generate(
    const ast::expression& e) {
  return e.visit([&](const auto& x) { return generate(e.location(), x); });
}

void expression_checker::generate_into(const local_info& address,
                                       io::location location,
                                       const ast::literal_aggregate& a,
                                       const semantics::ir::array_type& array) {
  // Array literals can either have delimited indices, e.g.
  // `[256]bool{[42] = true}`, or they can have ordered bare values, e.g.
  // `[3]int32{1, 2, 3}`. These two options cannot be mixed, and only the
  // former option can specify a partial set of indices.
  bool has_index = false;
  bool has_bare = false;
  for (const auto& argument : a.arguments) {
    if (auto* e = std::get_if<ast::expression>(&argument)) {
      if (has_index) {
        io::fatal_message{e->location(), io::message::error}
            << "cannot mix bare expressions and indexed expressions in an "
               "array literal.";
      }
      has_bare = true;
      continue;
    }
    if (auto* f = std::get_if<ast::literal_aggregate::field_assignment>(
            &argument)) {
      io::fatal_message{f->value.location(), io::message::error}
          << "cannot have field assignments in an array literal.";
    }
    if (auto* i = std::get_if<ast::literal_aggregate::index_assignment>(
            &argument)) {
      if (has_bare) {
        io::fatal_message{i->value.location(), io::message::error}
            << "cannot mix bare expressions and indexed expressions in an "
               "array literal.";
      }
      has_index = true;
    }
  }
  assert(!has_index || !has_bare);
  if (has_bare) {
    if (a.arguments.size() != array.size) {
      io::message{location, io::message::error}
          << "unindexed array literals must initialize every value.";
      io::fatal_message{a.type.location(), io::message::note}
          << "expected " << array.size << " initializers but got "
          << a.arguments.size() << ".";
    }
    for (std::int32_t i = 0, n = a.arguments.size(); i < n; i++) {
      const auto& argument = std::get<ast::expression>(a.arguments[i]);
      const auto& target =
          index(argument.location(), address, add({argument.location(), i}));
      generate_into(target, argument);
    }
    return;
  }
  // TODO: Implement designated array literals, e.g. `[256]bool{[42] = true}`.
  io::fatal_message{a.type.location(), io::message::error}
      << "unimplemented array literal type.";
}

void expression_checker::generate_into(const local_info& address,
                                       io::location location,
                                       const ast::literal_aggregate& a) {
  const auto type = environment.check_type(a.type);
  if (const auto* array = type.get<semantics::ir::array_type>()) {
    return generate_into(address, location, a, *array);
  }
  // TODO: Implement object literals.
  io::fatal_message{location, io::message::error}
      << "unimplemented aggregate literal type.";
}

void expression_checker::generate_into(const local_info& address,
                                       io::location location,
                                       const ast::logical_and& a) {
  const auto end = program.symbol();
  generate_into(address, a.left);
  const auto& value =
      add({location, semantics::ir::bool_type},
          {location, semantics::ir::logical_not{load(address).first}});
  conditional_jump(location, value, end);
  generate_into(address, a.right);
  label(end);
}

void expression_checker::generate_into(const local_info& address,
                                       io::location location,
                                       const ast::logical_or& o) {
  const auto end = program.symbol();
  generate_into(address, o.left);
  conditional_jump(location, load(address), end);
  generate_into(address, o.right);
  label(end);
}

void expression_checker::generate_into(const local_info& address,
                                       const ast::expression& e) {
  return e.visit(
      [&](const auto& x) { generate_into(address, e.location(), x); });
}

export void check(const char* filename) {
  checker{}.get_module(std::filesystem::absolute(filename));
}

}  // namespace syntax
