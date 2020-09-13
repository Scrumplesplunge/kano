// Check Kano source code, producing Kano IR.

module;

#include <cassert>

export module syntax.check;

import syntax.parse;
export import semantics.ir;
import<iostream>;
import<filesystem>;

namespace syntax {

namespace ir = ::semantics::ir;

using variable_info = std::pair<const ir::variable, ir::data_type>;

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
  const variable_info* result;
};

template <ir::builtin_type t>
constexpr bool is(const ir::data_type& x) {
  const auto* b = x.get<ir::builtin_type>();
  return b && *b == t;
}

struct environment;

struct function {
  ir::symbol symbol;
  ir::function_type type;
};

struct module_type {
  const environment* exports;
};

struct global {
  ir::symbol symbol;
  ir::data_type type;
};

struct variable {
  ir::local id;
  ir::data_type type;
};

// Used for names which represent types instead of names which represent values
// of a given type.
struct type_type {
  ir::data_type type;
};

using name_type =
    std::variant<function, module_type, global, variable, type_type>;

struct checker;
struct module_checker;

// An environment keeps track of what names are currently in scope, and what
// symbols they map to.
struct environment {
  environment* parent = nullptr;
  struct name_info {
    io::location location;
    std::string name;
    name_type type;
  };
  std::map<std::string, name_info, std::less<>> names = {};
  std::optional<ir::symbol> break_label = {};
  std::optional<ir::symbol> continue_label = {};

  std::optional<ir::symbol> lookup_break() const;
  std::optional<ir::symbol> lookup_continue() const;

  // Resolve a name within the environment, searching upwards through the
  // lexical scope for its definition.
  const name_info& lookup(io::location location, std::string_view name) const;

  // Define a name within the current scope.
  const name_info& define(io::location location, std::string name,
                          name_type type);

  ir::data_type check_type(const ast::expression&) const;
  const environment::name_info& resolve(const ast::expression&) const;
};

template <typename function>
struct function_builder {
  checker& program;
  function result;

  const auto& make_stack_variable(ir::data_type);
  const variable_info& make_variable(ir::data_type);
  const variable_info& local(io::location, ir::local);
  template <typename T, typename... Args>
  void step(io::location, Args&&...);
  template <typename T, typename... Args>
  const variable_info& add(io::location, ir::data_type, Args&&...);
  // Given a pointer to an indexable object (i.e. [n]T) and an index i, produce
  // a pointer to the ith element of the object.
  const variable_info& index(io::location, const variable_info&,
                             const variable_info&);
  const variable_info& ensure_loaded(const info&);
  const variable_info& load(const variable_info&);
  void construct_into(const variable_info&, const info&);
};

struct expression_checker : function_builder<ir::function&> {
  const environment& environment;

  const ir::data_type& effective_type(const info&);

  info generate(io::location, const environment::name_info&);
  info generate(io::location, const ast::identifier&);
  info generate(io::location, const ast::literal_integer&);
  info generate(io::location, const ast::literal_string&);
  info generate(io::location, const ast::literal_aggregate&,
                const ir::array_type&);
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

  // Like generate, but instead of generating the value into a variable,
  // generate and store the value at the given address.
  void generate_into(const variable_info&, io::location,
                     const ast::literal_aggregate&, const ir::array_type&);
  void generate_into(const variable_info&, io::location,
                     const ast::literal_aggregate&);
  void generate_into(const variable_info&, io::location,
                     const ast::logical_and&);
  void generate_into(const variable_info&, io::location,
                     const ast::logical_or&);
  template <typename T>
  void generate_into(const variable_info& address, io::location l, const T& x) {
    construct_into(address, generate(l, x));
  }
  void generate_into(const variable_info&, const ast::expression&);
};

struct module_data {
  bool checked = false;
  std::filesystem::path path;
  environment exports;
};

struct type_info {
  // function copy(destination : *T, source : *T) : void { ... }
  ir::symbol copy;
  // function move(destination : *T, source : *T) : void { ... }
  ir::symbol move;
  // function equal(l : *T, r : *T) : bool { ... }
  ir::symbol equal;
  // function compare(l : *T, r : *T) : int32 { ... }
  ir::symbol compare;
};

struct checker {
  ir::symbol next_symbol = ir::symbol::first_user_symbol;
  ir::variable next_variable = {};
  ir::function initialization = {};
  std::map<ir::symbol, ir::function> functions = {};
  std::map<std::filesystem::path, module_data> modules;
  // Map from structural type to its symbolic name. This is used for looking up
  // operators for moving values of this type around.
  std::map<ir::data_type, ir::symbol> type_by_structure;
  // Map from symbolic type names to the table of operators for the type.
  std::map<ir::symbol, type_info> types = {};
  environment builtins = {
      .names = {{"bool", {{}, "bool", type_type{{{}, ir::bool_type}}}},
                {"int32", {{}, "int32", type_type{{{}, ir::int32_type}}}},
                {"void", {{}, "void", type_type{{{}, ir::void_type}}}}}};

  // Fetches data about a module. If the module is not yet processed, open the
  // file and process it. If it is already processed, return the existing data.
  const module_data& get_module(const std::filesystem::path& path);

  const type_info& lookup_type(const ir::data_type&);
};

struct module_checker {
  checker& program;
  const std::filesystem::path& path;
  module_data& module;
  environment environment = {&program.builtins};
  expression_checker initialization = {{program, program.initialization},
                                       environment};

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

struct function_checker : function_builder<ir::function> {
  module_checker& module;
  const ir::function_type& type;
  // TODO: Find a nicer way of mapping these. One option is to merge symbol and
  // variable into a single type that uniquely identifies "things", but possibly
  // that is more bug-prone.
  std::map<ir::symbol, const variable_info*> variables = {};

  void check(io::location, const ast::function_definition&);

  void generate(environment&, io::location, const ast::import_statement&);
  void generate(environment&, io::location, const ast::variable_definition&);
  void generate(environment&, io::location, const ast::alias_definition&);
  void generate(environment&, io::location, const ast::function_definition&);
  void generate(environment&, io::location, const ast::class_definition&);
  void generate(environment&, io::location, const ast::definition&);
  void generate(environment&, io::location, const ast::exported_definition&);
  void generate(environment&, io::location, const ast::assignment&);
  void generate(environment&, io::location, const ast::if_statement&);
  void generate(environment&, io::location, const ast::while_statement&);
  void generate(environment&, io::location, const ast::break_statement&);
  void generate(environment&, io::location, const ast::continue_statement&);
  void generate(environment&, io::location, const ast::return_statement&);
  void generate(environment&, io::location, const ast::expression_statement&);
  void generate(environment&, io::location, const ast::block_statement&);
  void generate(environment&, const ast::statement&);
};

std::optional<ir::symbol> environment::lookup_break() const {
  if (break_label) return break_label;
  if (parent) return parent->lookup_break();
  return std::nullopt;
}

std::optional<ir::symbol> environment::lookup_continue() const {
  if (continue_label) return continue_label;
  if (parent) return parent->lookup_continue();
  return std::nullopt;
}

const environment::name_info& environment::lookup(io::location location,
                                                  std::string_view name) const {
  auto i = names.find(name);
  if (i != names.end()) return i->second;
  if (parent) return parent->lookup(location, name);
  io::fatal_message{location, io::message::error} << "undefined name "
                                                  << std::quoted(name) << ".";
}

const environment::name_info& environment::define(io::location l,
                                                  std::string id,
                                                  name_type type) {
  auto [i, is_new] = names.emplace(id, name_info{l, id, std::move(type)});
  if (!is_new) {
    io::message{l, io::message::error} << "redeclaration of variable "
                                       << std::quoted(id) << ".";
    io::fatal_message{i->second.location, io::message::note}
        << "previously declared here.";
  }
  return i->second;
}

const module_data& checker::get_module(const std::filesystem::path& path) {
  const auto name = std::filesystem::relative(path).native();
  std::cerr << name << "...\n";
  auto [i, is_new] = modules.emplace(path, module_data{});
  if (!is_new) return i->second;
  module_checker{*this, i->first, i->second}.check();
  return i->second;
}

const type_info& checker::lookup_type(const ir::data_type& d) {
  const auto [i, is_new] = type_by_structure.emplace(d, ir::symbol::none);
  if (!is_new) return types.at(i->second);
  i->second = ir::make_symbol();
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
  environment.define(l, i.path.back(), module_type{&m.exports});
}

const environment::name_info& module_checker::check(
    io::location l, const ast::variable_definition& v) {
  const auto type = environment.check_type(v.type);
  const ir::symbol id = ir::make_symbol();
  const auto& info = environment.define(l, v.id.value, global{id, type});
  if (v.initializer) {
    const auto& lhs = initialization.add<ir::copy>(
        l, {l, ir::pointer_type{std::move(type)}}, id);
    initialization.generate_into(lhs, *v.initializer);
  }
  return info;
}

const environment::name_info& module_checker::check(
    io::location l, const ast::alias_definition& a) {
  return environment.define(l, a.id.value,
                            type_type{environment.check_type(a.type)});
}

const environment::name_info& module_checker::check(
    io::location l, const ast::function_definition& f) {
  auto return_type = environment.check_type(f.return_type);
  std::vector<ir::data_type> parameters;
  for (const auto& parameter : f.parameters) {
    // TODO: Check that parameter names are not duplicated.
    parameters.push_back(environment.check_type(parameter.type));
  }
  const auto id = ir::make_symbol();
  const auto& info = environment.define(
      l, f.id.value,
      function{id, {std::move(return_type), std::move(parameters)}});
  function_checker checker{
      {program, {}}, *this, std::get<function>(info.type).type};
  checker.check(l, f);
  program.functions.emplace(id, std::move(checker.result));
  return info;
}

const environment::name_info& module_checker::check(
    io::location l, const ast::class_definition& c) {
  // TODO: Put the body of the class aside for subsequent checking after all
  // top-level declarations have been handled.
  const auto symbol = ir::make_symbol();
  return environment.define(l, c.id.value,
                            type_type{{l, ir::user_defined_type{symbol}}});
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

ir::data_type environment::check_type(const ast::expression& e) const {
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
    return {e.location(), ir::array_type{i->value, std::move(element)}};
  }
  if (const auto* d = e.get<ast::dereference>()) {
    // TODO: Add support for function pointer types once they have syntax.
    auto pointee = check_type(d->from);
    return {e.location(), ir::pointer_type{std::move(pointee)}};
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

const ir::data_type& expression_checker::effective_type(const info& info) {
  switch (info.category) {
    case info::rvalue:
      return info.result->second;
    case info::lvalue:
    case info::xvalue: {
      const auto* p = info.result->second.get<ir::pointer_type>();
      assert(p);
      return p->pointee;
    }
  }
}

template <typename function>
const auto& function_builder<function>::make_stack_variable(
    ir::data_type type) {
  const auto id = ir::make_local();
  auto [i, is_new] = result.stack_variables.emplace(id, std::move(type));
  assert(is_new);
  return *i;
}

template <typename function>
const variable_info& function_builder<function>::make_variable(
    ir::data_type type) {
  const auto id = ir::make_variable();
  auto [i, is_new] = result.variables.emplace(id, std::move(type));
  assert(is_new);
  return *i;
}

template <typename function>
const variable_info& function_builder<function>::local(io::location l,
                                                       ir::local id) {
  const auto& type = result.stack_variables.at(id);
  return add<ir::copy>(l, {type.location(), ir::pointer_type{type}}, id);
}

template <typename function>
template <typename T, typename... Args>
void function_builder<function>::step(io::location location, Args&&... args) {
  result.steps.push_back({location, T{std::forward<Args>(args)...}});
}

template <typename function>
template <typename T, typename... Args>
const variable_info& function_builder<function>::add(io::location location,
                                                     ir::data_type type,
                                                     Args&&... args) {
  const auto& info = make_variable(std::move(type));
  result.steps.push_back(
      {location, T{info.first, std::forward<Args>(args)...}});
  return info;
}

template <typename function>
const variable_info& function_builder<function>::index(
    io::location location, const variable_info& address,
    const variable_info& offset) {
  if (offset.second != ir::data_type{{}, ir::builtin_type::int32_type}) {
    io::fatal_message{location, io::message::error}
        << "index offset must be integral.";
  }
  // TODO: Make index() support **T as well.
  auto* p = address.second.get<ir::pointer_type>();
  if (!p) {
    io::fatal_message{location, io::message::error}
        << "index address must be *[n]T, but got " << address.second << ".";
  }
  auto* a = p->pointee.get<ir::array_type>();
  if (!a) {
    io::fatal_message{location, io::message::error}
        << "index address must be *[n]T, but got " << address.second << ".";
  }
  return add<ir::index>(location, {location, ir::pointer_type{a->element}},
                        address.first, offset.first);
}

template <typename function>
const variable_info& function_builder<function>::ensure_loaded(const info& x) {
  switch (x.category) {
    case info::rvalue:
      return *x.result;
    case info::lvalue:
    case info::xvalue:
      assert(x.result->second.is<ir::pointer_type>());
      return load(*x.result);
  }
}

template <typename function>
const variable_info& function_builder<function>::load(
    const variable_info& address) {
  const auto& [a, type] = address;
  if (auto* p = type.get<ir::pointer_type>()) {
    return add<ir::load>(type.location(), p->pointee, a);
  } else {
    io::fatal_message{type.location(), io::message::error}
        << "cannot load from expression of type " << type << ".";
  }
}

template <typename function>
void function_builder<function>::construct_into(const variable_info& address,
                                                const info& value) {
  const auto& [destination, destination_type] = address;
  auto* const p = destination_type.get<ir::pointer_type>();
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
      step<ir::store>(source_type.location(), destination, source);
      break;
    }
    case info::lvalue: {
      if (destination_type != source_type) {
        io::fatal_message{destination_type.location(), io::message::error}
            << "cannot copy-construct expression of type " << source_type
            << " to address expression of type " << destination_type << ".";
      }
      const auto& type_info = program.lookup_type(destination_type);
      if (type_info.copy == ir::none) {
        io::fatal_message{destination_type.location(), io::message::error}
            << p->pointee << " is not known to be copyable.";
      }
      ir::function_type copy_type{
          .return_type = {destination_type.location(),
                          ir::builtin_type::void_type},
          .parameters = {destination_type, source_type},
      };
      add<ir::copy>(destination_type.location(),
                    {destination_type.location(),
                     ir::function_pointer_type{std::move(copy_type)}},
                    type_info.copy);
      break;
    }
    case info::xvalue: {
      if (p->pointee != source_type) {
        io::fatal_message{destination_type.location(), io::message::error}
            << "cannot move-construct expression of type " << source_type
            << " to address expression of type " << destination_type << ".";
      }
      const auto& type_info = program.lookup_type(destination_type);
      if (type_info.move == ir::none) {
        io::fatal_message{destination_type.location(), io::message::error}
            << p->pointee << " is not known to be movable.";
      }
      ir::function_type move_type{
          .return_type = {destination_type.location(),
                          ir::builtin_type::void_type},
          .parameters = {destination_type, source_type},
      };
      add<ir::copy>(destination_type.location(),
                    {destination_type.location(),
                     ir::function_pointer_type{std::move(move_type)}},
                    type_info.move);
      break;
    }
  }
}

info expression_checker::generate(io::location location,
                                  const environment::name_info& info) {
  if (const auto* f = std::get_if<function>(&info.type)) {
    const auto& result = add<ir::copy>(
        location, {location, ir::function_pointer_type{f->type}}, f->symbol);
    return {.category = info::lvalue, .result = &result};
  }
  if (const auto* g = std::get_if<global>(&info.type)) {
    const auto& result = add<ir::copy>(
        location, {location, ir::pointer_type{g->type}}, g->symbol);
    return {.category = info::lvalue, .result = &result};
  }
  if (const auto* l = std::get_if<variable>(&info.type)) {
    return {.category = info::lvalue,
            .result = &add<ir::copy>(location, l->type, l->id)};
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

info expression_checker::generate(io::location location,
                                  const ast::identifier& i) {
  // In the IR, we will represent variable references as pointers with an lvalue
  // category.
  return generate(location, environment.lookup(location, i.value));
}

info expression_checker::generate(io::location location,
                                  const ast::literal_integer& i) {
  if (i.value > std::numeric_limits<std::int32_t>::max()) {
    io::fatal_message{location, io::message::error}
        << "integer literal exceeds the maximum allowed value for int32.";
  }
  const auto& result = add<ir::copy>(location, {location, ir::int32_type},
                                     std::int32_t(i.value));
  return {.category = info::rvalue, .result = &result};
}

info expression_checker::generate(io::location location,
                                  const ast::literal_string&) {
  // TODO: Implement string literals.
  io::fatal_message{location, io::message::error}
      << "string literals are unimplemented.";
}

info expression_checker::generate(io::location location,
                                  const ast::literal_aggregate& a,
                                  const ir::array_type& array) {
  const auto& mem = make_stack_variable({location, array});
  generate_into(local(location, mem.first), location, a, array);
  return {.category = info::xvalue, .result = &local(location, mem.first)};
}

info expression_checker::generate(io::location location,
                                  const ast::literal_aggregate& a) {
  const auto type = environment.check_type(a.type);
  if (const auto* array = type.get<ir::array_type>()) {
    return generate(location, a, *array);
  }
  // TODO: Implement object literals.
  io::fatal_message{location, io::message::error}
      << "unimplemented aggregate literal type.";
}

info expression_checker::generate(io::location location,
                                  const ast::array_type&) {
  io::message{location, io::message::error} << "unexpected type in expression.";
  io::fatal_message{location, io::message::note}
      << "types may only appear in expressions as part of aggregate "
         "initializers, which have the syntax `type{...}`.";
}

info expression_checker::generate(io::location location, const ast::dot& d) {
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

info expression_checker::generate(io::location location,
                                  const ast::dereference& d) {
  auto inner = generate(d.from);
  if (auto* p = inner.result->second.get<ir::pointer_type>()) {
    return {.category = info::lvalue, .result = inner.result};
  } else {
    io::fatal_message{location, io::message::error}
        << "cannot dereference expression of type " << inner.result->second
        << ".";
  }
}

info expression_checker::generate(io::location location,
                                  const ast::address_of& a) {
  auto inner = generate(a.inner);
  if (inner.category != info::lvalue) {
    io::fatal_message{location, io::message::error}
        << "cannot take the address of a temporary.";
  }
  return {.category = info::rvalue, .result = inner.result};
}

info expression_checker::generate(io::location location, const ast::index& i) {
  const auto& result =
      index(location, *generate(i.from).result, *generate(i.index).result);
  return {.category = info::lvalue, .result = &result};
}

// TODO: There is a load of duplication for the functions handling different
// arithmetic operators. Figure out a nice way of removing all the duplication.

info expression_checker::generate(io::location location, const ast::negate& n) {
  const auto& l = ensure_loaded(generate(n.inner));
  if (!is<ir::int32_type>(l.second)) {
    io::fatal_message{location, io::message::error}
        << "can't negate expression of type " << l.second << '.';
  }
  const auto& out =
      add<ir::negate>(location, {location, ir::int32_type}, l.first);
  return {.category = info::rvalue, .result = &out};
}

info expression_checker::generate(io::location location, const ast::add& a) {
  const auto& l = ensure_loaded(generate(a.left));
  const auto& r = ensure_loaded(generate(a.right));
  // TODO: Implement pointer arithmetic.
  if (!is<ir::int32_type>(l.second)) {
    io::fatal_message{a.left.location(), io::message::error}
        << "can't add expression of type " << l.second << '.';
  }
  if (!is<ir::int32_type>(r.second)) {
    io::fatal_message{a.right.location(), io::message::error}
        << "can't add expression of type " << r.second << '.';
  }
  const auto& out =
      add<ir::add>(location, {location, ir::int32_type}, l.first, r.first);
  return {.category = info::rvalue, .result = &out};
}

info expression_checker::generate(io::location location,
                                  const ast::subtract& s) {
  const auto& l = ensure_loaded(generate(s.left));
  const auto& r = ensure_loaded(generate(s.right));
  // TODO: Implement pointer arithmetic.
  if (!is<ir::int32_type>(l.second)) {
    io::fatal_message{s.left.location(), io::message::error}
        << "can't add expression of type " << l.second << '.';
  }
  if (!is<ir::int32_type>(r.second)) {
    io::fatal_message{s.right.location(), io::message::error}
        << "can't add expression of type " << r.second << '.';
  }
  const auto& out =
      add<ir::subtract>(location, {location, ir::int32_type}, l.first, r.first);
  return {.category = info::rvalue, .result = &out};
}

info expression_checker::generate(io::location location,
                                  const ast::multiply& m) {
  const auto& l = ensure_loaded(generate(m.left));
  const auto& r = ensure_loaded(generate(m.right));
  if (!is<ir::int32_type>(l.second)) {
    io::fatal_message{m.left.location(), io::message::error}
        << "can't add expression of type " << l.second << '.';
  }
  if (!is<ir::int32_type>(r.second)) {
    io::fatal_message{m.right.location(), io::message::error}
        << "can't add expression of type " << r.second << '.';
  }
  const auto& out =
      add<ir::multiply>(location, {location, ir::int32_type}, l.first, r.first);
  return {.category = info::rvalue, .result = &out};
}

info expression_checker::generate(io::location location, const ast::divide& d) {
  const auto& l = ensure_loaded(generate(d.left));
  const auto& r = ensure_loaded(generate(d.right));
  if (!is<ir::int32_type>(l.second)) {
    io::fatal_message{d.left.location(), io::message::error}
        << "can't add expression of type " << l.second << '.';
  }
  if (!is<ir::int32_type>(r.second)) {
    io::fatal_message{d.right.location(), io::message::error}
        << "can't add expression of type " << r.second << '.';
  }
  const auto& out =
      add<ir::divide>(location, {location, ir::int32_type}, l.first, r.first);
  return {.category = info::rvalue, .result = &out};
}

info expression_checker::generate(io::location location, const ast::modulo& m) {
  const auto& l = ensure_loaded(generate(m.left));
  const auto& r = ensure_loaded(generate(m.right));
  if (!is<ir::int32_type>(l.second)) {
    io::fatal_message{m.left.location(), io::message::error}
        << "can't add expression of type " << l.second << '.';
  }
  if (!is<ir::int32_type>(r.second)) {
    io::fatal_message{m.right.location(), io::message::error}
        << "can't add expression of type " << r.second << '.';
  }
  const auto& out =
      add<ir::modulo>(location, {location, ir::int32_type}, l.first, r.first);
  return {.category = info::rvalue, .result = &out};
}

info expression_checker::generate(io::location location,
                                  const ast::compare_eq& c) {
  const info l = generate(c.left);
  const info r = generate(c.right);
  const auto& ltype = effective_type(l);
  const auto& rtype = effective_type(r);
  if (ltype != rtype) {
    io::fatal_message{location, io::message::error}
        << "type mismatch in comparison: " << ltype << " vs. " << rtype << ".";
  }
  if (auto* b = ltype.get<ir::builtin_type>()) {
    const auto& l2 = ensure_loaded(l);
    const auto& r2 = ensure_loaded(r);
    switch (*b) {
      case ir::void_type:
        // void values are unconditionally equal to each other.
        return {
            .category = info::rvalue,
            .result = &add<ir::copy>(location, {location, ir::bool_type}, 1)};
      case ir::bool_type:
      case ir::int32_type:
        return {.category = info::rvalue,
                .result = &add<ir::compare_eq>(
                    location, {location, ir::bool_type}, l2.first, r2.first)};
    }
  }
  // TODO: Implement comparison of other types.
  io::fatal_message{location, io::message::error}
      << "comparison between objects of type " << ltype << " is unimplemented.";
}

info expression_checker::generate(io::location location,
                                  const ast::compare_ne& c) {
  const info l = generate(c.left);
  const info r = generate(c.right);
  const auto& ltype = effective_type(l);
  const auto& rtype = effective_type(r);
  if (ltype != rtype) {
    io::fatal_message{location, io::message::error}
        << "type mismatch in comparison: " << ltype << " vs. " << rtype << ".";
  }
  if (auto* b = ltype.get<ir::builtin_type>()) {
    const auto& l2 = ensure_loaded(l);
    const auto& r2 = ensure_loaded(r);
    switch (*b) {
      case ir::void_type:
        // void values are unconditionally equal to each other.
        return {
            .category = info::rvalue,
            .result = &add<ir::copy>(location, {location, ir::bool_type}, 0)};
      case ir::bool_type:
      case ir::int32_type:
        return {.category = info::rvalue,
                .result = &add<ir::compare_ne>(
                    location, {location, ir::bool_type}, l2.first, r2.first)};
    }
  }
  // TODO: Implement comparison of other types.
  io::fatal_message{location, io::message::error}
      << "comparison between objects of type " << ltype << " is unimplemented.";
}

info expression_checker::generate(io::location location,
                                  const ast::compare_lt& c) {
  const info l = generate(c.left);
  const info r = generate(c.right);
  const auto& ltype = effective_type(l);
  const auto& rtype = effective_type(r);
  if (ltype != rtype) {
    io::fatal_message{location, io::message::error}
        << "type mismatch in comparison: " << ltype << " vs. " << rtype << ".";
  }
  if (auto* b = ltype.get<ir::builtin_type>()) {
    const auto& l2 = ensure_loaded(l);
    const auto& r2 = ensure_loaded(r);
    switch (*b) {
      case ir::void_type:
        // void values are unconditionally equal to each other.
        return {
            .category = info::rvalue,
            .result = &add<ir::copy>(location, {location, ir::bool_type}, 0)};
      case ir::bool_type:
      case ir::int32_type:
        return {.category = info::rvalue,
                .result = &add<ir::compare_lt>(
                    location, {location, ir::bool_type}, l2.first, r2.first)};
    }
  }
  // TODO: Implement comparison of other types.
  io::fatal_message{location, io::message::error}
      << "comparison between objects of type " << ltype << " is unimplemented.";
}

info expression_checker::generate(io::location location,
                                  const ast::compare_le& c) {
  const info l = generate(c.left);
  const info r = generate(c.right);
  const auto& ltype = effective_type(l);
  const auto& rtype = effective_type(r);
  if (ltype != rtype) {
    io::fatal_message{location, io::message::error}
        << "type mismatch in comparison: " << ltype << " vs. " << rtype << ".";
  }
  if (auto* b = ltype.get<ir::builtin_type>()) {
    const auto& l2 = ensure_loaded(l);
    const auto& r2 = ensure_loaded(r);
    switch (*b) {
      case ir::void_type:
        // void values are unconditionally equal to each other.
        return {
            .category = info::rvalue,
            .result = &add<ir::copy>(location, {location, ir::bool_type}, 1)};
      case ir::bool_type:
      case ir::int32_type:
        return {.category = info::rvalue,
                .result = &add<ir::compare_le>(
                    location, {location, ir::bool_type}, l2.first, r2.first)};
    }
  }
  // TODO: Implement comparison of other types.
  io::fatal_message{location, io::message::error}
      << "comparison between objects of type " << ltype << " is unimplemented.";
}

info expression_checker::generate(io::location location,
                                  const ast::compare_gt& c) {
  const info l = generate(c.left);
  const info r = generate(c.right);
  const auto& ltype = effective_type(l);
  const auto& rtype = effective_type(r);
  if (ltype != rtype) {
    io::fatal_message{location, io::message::error}
        << "type mismatch in comparison: " << ltype << " vs. " << rtype << ".";
  }
  if (auto* b = ltype.get<ir::builtin_type>()) {
    const auto& l2 = ensure_loaded(l);
    const auto& r2 = ensure_loaded(r);
    switch (*b) {
      case ir::void_type:
        // void values are unconditionally equal to each other.
        return {
            .category = info::rvalue,
            .result = &add<ir::copy>(location, {location, ir::bool_type}, 0)};
      case ir::bool_type:
      case ir::int32_type:
        return {.category = info::rvalue,
                .result = &add<ir::compare_gt>(
                    location, {location, ir::bool_type}, l2.first, r2.first)};
    }
  }
  // TODO: Implement comparison of other types.
  io::fatal_message{location, io::message::error}
      << "comparison between objects of type " << ltype << " is unimplemented.";
}

info expression_checker::generate(io::location location,
                                  const ast::compare_ge& c) {
  const info l = generate(c.left);
  const info r = generate(c.right);
  const auto& ltype = effective_type(l);
  const auto& rtype = effective_type(r);
  if (ltype != rtype) {
    io::fatal_message{location, io::message::error}
        << "type mismatch in comparison: " << ltype << " vs. " << rtype << ".";
  }
  if (auto* b = ltype.get<ir::builtin_type>()) {
    const auto& l2 = ensure_loaded(l);
    const auto& r2 = ensure_loaded(r);
    switch (*b) {
      case ir::void_type:
        // void values are unconditionally equal to each other.
        return {
            .category = info::rvalue,
            .result = &add<ir::copy>(location, {location, ir::bool_type}, 1)};
      case ir::bool_type:
      case ir::int32_type:
        return {.category = info::rvalue,
                .result = &add<ir::compare_ge>(
                    location, {location, ir::bool_type}, l2.first, r2.first)};
    }
  }
  // TODO: Implement comparison of other types.
  io::fatal_message{location, io::message::error}
      << "comparison between objects of type " << ltype << " is unimplemented.";
}

info expression_checker::generate(io::location location,
                                  const ast::logical_and& a) {
  const auto& mem = make_stack_variable({location, ir::bool_type});
  generate_into(local(location, mem.first), location, a);
  return {.category = info::lvalue, .result = &local(location, mem.first)};
}

info expression_checker::generate(io::location location,
                                  const ast::logical_or& o) {
  const auto& mem = make_stack_variable({location, ir::bool_type});
  generate_into(local(location, mem.first), location, o);
  return {.category = info::xvalue, .result = &local(location, mem.first)};
}

info expression_checker::generate(io::location location,
                                  const ast::logical_not& n) {
  const auto inner = generate(n.inner);
  if (!is<ir::bool_type>(inner.result->second)) {
    io::fatal_message{location, io::message::error}
        << "cannot logically negate expression of type " << inner.result->second
        << ".";
  }
  const auto& result = add<ir::logical_not>(location, {location, ir::bool_type},
                                            inner.result->first);
  return {.category = info::rvalue, .result = &result};
}

info expression_checker::generate(io::location location, const ast::call& c) {
  generate(c.callee);
  for (const auto& argument : c.arguments) generate(argument);
  // TODO: Implement function calls. This will require deciding how to pass each
  // value type. Probably a simple option is to pass all builtins in registers
  // and all aggregate types by reference.
  io::fatal_message{location, io::message::error}
      << "function calls are unimplemented.";
}

info expression_checker::generate(const ast::expression& e) {
  return e.visit([&](const auto& x) { return generate(e.location(), x); });
}

void expression_checker::generate_into(const variable_info& address,
                                       io::location location,
                                       const ast::literal_aggregate& a,
                                       const ir::array_type& array) {
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
    if (auto* f =
            std::get_if<ast::literal_aggregate::field_assignment>(&argument)) {
      io::fatal_message{f->value.location(), io::message::error}
          << "cannot have field assignments in an array literal.";
    }
    if (auto* i =
            std::get_if<ast::literal_aggregate::index_assignment>(&argument)) {
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
      const auto& target = index(
          argument.location(), address,
          add<ir::copy>(location, {argument.location(), ir::int32_type}, i));
      generate_into(target, argument);
    }
    return;
  }
  // TODO: Implement designated array literals, e.g. `[256]bool{[42] = true}`.
  io::fatal_message{a.type.location(), io::message::error}
      << "unimplemented array literal type.";
}

void expression_checker::generate_into(const variable_info& address,
                                       io::location location,
                                       const ast::literal_aggregate& a) {
  const auto type = environment.check_type(a.type);
  if (const auto* array = type.get<ir::array_type>()) {
    return generate_into(address, location, a, *array);
  }
  // TODO: Implement object literals.
  io::fatal_message{location, io::message::error}
      << "unimplemented aggregate literal type.";
}

void expression_checker::generate_into(const variable_info& address,
                                       io::location location,
                                       const ast::logical_and& a) {
  const auto end = ir::make_symbol();
  generate_into(address, a.left);
  const auto& value = add<ir::logical_not>(location, {location, ir::bool_type},
                                           load(address).first);
  step<ir::conditional_jump>(location, value.first, end);
  generate_into(address, a.right);
  step<ir::label>(location, end);
}

void expression_checker::generate_into(const variable_info& address,
                                       io::location location,
                                       const ast::logical_or& o) {
  const auto end = ir::make_symbol();
  generate_into(address, o.left);
  step<ir::conditional_jump>(location, load(address).first, end);
  generate_into(address, o.right);
  step<ir::label>(location, end);
}

void expression_checker::generate_into(const variable_info& address,
                                       const ast::expression& e) {
  return e.visit(
      [&](const auto& x) { generate_into(address, e.location(), x); });
}

void function_checker::check(io::location l,
                             const ast::function_definition& f) {
  // TODO: Implement functions with actual inputs and outputs.
  if (!is<ir::void_type>(type.return_type) || !type.parameters.empty()) {
    io::fatal_message{l, io::message::error}
        << "functions with parameters or with a non-void return type are "
           "unimplemented.";
  }
  generate(module.environment, l, f.body);
}

void function_checker::generate(environment&, io::location l,
                                const ast::import_statement&) {
  io::fatal_message{l, io::message::error}
      << "import statements may not appear inside functions.";
}

// TODO: Some of these functions look very similar to the function in
// module_checker, maybe they can be merged together somehow.

void function_checker::generate(environment& environment, io::location l,
                                const ast::variable_definition& v) {
  const auto type = environment.check_type(v.type);
  const auto& address = make_stack_variable(type);
  environment.define(l, v.id.value, variable{address.first, type});
  if (v.initializer) {
    expression_checker{{program, result}, environment}.generate_into(
        local(l, address.first), *v.initializer);
  }
}

void function_checker::generate(environment& environment, io::location l,
                                const ast::alias_definition& a) {
  environment.define(l, a.id.value, type_type{environment.check_type(a.type)});
}

void function_checker::generate(environment&, io::location l,
                                const ast::function_definition&) {
  io::fatal_message{l, io::message::error}
      << "function definitions may not appear inside other functions.";
}

void function_checker::generate(environment&, io::location l,
                                const ast::class_definition&) {
  // TODO: There's no real reason to forbid local classes, so this should be
  // implemented.
  io::fatal_message{l, io::message::error}
      << "class definitions may not appear inside other functions.";
}

void function_checker::generate(environment& environment, io::location l,
                                const ast::definition& d) {
  d.visit([&](const auto& x) { generate(environment, l, x); });
}

void function_checker::generate(environment&, io::location l,
                                const ast::exported_definition&) {
  io::fatal_message{l, io::message::error}
      << "exports may not appear inside functions.";
}

void function_checker::generate(environment& environment, io::location l,
                                const ast::assignment& a) {
  expression_checker checker{{program, result}, environment};
  const info lhs = checker.generate(a.destination);
  if (lhs.category != info::lvalue) {
    io::fatal_message{l, io::message::error}
        << "only lvalue expressions can be assigned to.";
  }
  checker.generate_into(*lhs.result, a.value);
}

void function_checker::generate(environment& environment, io::location l,
                                const ast::if_statement& i) {
  expression_checker checker{{program, result}, environment};
  const auto& condition = ensure_loaded(checker.generate(i.condition));
  const auto& inverse =
      add<ir::logical_not>(l, {l, ir::bool_type}, condition.first);
  const ir::symbol end = ir::make_symbol();
  if (i.else_branch) {
    const ir::symbol else_branch = ir::make_symbol();
    step<ir::conditional_jump>(l, inverse.first, else_branch);
    generate(environment, i.then_branch);
    step<ir::jump>(l, end);
    step<ir::label>(l, else_branch);
    generate(environment, *i.else_branch);
  } else {
    step<ir::conditional_jump>(l, inverse.first, end);
    generate(environment, i.then_branch);
  }
  step<ir::label>(l, end);
}

void function_checker::generate(environment& outer, io::location l,
                                const ast::while_statement& w) {
  const ir::symbol while_condition = ir::make_symbol();
  step<ir::jump>(l, while_condition);
  const ir::symbol while_body = ir::make_symbol();
  const ir::symbol while_end = ir::make_symbol();
  step<ir::label>(l, while_body);
  environment inner{&outer};
  inner.break_label = while_end;
  inner.continue_label = while_condition;
  generate(inner, w.body);
  step<ir::label>(l, while_condition);
  expression_checker checker{{program, result}, outer};
  const auto& condition = ensure_loaded(checker.generate(w.condition));
  step<ir::conditional_jump>(l, condition.first, while_body);
  step<ir::label>(l, while_end);
}

void function_checker::generate(environment& environment, io::location l,
                                const ast::break_statement&) {
  auto label = environment.lookup_break();
  if (!label) {
    io::fatal_message{l, io::message::error} << "cannot break here.";
  }
  step<ir::jump>(l, *label);
}

void function_checker::generate(environment& environment, io::location l,
                                const ast::continue_statement&) {
  auto label = environment.lookup_continue();
  if (!label) {
    io::fatal_message{l, io::message::error} << "cannot continue here.";
  }
  step<ir::jump>(l, *label);
}

void function_checker::generate(environment& environment, io::location l,
                                const ast::return_statement& r) {
  // TODO: Implement return statements for functions with non-void return
  // types.
  assert(is<ir::void_type>(type.return_type));
  if (r.value) {
    expression_checker checker{{program, result}, environment};
    const info value = checker.generate(*r.value);
    io::fatal_message{l, io::message::error}
        << "cannot return object of type " << value.result->second
        << " from function returning " << ir::void_type << ".";
  }
  step<ir::ret>(l);
}

void function_checker::generate(environment& environment, io::location,
                                const ast::expression_statement& e) {
  expression_checker{{program, result}, environment}.generate(e.expression);
}

void function_checker::generate(environment& outer, io::location,
                                const ast::block_statement& b) {
  environment inner{&outer};
  for (const auto& statement : b.statements) generate(inner, statement);
}

void function_checker::generate(environment& environment,
                                const ast::statement& s) {
  s.visit([&](const auto& x) { generate(environment, s.location(), x); });
}

export ir::program check(const char* filename) {
  checker checker;
  checker.get_module(std::filesystem::absolute(filename));
  return {
      .initialization = std::move(checker.initialization),
      .functions = std::move(checker.functions),
  };
}

}  // namespace syntax
