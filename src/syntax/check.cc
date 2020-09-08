// Check Kano source code, producing Kano IR.

module;

#include <cassert>

export module syntax.check;

import syntax.parse;
export import semantics.ir;
import <iostream>;
import <filesystem>;

namespace syntax {

namespace ir = ::semantics::ir;

using local_info = std::pair<const ir::local, ir::data_type>;

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

struct local {
  const local_info* address;
  ir::data_type type;
};

// Used for names which represent types instead of names which represent values
// of a given type.
struct type_type {
  ir::data_type type;
};

using name_type = std::variant<function, module_type, global, local, type_type>;

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

  const local_info& add(ir::data_type, ir::action);
  const local_info& add(ir::value);
  const local_info& alloc(ir::data_type);
  // Given a pointer to an indexable object (i.e. [n]T) and an index i, produce
  // a pointer to the ith element of the object.
  const local_info& index(io::location, const local_info&, const local_info&);
  const local_info& ensure_loaded(const info&);
  const local_info& load(const local_info&);
  void label(ir::symbol);
  void jump(io::location, ir::symbol);
  void conditional_jump(io::location, const local_info&, ir::symbol);
  void construct_into(const local_info&, const info&);
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

  // Like generate, but instead of generating the value into a local, generate
  // and store the value at the given address.
  void generate_into(const local_info&, io::location,
                     const ast::literal_aggregate&, const ir::array_type&);
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
  ir::local next_local = {};
  ir::function initialization = {};
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

  // Generate a new unique symbol for some exported artefact.
  ir::symbol symbol();

  // Generate a new unique id for some local variable.
  ir::local local();

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
  // local into a single type that uniquely identifies "things", but possibly
  // that is more bug-prone.
  std::map<ir::symbol, const local_info*> locals = {};

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

ir::symbol checker::symbol() {
  const auto result = next_symbol;
  next_symbol = ir::symbol((int)next_symbol + 1);
  return result;
}

ir::local checker::local() {
  const auto result = next_local;
  next_local = ir::local((int)next_local + 1);
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

const type_info& checker::lookup_type(const ir::data_type& d) {
  const auto [i, is_new] =
      type_by_structure.emplace(d, ir::symbol::none);
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
  environment.define(l, i.path.back(), module_type{&m.exports});
}

const environment::name_info& module_checker::check(
    io::location l, const ast::variable_definition& v) {
  const auto type = environment.check_type(v.type);
  const ir::symbol id = program.symbol();
  const auto& info = environment.define(l, v.id.value, global{id, type});
  if (v.initializer) {
    const auto& lhs = initialization.add({l, ir::pointer{id, type}});
    initialization.generate_into(lhs, *v.initializer);
  }
  return info;
}

const environment::name_info& module_checker::check(io::location l,
                                            const ast::alias_definition& a) {
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
  const auto& info = environment.define(
      l, f.id.value,
      function{program.symbol(),
               {std::move(return_type), std::move(parameters)}});
  function_checker checker{
      {program, {}}, *this, std::get<function>(info.type).type};
  checker.check(l, f);
  return info;
}

const environment::name_info& module_checker::check(
    io::location l, const ast::class_definition& c) {
  // TODO: Put the body of the class aside for subsequent checking after all
  // top-level declarations have been handled.
  const auto symbol = program.symbol();
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
    case info::rvalue: return info.result->second;
    case info::lvalue:
    case info::xvalue: {
      const auto* p = info.result->second.get<ir::pointer_type>();
      assert(p);
      return p->pointee;
    }
  }
}

template <typename function>
const local_info& function_builder<function>::add(ir::data_type type,
                                                  ir::action action) {
  const auto id = program.local();
  const auto [i, is_new] = result.locals.emplace(id, std::move(type));
  result.steps.emplace_back(ir::step{id, std::move(action)});
  return *i;
}

template <typename function>
const local_info& function_builder<function>::add(ir::value value) {
  const auto location = value.location();
  auto type = type_of(value);
  return add(std::move(type), {location, ir::constant{std::move(value)}});
}

template <typename function>
const local_info& function_builder<function>::alloc(ir::data_type type) {
  return add({type.location(), ir::pointer_type{type}},
             {type.location(), ir::stack_allocate{}});
}

template <typename function>
const local_info& function_builder<function>::index(io::location location,
                                                    const local_info& address,
                                                    const local_info& offset) {
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
  return add({location, ir::pointer_type{a->element}},
             {location, ir::index{address.first, offset.first}});
}

template <typename function>
const local_info& function_builder<function>::ensure_loaded(const info& x) {
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
const local_info& function_builder<function>::load(const local_info& address) {
  const auto& [a, type] = address;
  if (auto* p = type.get<ir::pointer_type>()) {
    return add(p->pointee, {type.location(), ir::load{a}});
  } else {
    io::fatal_message{type.location(), io::message::error}
        << "cannot load from expression of type " << type << ".";
  }
}

template <typename function>
void function_builder<function>::label(ir::symbol s) {
  auto [i, is_new] = result.labels.emplace(s, result.steps.size());
  assert(is_new);
}

template <typename function>
void function_builder<function>::jump(io::location location,
                                      ir::symbol target) {
  add({location, ir::void_type}, {location, ir::jump{target}});
}

template <typename function>
void function_builder<function>::conditional_jump(io::location location,
                                                  const local_info& condition,
                                                  ir::symbol target) {
  assert(is<ir::bool_type>(condition.second));
  add({location, ir::void_type},
      {location, ir::conditional_jump{condition.first, target}});
}

template <typename function>
void function_builder<function>::construct_into(const local_info& address,
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
      add(p->pointee, {source_type.location(), ir::store{destination, source}});
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
      add({destination_type.location(),
           ir::function_pointer{type_info.copy, std::move(copy_type)}});
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
      add({destination_type.location(),
           ir::function_pointer{type_info.move, std::move(move_type)}});
      break;
    }
  }
}

info expression_checker::generate(io::location location,
                                  const environment::name_info& info) {
  if (const auto* f = std::get_if<function>(&info.type)) {
    const auto& result =
        add({location, ir::function_pointer{f->symbol, f->type}});
    return {.category = info::lvalue, .result = &result};
  }
  if (const auto* g = std::get_if<global>(&info.type)) {
    const auto& result = add({location, ir::pointer{g->symbol, g->type}});
    return {.category = info::lvalue, .result = &result};
  }
  if (const auto* l = std::get_if<local>(&info.type)) {
    return {.category = info::lvalue, .result = l->address};
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
  const auto& result = add({location, std::int32_t(i.value)});
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
  const auto& mem = alloc({location, array});
  generate_into(mem, location, a, array);
  return {.category = info::xvalue, .result = &mem};
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
      add({location, ir::int32_type}, {location, ir::negate{l.first}});
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
      add({location, ir::int32_type}, {location, ir::add{l.first, r.first}});
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
  const auto& out = add({location, ir::int32_type},
                        {location, ir::subtract{l.first, r.first}});
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
  const auto& out = add({location, ir::int32_type},
                        {location, ir::multiply{l.first, r.first}});
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
      add({location, ir::int32_type}, {location, ir::divide{l.first, r.first}});
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
      add({location, ir::int32_type}, {location, ir::modulo{l.first, r.first}});
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
        return {.category = info::rvalue, .result = &add({location, true})};
      case ir::bool_type:
      case ir::int32_type:
        return {.category = info::rvalue,
                .result = &add({location, ir::bool_type},
                               {location, ir::compare_eq{l2.first, r2.first}})};
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
        return {.category = info::rvalue, .result = &add({location, true})};
      case ir::bool_type:
      case ir::int32_type:
        return {.category = info::rvalue,
                .result = &add({location, ir::bool_type},
                               {location, ir::compare_ne{l2.first, r2.first}})};
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
        return {.category = info::rvalue, .result = &add({location, true})};
      case ir::bool_type:
      case ir::int32_type:
        return {.category = info::rvalue,
                .result = &add({location, ir::bool_type},
                               {location, ir::compare_lt{l2.first, r2.first}})};
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
        return {.category = info::rvalue, .result = &add({location, true})};
      case ir::bool_type:
      case ir::int32_type:
        return {.category = info::rvalue,
                .result = &add({location, ir::bool_type},
                               {location, ir::compare_le{l2.first, r2.first}})};
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
        return {.category = info::rvalue, .result = &add({location, true})};
      case ir::bool_type:
      case ir::int32_type:
        return {.category = info::rvalue,
                .result = &add({location, ir::bool_type},
                               {location, ir::compare_gt{l2.first, r2.first}})};
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
        return {.category = info::rvalue, .result = &add({location, true})};
      case ir::bool_type:
      case ir::int32_type:
        return {.category = info::rvalue,
                .result = &add({location, ir::bool_type},
                               {location, ir::compare_ge{l2.first, r2.first}})};
    }
  }
  // TODO: Implement comparison of other types.
  io::fatal_message{location, io::message::error}
      << "comparison between objects of type " << ltype << " is unimplemented.";
}

info expression_checker::generate(io::location location,
                                  const ast::logical_and& a) {
  const auto& mem = alloc({location, ir::bool_type});
  generate_into(mem, location, a);
  return {.category = info::lvalue, .result = &mem};
}

info expression_checker::generate(io::location location,
                                  const ast::logical_or& o) {
  const auto& mem = alloc({location, ir::bool_type});
  generate_into(mem, location, o);
  return {.category = info::xvalue, .result = &mem};
}

info expression_checker::generate(io::location location,
                                  const ast::logical_not& n) {
  const auto inner = generate(n.inner);
  if (!is<ir::bool_type>(inner.result->second)) {
    io::fatal_message{location, io::message::error}
        << "cannot logically negate expression of type " << inner.result->second
        << ".";
  }
  const auto& result = add({location, ir::bool_type},
                           {location, ir::logical_not{inner.result->first}});
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

void expression_checker::generate_into(const local_info& address,
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
  if (const auto* array = type.get<ir::array_type>()) {
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
  const auto& value = add({location, ir::bool_type},
                          {location, ir::logical_not{load(address).first}});
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
  const auto& address = alloc(type);
  environment.define(l, v.id.value, local{&address, type});
  if (v.initializer) {
    expression_checker{{program, result}, environment}.generate_into(
        address, *v.initializer);
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
      add({l, ir::bool_type}, {l, ir::logical_not{condition.first}});
  const ir::symbol end = program.symbol();
  if (i.else_branch) {
    const ir::symbol else_branch = program.symbol();
    conditional_jump(l, inverse, else_branch);
    generate(environment, i.then_branch);
    jump(l, end);
    label(else_branch);
    generate(environment, *i.else_branch);
  } else {
    conditional_jump(l, inverse, end);
    generate(environment, i.then_branch);
  }
  label(end);
}

void function_checker::generate(environment& outer, io::location l,
                                const ast::while_statement& w) {
  const ir::symbol while_condition = program.symbol();
  jump(l, while_condition);
  const ir::symbol while_body = program.symbol();
  const ir::symbol while_end = program.symbol();
  label(while_body);
  environment inner{&outer};
  inner.break_label = while_end;
  inner.continue_label = while_condition;
  generate(inner, w.body);
  label(while_condition);
  expression_checker checker{{program, result}, outer};
  const auto& condition = ensure_loaded(checker.generate(w.condition));
  conditional_jump(l, condition, while_body);
  label(while_end);
}

void function_checker::generate(environment& environment, io::location l,
                                const ast::break_statement&) {
  auto label = environment.lookup_break();
  if (!label) {
    io::fatal_message{l, io::message::error} << "cannot break here.";
  }
  jump(l, *label);
}

void function_checker::generate(environment& environment, io::location l,
                                const ast::continue_statement&) {
  auto label = environment.lookup_continue();
  if (!label) {
    io::fatal_message{l, io::message::error} << "cannot continue here.";
  }
  jump(l, *label);
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
  add({l, ir::void_type}, {l, ir::ret{}});
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

export void check(const char* filename) {
  checker{}.get_module(std::filesystem::absolute(filename));
}

}  // namespace syntax
