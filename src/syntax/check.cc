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

struct type_info {
  // function load(address : *T) : T { ... }
  semantics::ir::symbol load;
  // function store(address : *T, value : T) { ... }
  semantics::ir::symbol store;
};

struct checker {
  semantics::ir::symbol next_symbol = semantics::ir::symbol::first_user_symbol;
  semantics::ir::local next_local = {};
  std::map<std::filesystem::path, module_data> modules;
  // Map from structural type to its symbolic name. This is used for looking up
  // operators for moving values of this type around.
  std::map<semantics::ir::data_type, semantics::ir::symbol> type_by_structure;
  // Map from symbolic type names to the table of operators for the type.
  std::map<semantics::ir::symbol, type_info> types = {
      {semantics::ir::symbol::builtin_void,
       {.load = semantics::ir::symbol::builtin_void_load,
        .store = semantics::ir::symbol::builtin_void_store}},
      {semantics::ir::symbol::builtin_bool,
       {.load = semantics::ir::symbol::builtin_bool_load,
        .store = semantics::ir::symbol::builtin_bool_store}},
      {semantics::ir::symbol::builtin_int32,
       {.load = semantics::ir::symbol::builtin_int32_load,
        .store = semantics::ir::symbol::builtin_int32_store}},
  };
  environment builtins = {
      .names =
          {
              {"bool",
               {"builtin",
                {},
                semantics::ir::symbol::builtin_bool,
                "bool",
                type_type{{{}, semantics::ir::builtin_type::bool_type}}}},
              {"int32",
               {"builtin",
                {},
                semantics::ir::symbol::builtin_int32,
                "int32",
                type_type{{{}, semantics::ir::builtin_type::int32_type}}}},
              {"void",
               {"builtin",
                {},
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

  semantics::ir::data_type check_type(const ast::expression&);

  semantics::ir::expression check_expression(io::location,
                                             const ast::identifier&);
  semantics::ir::expression check_expression(const ast::expression&);

  const environment::name_info& resolve(const ast::expression&);
};

struct expression_checker {
  module_checker& module;
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

  const local_info& add(semantics::ir::data_type, semantics::ir::action);
  const local_info& add(semantics::ir::value);
  const local_info& alloc(semantics::ir::data_type);
  const local_info& index(const local_info&, const local_info&);
  const local_info& load(const local_info&);
  void store(const local_info&, const local_info&);

  info generate(io::location, const environment::name_info&);
  info generate(io::location, const ast::identifier&);
  info generate(io::location, const ast::literal_integer&);
  info generate(io::location, const ast::literal_string&);
  info generate(io::location, const ast::literal_aggregate&,
                const semantics::ir::array_type&);
  info generate(io::location, const ast::literal_aggregate&);
  info generate(io::location, const ast::array_type&);
  info generate(io::location, const ast::dot&);
  template <typename T>
  info generate(io::location l, const T&) {
    io::fatal_message{module.name(), l, io::message::error}
        << "unimplemented expression type.";
  }
  info generate(const ast::expression&);

  // Like generate, but instead of generating the value into a local, generate
  // and store the value at the given address.
  void generate_into(const local_info&, io::location,
                     const ast::literal_integer&);
  void generate_into(const local_info&, io::location,
                     const ast::literal_aggregate&,
                     const semantics::ir::array_type&);
  template <typename T>
  void generate_into(const local_info&, io::location l, const T&) {
    io::fatal_message{module.name(), l, io::message::error}
        << "unimplemented expression type.";
  }
  void generate_into(const local_info&, const ast::expression&);
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
  // TODO: Generate inline functions for each of these operations.
  type_info info = {
    .load = symbol(),
    .store = symbol(),
  };
  return types.emplace(i->second, info).first->second;
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
  const auto& info =
      environment.define(*this, l, v.id.value, global{check_type(v.type)});
  if (v.initializer) {
    expression_checker checker{*this};
    checker.generate(*v.initializer);
    // TODO: Consume the resulting expression.
  }
  return info;
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

semantics::ir::expression module_checker::check_expression(
    io::location l, const ast::identifier& i) {
  const auto& info = environment.lookup(*this, l, i.value);
  if (const auto* type = std::get_if<global>(&info.type)) {

  }
  io::fatal_message{name(), l, io::message::error}
      << "unimplemented.";
}

semantics::ir::expression module_checker::check_expression(
    const ast::expression& e) {
  io::fatal_message{name(), e.location(), io::message::error}
      << "unimplemented.";
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

const expression_checker::local_info& expression_checker::add(
    semantics::ir::data_type type, semantics::ir::action action) {
  const auto id = module.program.local();
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
    const local_info& address, const local_info& offset) {
  if (offset.second !=
      semantics::ir::data_type{{}, semantics::ir::builtin_type::int32_type}) {
    io::fatal_message{module.name(), offset.second.location(),
                      io::message::error}
        << "index offset must be integral.";
  }
  if (auto* p = address.second.get<semantics::ir::pointer_type>()) {
    return add(address.second,
               {offset.second.location(),
                semantics::ir::index{address.first, offset.first}});
  }
  if (auto* a = address.second.get<semantics::ir::array_type>()) {
    return add(
        {address.second.location(), semantics::ir::pointer_type{a->element}},
        {offset.second.location(),
         semantics::ir::index{address.first, offset.first}});
  }
  io::fatal_message{module.name(), offset.second.location(),
                    io::message::error}
      << "index address must be either an array type or a pointer type.";
}

const expression_checker::local_info& expression_checker::load(
    const local_info& address) {
  const auto& [a, type] = address;
  if (auto* p = type.get<semantics::ir::pointer_type>()) {
    return add(p->pointee, {type.location(), semantics::ir::load{a}});
  } else {
    io::fatal_message{module.name(), type.location(), io::message::error}
        << "cannot load from expression of type " << type << ".";
  }
}

void expression_checker::store(const local_info& address,
                               const local_info& value) {
  const auto& [source, source_type] = value;
  const auto& [destination, destination_type] = address;
  if (auto* p = destination_type.get<semantics::ir::pointer_type>()) {
    if (p->pointee != source_type) {
      io::fatal_message{module.name(), destination_type.location(),
                        io::message::error}
          << "cannot store expression of type " << source_type
          << " to address expression of type " << destination_type << ".";
    }
    add(p->pointee,
        {source_type.location(), semantics::ir::store{destination, source}});
  } else {
    io::fatal_message{module.name(), destination_type.location(),
                      io::message::error}
        << "cannot store to address expression of type " << destination_type
        << ".";
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
    io::fatal_message{module.name(), location, io::message::error}
        << "local variables are unimplemented.";
  }
  if (const auto* t = std::get_if<type_type>(&info.type)) {
    io::message{module.name(), location, io::message::error}
        << "a type may not appear in an expression.";
    io::fatal_message{module.name(), location, io::message::note}
        << "a value literal would have the syntax `type{...}`.";
  }
  io::fatal_message{module.name(), location, io::message::error}
      << "unimplemented name type.";
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::identifier& i) {
  // In the IR, we will represent variable references as pointers with an lvalue
  // category.
  return generate(location,
                  module.environment.lookup(module, location, i.value));
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::literal_integer& i) {
  if (i.value > std::numeric_limits<std::int32_t>::max()) {
    io::fatal_message{module.name(), location, io::message::error}
        << "integer literal exceeds the maximum allowed value for int32.";
  }
  const auto& result = add({location, std::int32_t(i.value)});
  return {.category = info::rvalue, .result = &result};
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::literal_string&) {
  io::fatal_message{module.name(), location, io::message::error}
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
  const auto type = module.check_type(a.type);
  if (const auto* array = type.get<semantics::ir::array_type>()) {
    return generate(location, a, *array);
  }
  io::fatal_message{module.name(), location, io::message::error}
      << "unimplemented aggregate literal type.";
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::array_type&) {
  io::message{module.name(), location, io::message::error}
      << "unexpected type in expression.";
  io::fatal_message{module.name(), location, io::message::note}
      << "types may only appear in expressions as part of aggregate "
         "initializers, which have the syntax `type{...}`.";
}

expression_checker::info expression_checker::generate(
    io::location location, const ast::dot& d) {
  // Normally, `<expr>.bar` is indirection into a class. As a special case,
  // `foo.bar` may instead mean accessing the name `bar` from the imported
  // module `some.path.foo`.
  if (auto* i = d.from.get<ast::identifier>()) {
    const auto& lhs = module.environment.lookup(module, location, i->value);
    if (const auto* m = std::get_if<module_type>(&lhs.type)) {
      return generate(location,
                      m->exports->lookup(module, location, d.id.value));
    }
  }
  // TODO: Implement access into objects.
  io::fatal_message{module.name(), location, io::message::error}
      << "object access is unimplemented.";
}

expression_checker::info expression_checker::generate(
    const ast::expression& e) {
  return e.visit([&](const auto& x) { return generate(e.location(), x); });
}

void expression_checker::generate_into(const local_info& address,
                                       io::location location,
                                       const ast::literal_integer& i) {
  store(address, *generate(location, i).result);
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
        io::fatal_message{module.name(), e->location(), io::message::error}
            << "cannot mix bare expressions and indexed expressions in an "
               "array literal.";
      }
      has_bare = true;
      continue;
    }
    if (auto* f = std::get_if<ast::literal_aggregate::field_assignment>(
            &argument)) {
      io::fatal_message{module.name(), f->value.location(),
                        io::message::error}
          << "cannot have field assignments in an array literal.";
    }
    if (auto* i = std::get_if<ast::literal_aggregate::index_assignment>(
            &argument)) {
      if (has_bare) {
        io::fatal_message{module.name(), i->value.location(),
                          io::message::error}
            << "cannot mix bare expressions and indexed expressions in an "
               "array literal.";
      }
      has_index = true;
    }
  }
  assert(!has_index || !has_bare);
  if (has_bare) {
    if (a.arguments.size() != array.size) {
      io::message{module.name(), location, io::message::error}
          << "unindexed array literals must initialize every value.";
      io::fatal_message{module.name(), a.type.location(), io::message::note}
          << "expected " << array.size << " initializers but got "
          << a.arguments.size() << ".";
    }
    for (std::int32_t i = 0, n = a.arguments.size(); i < n; i++) {
      const auto& argument = std::get<ast::expression>(a.arguments[i]);
      const auto& target = index(address, add({argument.location(), i}));
      generate_into(target, argument);
    }
    return;
  }
  io::fatal_message{module.name(), a.type.location(), io::message::error}
      << "unimplemented array literal type.";
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
