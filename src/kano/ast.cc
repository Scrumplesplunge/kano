export module kano.ast;

import io;
import <map>;
import <memory>;
import <set>;
export import <variant>;
export import <vector>;

export namespace kano::ast {

namespace types {

struct name { std::string_view value; };
struct pointer;

using type_type = std::variant<name, pointer>;
struct type {
  template <typename T> type(io::location location, T&& value);
  io::location location;
  std::unique_ptr<type_type> value;
};

struct pointer { type pointee; };

template <typename T>
type::type(io::location location, T&& value)
    : location(location), value(new type_type{std::forward<T>(value)}) {}

}  // namespace types

// Note: index of variant values matches builtin values.
using literal = std::variant<std::int32_t, std::string>;
struct name { std::string_view value; };
struct dereference;
struct address_of;
struct add;
struct sub;
struct cmp_eq;
struct cmp_lt;
struct call;
using expression_type = std::variant<literal, name, dereference, address_of,
                                     add, sub, cmp_eq, cmp_lt, call>;
struct expression {
  template <typename T> expression(io::location, T&& value);
  io::location location;
  std::unique_ptr<expression_type> value;
};

struct unop {
  unop(expression inner) : inner(std::move(inner)) {}
  expression inner;
};

struct dereference : unop { using unop::unop; };
struct address_of : unop { using unop::unop; };

struct binop {
  binop(expression left, expression right)
      : left(std::move(left)), right(std::move(right)) {}
  expression left, right;
};

struct add : binop { using binop::binop; };
struct sub : binop { using binop::binop; };
struct cmp_eq : binop { using binop::binop; };
struct cmp_lt : binop { using binop::binop; };

struct call {
  expression callee;
  std::vector<expression> arguments;
};

template <typename T>
expression::expression(io::location location, T&& value)
    : location(location), value(new expression_type{std::forward<T>(value)}) {}

struct statement;
struct variable_declaration;
struct assignment;
struct if_statement;
struct while_statement;
struct break_statement {};
struct continue_statement {};
struct return_statement { std::optional<expression> value = {}; };
struct call_statement : call {};
using statement_type =
    std::variant<variable_declaration, assignment, if_statement,
                 while_statement, break_statement, continue_statement,
                 return_statement, call_statement, std::vector<statement>>;
struct statement {
  statement();
  template <typename T> statement(io::location, T&& value);
  io::location location;
  std::unique_ptr<statement_type> value;
};

struct variable_declaration {
  std::string_view name;
  types::type type;
  std::optional<expression> initializer = {};
};

struct assignment { expression destination, value; };

struct if_statement {
  expression condition;
  statement then_branch;
  std::optional<statement> else_branch;
};

struct while_statement {
  expression condition;
  statement body;
};

statement::statement() : statement({}, std::vector<statement>{}) {}

template <typename T>
statement::statement(io::location location, T&& value)
    : location(location), value(new statement_type{std::move(value)}) {}

struct function {
  std::string_view name;
  types::type return_type;
  std::vector<variable_declaration> parameters;
  std::vector<statement> body;
};

struct definition {
  io::location location;
  std::variant<variable_declaration, function> value;
};

struct module {
  std::string filename;
  std::vector<definition> definitions;
};

}  // namespace kano::ast
