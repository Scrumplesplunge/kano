// AST for Kano source code. This is a representation of the syntactic version
// of Kano code, so it does not model the formal semantics and doesn't do any
// desugaring.

export module syntax.ast;

import io;
export import node;
export import <optional>;
export import <vector>;

export namespace syntax::ast {

struct identifier;
struct literal_integer;
struct literal_string;
struct literal_aggregate;
struct array_type;
struct dot;
struct dereference;
struct address_of;
struct index;
struct negate;
struct add;
struct subtract;
struct multiply;
struct divide;
struct modulo;
struct compare_eq;
struct compare_ne;
struct compare_lt;
struct compare_le;
struct compare_gt;
struct compare_ge;
struct logical_and;
struct logical_or;
struct logical_not;
struct call;

using expression =
    node<identifier, literal_integer, literal_string, literal_aggregate,
         array_type, dot, dereference, address_of, index, negate, add, subtract,
         multiply, divide, modulo, compare_eq, compare_ne, compare_lt,
         compare_le, compare_gt, compare_ge, logical_and, logical_or,
         logical_not, call>;

// A single atomic identifier name, ambiguous without context.
struct identifier {
  std::string value;
};

// A literal integer value. All such values in the source are positive, as
// the leading '-' is parsed as a `negate` node for negative numbers.
struct literal_integer {
  std::uint64_t value;
};

// A literal aggregate, such as an array or struct literal.
// `[]int{1, 2, 3}` or `mymodule.mystruct{.x = 4}`.
struct literal_aggregate {
  // `[a] = b`.
  struct index_assignment {
    expression index, value;
  };
  // `.foo = a`.
  struct field_assignment {
    identifier field;
    expression value;
  };
  using element_type =
      std::variant<expression, index_assignment, field_assignment>;
  expression type;
  std::vector<element_type> arguments;
};

// A literal string value, with escape sequences evaluated.
struct literal_string {
  std::string value;
};

// `[4]int`.
struct array_type {
  expression size, element;
};

// `x[42].foo` or `mymodule.x`.
struct dot {
  expression from;
  identifier id;
};

// `*x`. Could be a type or a value.
struct dereference {
  expression from;
};

// `&x`.
struct address_of {
  expression inner;
};

// `x[42]`.
struct index {
  expression from, index;
};

// `-x`.
struct negate {
  expression inner;
};

// `a + b`.
struct add {
  expression left, right;
};

// `a - b`.
struct subtract {
  expression left, right;
};

// `a * b`.
struct multiply {
  expression left, right;
};

// `a / b`.
struct divide {
  expression left, right;
};

// `a % b`.
struct modulo {
  expression left, right;
};

// `a == b`.
struct compare_eq {
  expression left, right;
};

// `a != b`.
struct compare_ne {
  expression left, right;
};

// `a < b`.
struct compare_lt {
  expression left, right;
};

// `a <= b`.
struct compare_le {
  expression left, right;
};

// `a > b`.
struct compare_gt {
  expression left, right;
};

// `a >= b`.
struct compare_ge {
  expression left, right;
};

// `a && b`.
struct logical_and {
  expression left, right;
};

// `a || b`.
struct logical_or {
  expression left, right;
};

// `!a`.
struct logical_not {
  expression inner;
};

// `f(a, b)`.
struct call {
  expression callee;
  std::vector<expression> arguments;
};

struct import_statement;
struct variable_definition;
struct alias_definition;
struct function_definition;
struct exported_definition;
struct class_definition;
struct assignment;
struct if_statement;
struct while_statement;
struct break_statement;
struct continue_statement;
struct return_statement;
struct expression_statement;
struct block_statement;

using definition = node<variable_definition, alias_definition,
                        function_definition, class_definition>;
using statement =
    node<import_statement, definition, exported_definition, assignment,
         if_statement, while_statement, break_statement, continue_statement,
         return_statement, expression_statement, block_statement>;

// `import foo.bar;`
struct import_statement {
  std::vector<std::string> path;
};

// `x = 42;`.
struct assignment {
  expression destination, value;
};

// `if (x) ...`, `if (x) ... else ...`.
struct if_statement {
  expression condition;
  statement then_branch;
  std::optional<statement> else_branch;
};

// `while (x) ...`.
struct while_statement {
  expression condition;
  statement body;
};

// `break;`.
struct break_statement {};

// `continue;`.
struct continue_statement {};

// `return;` or `return x;`.
struct return_statement {
  std::optional<expression> value;
};

// `expr;`. Mostly useful for call expressions, others should have warnings.
struct expression_statement {
  expression expression;
};

// `{...}`.
struct block_statement {
  std::vector<statement> statements;
};

// `var x : int;` or `var x : int = 42;`.
struct variable_definition {
  identifier id;
  expression type;
  std::optional<expression> initializer;
};

// `type foo = int;`.
struct alias_definition {
  identifier id;
  expression type;
};

// `function foo(x : int) : int { ... }`.
struct function_definition {
  identifier id;
  struct parameter { io::location location; identifier id; expression type; };
  std::vector<parameter> parameters;
  expression return_type;
  block_statement body;
};

// `class foo { ... }`.
struct class_definition {
  identifier id;
  block_statement body;
};

// `export ...`.
struct exported_definition {
  definition value;
};

struct module {
  std::vector<statement> statements;
};

}  // namespace syntax::ast
