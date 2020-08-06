export module ir.ast;

import io;
import <string_view>;
import <memory>;
export import <unordered_map>;
export import <variant>;
export import <vector>;

export namespace ir::ast {

// Local variable. The integer value is the offset from the frame pointer.
enum local : int {};

// Global variable. The string value is the name of the global label.
struct global { std::string_view name; };

struct loadw;
struct add;
struct sub;
struct callw;
enum class builtin { write, exit };
using expr_type =
    std::variant<std::int32_t, local, global, loadw, add, sub, callw, builtin>;
struct expr {
  template <typename T> expr(T&& value);
  std::unique_ptr<expr_type> value;
};

struct unop {
  unop(expr inner) : inner(std::move(inner)) {}
  expr inner;
};

// Load a word from the address computed by the inner expression.
struct loadw : unop { using unop::unop; };

struct binop {
  binop(expr left, expr right)
      : left(std::move(left)), right(std::move(right)) {}
  expr left, right;
};

// Arithmetic operators.
struct add : binop { using binop::binop; };
struct sub : binop { using binop::binop; };

// Call a function with the given args and produce a single word result.
struct callw { expr callee; std::vector<expr> args; };

template <typename T>
expr::expr(T&& value) : value(new expr_type{std::forward<T>(value)}) {}

// Code label that can be jumped to.
struct label { std::string name; };

// Jump to the given label.
struct jump { std::string label; };

// Jump to the given label if the condition value is zero/nonzero.
struct jz { expr cond; std::string label; };
struct jnz { expr cond; std::string label; };

// Call a function with the given args, producing no result.
struct call : callw {};

// Return a single word value from this function.
struct retw { expr value; };

// Store a word-sized value to a location.
struct storew { expr location; expr value; };

// Return from a function that produces no result.
struct ret {};

using stmt_type = std::variant<label, jump, jz, jnz, call, retw, storew, ret>;
struct stmt {
  template <typename T>
  requires(!std::is_same_v<std::decay_t<T>, stmt>)
  stmt(io::location location, T&& value)
      : location(location), value(new stmt_type{std::forward<T>(value)}) {}
  io::location location;
  std::unique_ptr<stmt_type> value;
};

// Read-only data.
using rodata = std::variant<std::int32_t, std::string>;

// A sequence of zero bytes of a given size.
enum class zeros : std::int32_t {};

// A function definition.
struct function {
  // The number of bytes needed for local variables and temporaries.
  std::int32_t frame_size;
  std::vector<stmt> code;
};

struct definition {
  io::location location;
  std::variant<zeros, rodata, function> value;
  enum type { zeros, rodata, function };  // index for the variant.
};
using program = std::unordered_map<std::string, definition>;

}  // namespace ir::ast
