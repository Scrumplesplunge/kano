export module kano.code;

export import <variant>;
export import <vector>;

export namespace kano::code {

enum class builtin_type {
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

struct pointer_type { type pointee; };

struct function_type {
  type return_type;
  std::vector<type> parameters;
};

template <typename T>
type::type(T&& value) : value(new type_type{std::forward<T>(value)}) {}

enum class symbol {};

using literal = std::variant<bool, std::byte, std::int32_t>;
struct global { symbol symbol; };
struct local { int offset; };
struct dereference;
struct add;
struct sub;
using expression_type =
    std::variant<literal, global, local, dereference, add, sub>;
struct expression {
  template <typename T> expression(type* type, T&& value);
  type* type;
  std::unique_ptr<expression_type> value;
};

struct unop {
  unop(expression inner) : inner(std::move(inner)) {}
  expression inner;
};

struct dereference : unop { using unop::unop; };

struct binop {
  binop(expression left, expression right)
      : left(std::move(left)), right(std::move(right)) {}
  expression left, right;
};

struct add : binop { using binop::binop; };
struct sub : binop { using binop::binop; };

template <typename T>
expression::expression(struct type* type, T&& value)
    : type(type), value(new expression_type{std::forward<T>(value)}) {}

struct label { symbol symbol; };
struct jump { symbol symbol; };
struct jumpc { expression cond; symbol symbol; };
struct retw { expression value; };

}  // namespace kano::code
