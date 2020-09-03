// IR for Kano semantics. Its primary objective is to disambiguate Kano source
// code, but a consequence of transforming the syntax tree into the IR is that
// we will catch some basic bugs such as typos or type errors.

export module semantics.ir;

export import node;
export import <map>;
export import <vector>;

export namespace semantics::ir {

// Symbols are unique identifiers for user-defined artefacts.
enum symbol : int {};

enum builtin_type : int;
struct array_type;
struct pointer_type;
struct user_defined_type;
struct function_pointer_type;
using data_type = node<builtin_type, array_type, pointer_type,
                       user_defined_type, function_pointer_type>;

// Primitive builtin types.
enum builtin_type : int {
  bool_type,
  int32_type,
  void_type,
};

// Fixed-size arrays of values.
struct array_type {
  std::uint64_t size;
  data_type element;
};

// Pointers to objects.
struct pointer_type {
  data_type pointee;
};

// User-defined compound types.
struct user_defined_type {
  symbol symbol;
};

// Function types. Note that function types are *not* data types! A function
// cannot be copied, moved, stored in a class, etc. However, *pointers* to
// functions *are* data types.
struct function_type {
  data_type return_type;
  std::vector<data_type> parameters;
};

// Pointers to functions.
struct function_pointer_type {
  function_type pointee;
};

using type = std::variant<data_type, function_type>;

}  // namespace semantics::ir
