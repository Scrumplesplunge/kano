// IR for Kano semantics. Its primary objective is to disambiguate Kano source
// code, but a consequence of transforming the syntax tree into the IR is that
// we will catch some basic bugs such as typos or type errors.

module;

#include <cassert>

export module semantics.ir;

export import node;
export import <map>;
export import <vector>;

export namespace semantics::ir {

// Symbols are unique identifiers for user-defined artefacts.
enum symbol : int {
  none,
  builtin_bool,
  builtin_int32,
  builtin_void,
  builtin_void_load,
  builtin_void_store,
  builtin_bool_load,
  builtin_bool_store,
  builtin_bool_not,
  builtin_bool_and,
  builtin_bool_or,
  builtin_bool_to_int32,
  builtin_int32_load,
  builtin_int32_store,
  builtin_int32_to_bool,
  builtin_int32_negate,
  builtin_int32_add,
  builtin_int32_subtract,
  builtin_int32_multiply,
  builtin_int32_divide,
  builtin_int32_modulo,
  builtin_int32_compare_eq,
  builtin_int32_compare_ne,
  builtin_int32_compare_lt,
  builtin_int32_compare_le,
  builtin_int32_compare_gt,
  builtin_int32_compare_ge,
  first_user_symbol,
};

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
  bool operator==(const array_type&) const = default;
  auto operator<=>(const array_type&) const = default;
};

// Pointers to objects.
struct pointer_type {
  data_type pointee;
  bool operator==(const pointer_type&) const = default;
  auto operator<=>(const pointer_type&) const = default;
};

// User-defined compound types.
struct user_defined_type {
  symbol symbol;
  bool operator==(const user_defined_type&) const = default;
  auto operator<=>(const user_defined_type&) const = default;
};

// Function types. Note that function types are *not* data types! A function
// cannot be copied, moved, stored in a class, etc. However, *pointers* to
// functions *are* data types.
struct function_type {
  data_type return_type;
  std::vector<data_type> parameters;
  bool operator==(const function_type&) const = default;
  auto operator<=>(const function_type& o) const {
    if (auto x = return_type <=> o.return_type; x != 0) return x;
    return parameters == o.parameters
               ? std::strong_ordering::equal
               : parameters < o.parameters ? std::strong_ordering::less
                                           : std::strong_ordering::greater;
  }
};

// Pointers to functions.
struct function_pointer_type {
  function_type pointee;
  bool operator==(const function_pointer_type&) const = default;
  auto operator<=>(const function_pointer_type&) const = default;
};

using type = std::variant<data_type, function_type>;

struct void_value {};
struct pointer;
struct function_pointer;
struct array;
using value =
    node<void_value, bool, std::int32_t, pointer, function_pointer, array>;

struct pointer {
  symbol symbol;
  data_type pointee_type;
};

struct function_pointer {
  symbol symbol;
  function_type pointee_type;
};

struct array {
  std::vector<value> contents;
  data_type element_type;
};

data_type type_of(const value&);
builtin_type type_of(const void_value&) { return builtin_type::void_type; }
builtin_type type_of(bool) { return builtin_type::bool_type; }
builtin_type type_of(std::int32_t) { return builtin_type::int32_type; }
pointer_type type_of(const pointer& p) { return pointer_type{p.pointee_type}; }
function_pointer_type type_of(const function_pointer& f) {
  return function_pointer_type{f.pointee_type};
}
array_type type_of(const array& a) {
  DEBUG_ONLY {
    for (const auto& x : a.contents) {
      assert(type_of(x) == a.element_type);
    }
  }
  return array_type{a.contents.size(), a.element_type};
}

data_type type_of(const value& v) {
  return v.visit([&](const auto& x) -> data_type {
    return {v.location(), type_of(x)};
  });
}

struct literal {
  value value;
};

enum local : int {};

struct constant {
  value value;
};

struct stack_allocate {};

struct call {
  symbol op;
  std::vector<local> arguments;
};

// Given a *T referring to an array of T, produces a *T referring to the nth
// element of that array.
struct index {
  local address;
  local offset;
};

using action = node<constant, stack_allocate, call, index>;

struct step {
  local destination;
  action action;
};

struct expression {
  // TODO: For supporting efficient move semantics, we need to track the value
  // category for locals as well as their type. Value categories are not types:
  // you can't have an lvalue array of prvalues or a prvalue array of lvalues.
  std::map<local, data_type> locals;
  std::vector<step> steps;
};

}  // namespace semantics::ir
