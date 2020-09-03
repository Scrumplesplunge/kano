module;

#include <cassert>

export module node;

export import io;
export import <variant>;
import <concepts>;
import <iostream>;

export template <typename... Types>
class node {
 public:
  using value_type = std::variant<Types...>;

  constexpr node() = default;

  // Construct a node from a source location and a value.
  template <typename T>
  requires std::is_constructible_v<value_type, T>
  node(io::location location, T&& value)
      : location_(location),
        value_(std::make_unique<value_type>(std::forward<T>(value))) {}

  // Move construction and assignment.
  constexpr node(node&& other) noexcept
      : location_(std::exchange(other.location_, {})),
        value_(std::exchange(other.value_, nullptr)) {}

  constexpr node& operator=(node&& other) noexcept {
    location_ = other.location_;
    value_ = std::move(other.value_);
    return *this;
  }

  // Copy construction and assignment, conditionally available.
  // In principle this should have:
  //
  //   requires std::is_copy_constructible_v<value_type>
  //
  // However, adding such a requirement in code causes problems with incomplete
  // types and that prevents us from defining recursive types like trees, so the
  // requirement is not specified.
  constexpr node(const node& other)
      : location_(other.location_),
        value_(other.value_ ? std::make_unique<value_type>(*other.value_)
                            : nullptr) {}

  constexpr node& operator=(const node& other) {
    location_ = other.location_;
    value_ =
        other.value_ ? std::make_unique<value_type>(*other.value_) : nullptr;
    return *this;
  }

  // Check whether this node is empty.
  constexpr explicit operator bool() const { return value_ != nullptr; }
  constexpr const io::location& location() const { return location_; }

  // Direct access to the stored value.
  constexpr value_type& operator*() { return *value_; }
  constexpr const value_type& operator*() const { return *value_; }
  constexpr value_type* operator->() { return value_.get(); }
  constexpr const value_type* operator->() const { return value_.get(); }

  // Accessors.
  template <typename T>
  requires (std::is_same_v<T, Types> || ...)
  constexpr bool is() const {
    return value_ && std::holds_alternative<T>(*value_);
  }

  template <typename T>
  requires (std::is_same_v<T, Types> || ...)
  constexpr const T* get() const {
    return value_ ? std::get_if<T>(value_.get()) : nullptr;
  }

  template <typename T>
  requires (std::is_same_v<T, Types> || ...)
  constexpr T* get() {
    return value_ ? std::get_if<T>(value_.get()) : nullptr;
  }

  // Mutable visit.
  template <typename Visitor>
  requires (std::is_invocable_v<Visitor, Types&> && ...)
  auto visit(Visitor&& visitor) -> decltype(auto) {
    assert(value_);
    return std::visit(std::forward<Visitor>(visitor), **this);
  }

  // Immutable visit.
  template <typename Visitor>
  requires (std::is_invocable_v<Visitor, const Types&> && ...)
  auto visit(Visitor&& visitor) const -> decltype(auto) {
    assert(value_);
    return std::visit(std::forward<Visitor>(visitor), **this);
  }

 private:
  io::location location_;
  std::unique_ptr<value_type> value_;
};

template <typename T>
concept printable = requires (std::ostream& output, const T& t) {
  { output << t } -> std::same_as<std::ostream&>;
};

export template <typename... Types>
requires (printable<Types> && ...)
std::ostream& operator<<(std::ostream& output, const node<Types...>& n) {
  n.visit([&](const auto& x) { output << x; });
  return output;
}

template <typename T>
concept comparable = requires(const T& a, const T& b) {
  { a == b } -> std::same_as<bool>;
};

export template <typename... Types>
requires (comparable<Types> && ...)
bool operator==(const node<Types...>& l, const node<Types...>& r) {
  return (!l && !r) || *l == *r;
}
