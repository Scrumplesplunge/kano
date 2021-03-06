module;

#include <assert.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

export module io;

export import <sstream>;
import <iostream>;
import <string>;
import <string_view>;

namespace io {

struct input {
  std::string name;
  std::string_view source;
};

// Open a file for reading. This intentionally leaks resources: the mmap is
// opened and never closed, and a copy of the filename is allocated but never
// freed. For this reason, calls to open should be limited to a small number.
export const input& open(const char* filename) {
  int fd = ::open(filename, O_RDONLY);
  if (fd < 0) {
    fprintf(stderr, "failed to open file \"%s\": %s\n", filename,
            strerror(errno));
    exit(1);
  }
  struct stat info;
  if (fstat(fd, &info) < 0) {
    fprintf(stderr, "failed to stat file \"%s\": %s\n", filename,
            strerror(errno));
    exit(1);
  }
  const char* data =
      (const char*)mmap(nullptr, info.st_size, PROT_READ, MAP_SHARED, fd, 0);
  if (data == (caddr_t)-1) {
    fprintf(stderr, "failed to mmap file \"%s\": %s\n", filename,
            strerror(errno));
    exit(1);
  }
  close(fd);
  return *new input{filename, std::string_view(data, info.st_size)};
}

export struct location {
  int line = 1, column = 1;
  const char* position = "\n";
  const char* filename = "<unknown>";
  std::string_view line_contents() const {
    const char* const first = position - (column - 1);
    const char* last = position;
    while (*last != '\n') last++;
    return std::string_view(first, last - first);
  }
};

enum class color {
  reset,
  gray,
  green,
  red,
  white,
  yellow,
};

std::ostream& operator<<(std::ostream& output, color c) {
  switch (c) {
    case color::reset: return output << "\x1b[0m";
    case color::gray: return output << "\x1b[30;1m";
    case color::green: return output << "\x1b[32m";
    case color::red: return output << "\x1b[31;1m";
    case color::white: return output << "\x1b[37;1m";
    case color::yellow: return output << "\x1b[33m";
  }
}

export class message {
 public:
  enum type {
    note,
    warning,
    error,
  };
  message(location location, type type) : location_(location) {
    buffer_ << color::white << location_.filename << ':' << location_.line
            << ':' << location_.column << color::reset << ": ";
    switch (type) {
      case note:
        buffer_ << color::gray << "note" << color::reset;
        break;
      case warning:
        buffer_ << color::yellow << "warning" << color::reset;
        break;
      case error:
        buffer_ << color::red << "error" << color::reset;
        break;
    }
    buffer_ << ": ";
  }
  ~message() {
    std::cerr << buffer_.str() << '\n'
              << "    " << location_.line_contents() << "\n    "
              << std::string(location_.column - 1, ' ') << color::green
              << '^' << color::reset << '\n';
  }
  template <typename T>
  message& operator<<(const T& value) {
    buffer_ << value;
    return *this;
  }
 private:
  location location_;
  std::ostringstream buffer_;
};

export class fatal_message : public message {
 public:
  using message::message;
  [[noreturn]] ~fatal_message() { ((message*)this)->~message(); std::exit(1); }
};

export constexpr bool is_hspace(char c) { return c == ' ' || c == '\t'; }
export constexpr bool is_vspace(char c) { return c == '\r' || c == '\n'; }
export constexpr bool is_space(char c) { return is_hspace(c) || is_vspace(c); }
export constexpr bool is_digit(char c) { return '0' <= c && c <= '9'; }
export constexpr bool is_lower(char c) { return 'a' <= c && c <= 'z'; }
export constexpr bool is_upper(char c) { return 'A' <= c && c <= 'Z'; }
export constexpr bool is_alpha(char c) { return is_lower(c) || is_upper(c); }
export constexpr bool is_alnum(char c) { return is_digit(c) || is_alpha(c); }
export constexpr bool is_word(char c) { return c == '_' || is_alnum(c); }
export constexpr bool is_symbol(char c) {
  switch (c) {
    case '!':
    case '%':
    case '&':
    case '*':
    case '+':
    case '-':
    case '.':
    case '/':
    case '<':
    case '=':
    case '>':
    case '|':
    case ';':
    case ':':
    case ',':
      return true;
    default:
      return false;
  }
}

export constexpr bool is_hex(char c) {
  return ('a' <= c && c <= 'f') ||
         ('A' <= c && c <= 'F') ||
         ('0' <= c && c <= '9');
}

export constexpr int parse_hex(char c) {
  assert(is_hex(c));
  switch (c & 0x70) {
    case 3: return c - '0';
    case 4: return c - 'A';
    case 6: return c - 'a';
  }
  // Unreachable when the precondition is met.
  return 0;
}

export class reader {
 public:
  reader(const char* filename)
      : input_(open(filename)) {
    assert(!input_.source.empty());
    assert(input_.source.back() == '\n');
  }

  std::string_view filename() const { return input_.name; }

  void skip_hspace() {
    const char* const begin = remaining_.data();
    const char* const end = begin + remaining_.size();
    const char* i = begin;
    while (i != end && is_hspace(*i)) i++;
    advance(i - begin);
  }

  void skip_space() {
    const char* const begin = remaining_.data();
    const char* const end = begin + remaining_.size();
    const char* i = begin;
    while (i != end && is_space(*i)) i++;
    advance(i - begin);
  }

  template <auto predicate>
  std::string_view peek_sequence() const {
    const char* const begin = remaining_.data();
    const char* const end = begin + remaining_.size();
    const char* i = begin;
    while (i != end && predicate(*i)) i++;
    return std::string_view(begin, i - begin);
  }

  template <auto predicate>
  std::string_view sequence() {
    auto result = peek_sequence<predicate>();
    advance(result.size());
    return result;
  }

  std::string_view peek_word() const { return peek_sequence<is_word>(); }
  std::string_view word() { return sequence<is_word>(); }

  std::string_view peek_symbol() const { return peek_sequence<is_symbol>(); }
  std::string_view symbol() { return sequence<is_symbol>(); }

  // Precondition: peek() == '"'
  std::string parse_string_literal() {
    // TODO: Change this to check for and only accept valid UTF-8.
    assert(peek() == '"');
    eat('"');
    std::string value;
    for (char c = get(); c != '"'; c = get()) {
      if (c == '\\') {
        const auto l = location();
        c = get();
        switch (c) {
          case '0':
            value.push_back('\0');
            break;
          case 'n':
            value.push_back('\n');
            break;
          case 'r':
            value.push_back('\r');
            break;
          case 't':
            value.push_back('\t');
            break;
          case 'x': {
            char hi = get(), lo = get();
            if (!is_hex(hi) || !is_hex(lo)) {
              die(l) << "truncated hex byte.";
            }
            value.push_back(parse_hex(hi) << 4 | parse_hex(lo));
            break;
          }
          case '\\':
          case '\"':
            value.push_back(c);
            break;
          default:
            die(l) << "invalid escape sequence.";
        }
      } else if (c == '\n') {
        die() << "unexpected newline in string literal.";
      } else {
        value.push_back(c);
      }
    }
    return value;
  }

  bool eof() {
    return remaining_.empty();
  }

  char peek() {
    if (remaining_.empty()) die() << "unexpected end of file.";
    return remaining_[0];
  }

  char get() {
    char c = peek();
    advance(1);
    return c;
  }

  bool starts_with(std::string_view prefix) {
    return remaining_.starts_with(prefix);
  }

  bool try_eat(std::string_view prefix) {
    if (!remaining_.starts_with(prefix)) return false;
    advance(prefix.size());
    return true;
  }

  bool try_eat(char c) { return try_eat(std::string_view(&c, 1)); }

  void eat(std::string_view prefix) {
    if (!try_eat(prefix)) die() << "expected '" << prefix << "'.";
  }

  void eat(char c) { return eat(std::string_view(&c, 1)); }

  location location() const {
    return {.line = line_,
            .column = column_,
            .position = remaining_.data(),
            .filename = input_.name.c_str()};
  }

  io::message message(message::type type, io::location l) const {
    return {l, type};
  }

  io::message message(message::type type) const {
    return message(type, location());
  }

  io::fatal_message die(io::location l) const {
    return {l, io::message::error};
  }

  io::fatal_message die() const { return die(location()); }

  const char* begin() const { return remaining_.data(); }
  const char* end() const { return remaining_.data() + remaining_.size(); }

  void advance(unsigned n) {
    assert(n <= remaining_.size());
    for (char c : remaining_.substr(0, n)) {
      if (c == '\n') {
        line_++;
        column_ = 1;
      } else {
        column_++;
      }
    }
    remaining_.remove_prefix(n);
  }

  void rewind(io::location location) {
    const auto begin = input_.source.data(), end = begin + input_.source.size();
    assert(begin <= location.position && location.position <= end);
    line_ = location.line;
    column_ = location.column;
    remaining_ = std::string_view(location.position, end - location.position);
  }

 private:
  const io::input& input_;
  int line_ = 1, column_ = 1;
  std::string_view remaining_ = input_.source;
};

}  // namespace io
