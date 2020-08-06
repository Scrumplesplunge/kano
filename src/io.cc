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

export std::string_view open(const char* filename) {
  int fd = ::open(filename, O_RDONLY);
  if (fd < 0) {
    perror("open");
    exit(1);
  }
  struct stat info;
  if (fstat(fd, &info) < 0) {
    perror("fstat");
    exit(1);
  }
  const char* data =
      (const char*)mmap(nullptr, info.st_size, PROT_READ, MAP_SHARED, fd, 0);
  if (data == (caddr_t)-1) {
    perror("mmap");
    exit(1);
  }
  close(fd);
  return std::string_view(data, info.st_size);
}

export struct location {
  int line = 1, column = 1;
  const char* position = "\n";
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
  message(std::string_view filename, location location, type type)
      : location_(location) {
    buffer_ << color::white << filename << ':' << location_.line << ':'
            << location_.column << color::reset << ": ";
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

export class reader {
 public:
  reader(const char* filename)
      : filename_(filename), source_(open(filename)) {
    assert(!source_.empty());
    assert(source_.back() == '\n');
  }

  std::string_view filename() const { return filename_; }

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

  std::string_view peek_word() const {
    const char* const begin = remaining_.data();
    const char* const end = begin + remaining_.size();
    const char* i = begin;
    while (i != end && is_word(*i)) i++;
    return std::string_view(begin, i - begin);
  }

  std::string_view word() {
    auto result = peek_word();
    advance(result.size());
    return result;
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
    return {.line = line_, .column = column_, .position = remaining_.data()};
  }

  io::message message(message::type type, io::location l) const {
    return {filename_, l, type};
  }

  io::message message(message::type type) const {
    return message(type, location());
  }

  io::fatal_message die(io::location l) const {
    return {filename_, l, io::message::error};
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

 private:

  const std::string filename_;
  const std::string_view source_;
  int line_ = 1, column_ = 1;
  std::string_view remaining_ = source_;
};

}  // namespace io
