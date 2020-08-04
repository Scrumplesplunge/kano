module;

#include <cassert>

export module parser;

export import ast;
import io;
import <unordered_set>;

bool is_integer(std::string_view value) {
  if (value.starts_with("-")) value.remove_prefix(1);
  if (value.empty()) return false;
  if (value[0] == '0' && value.size() > 1) return false;  // No 0 padding.
  for (char c : value) if (!io::is_digit(c)) return false;
  return true;
}

bool is_identifier(std::string_view value) {
  if (value.empty()) return false;
  for (char c : value) if (!io::is_word(c)) return false;
  if (io::is_digit(value[0])) return false;
  return true;
}

struct parser : io::reader {
  using reader::reader;

  void skip_comment() {
    eat('#');
    const char* i = begin();
    while (*i != '\n') i++;
    advance(i - begin());
  }

  void skip_whitespace_and_comments() {
    skip_space();
    while (!eof() && peek() == '#') {
      skip_comment();
      skip_space();
    }
  }

  std::int32_t parse_integer() {
    if (!is_integer(peek_word())) die() << "expected number.";
    const auto number = peek_word();
    advance(number.size());
    return std::stoi(std::string(number));
  }

  // Precondition: peek_word() == "ascii"
  std::string parse_ascii() {
    assert(peek_word() == "ascii");
    advance(5);
    skip_hspace();
    eat('"');
    std::string value;
    for (char c = get(); c != '"'; c = get()) {
      if (c == '\\') {
        c = get();
        switch (c) {
          case 'n':
            value.push_back('\n');
            break;
          case '\\':
          case '\"':
            value.push_back(c);
            break;
          default:
            die() << "invalid escape sequence.";
        }
      } else if (c == '\n') {
        die() << "unexpected newline in string literal.";
      } else {
        value.push_back(c);
      }
    }
    return value;
  }

  ast::builtin parse_builtin() {
    const auto l = location();
    const auto builtin_name = word();
    if (builtin_name == "write") return ast::builtin::write;
    if (builtin_name == "exit") return ast::builtin::exit;
    die(l) << "no such builtin: " << builtin_name;
  }

  std::variant<std::int32_t, std::string> parse_value() {
    skip_hspace();
    const auto word = peek_word();
    if (word == "ascii") return parse_ascii();
    if (is_integer(word)) return parse_integer();
    die() << "expected a value.";
  }

  // Precondition: peek_word() == "function"
  std::pair<std::string_view, ast::function> parse_function() {
    assert(peek_word() == "function");
    advance(8);
    skip_hspace();
    const auto name = word();
    if (name.empty()) die() << "expected function name.";
    skip_hspace();
    ast::function function;
    function.frame_size = parse_integer();
    skip_hspace();
    eat('{');
    struct stack_entry {
      io::location location;
      ast::expr value;
    };
    std::unordered_map<std::string_view, io::location> defined_labels;
    std::unordered_map<std::string_view, io::location> jump_labels;
    std::vector<stack_entry> stack;
    while (true) {
      skip_whitespace_and_comments();
      const auto l = location();
      auto pop = [&] {
        if (stack.empty()) die(l) << "no value on stack.";
        auto result = std::move(stack.back().value);
        stack.pop_back();
        return result;
      };
      auto check_empty = [&] {
        if (!stack.empty()) {
          message(io::message::error, l) << "expression stack is not empty.";
          for (const auto& x : stack) {
            message(io::message::note, x.location) << "item still on stack.";
          }
          std::exit(1);
        }
      };
      auto call_args = [&] {
        const auto argc = parse_integer();
        if ((int)stack.size() < argc) {
          die(l) << "not enough values on stack, need " << argc
                 << " params plus a callee.";
        }
        std::vector<ast::expr> args;
        for (int i = 0; i < argc; i++) {
          args.push_back(std::move(stack.back().value));
          stack.pop_back();
        }
        return args;
      };
      if (try_eat('}')) {
        check_empty();
        break;
      }
      const auto command = word();
      skip_hspace();
      if (command == "const") {
        stack.push_back({l, parse_integer()});
      } else if (command == "address") {
        stack.push_back({l, ast::global{word()}});
      } else if (command == "builtin") {
        stack.push_back({l, parse_builtin()});
      } else if (command == "local") {
        stack.push_back({l, ast::local{parse_integer()}});
      } else if (command == "loadw") {
        if (stack.empty()) die(l) << "no value on stack.";
        stack.back() = {l, ast::loadw{std::move(stack.back().value)}};
      } else if (command == "add") {
        if (stack.size() < 2) die(l) << "not enough values on stack.";
        auto b = pop();
        stack.back() = {l,
                        ast::add{std::move(stack.back().value), std::move(b)}};
      } else if (command == "callw") {
        auto callee = pop();
        stack.push_back({l, ast::callw{std::move(callee), call_args()}});
      } else if (command == "jump") {
        check_empty();
        const auto label_pos = location();
        const auto target = word();
        jump_labels.emplace(target, label_pos);
        function.code.push_back(ast::jump{target});
      } else if (command == "jumpc") {
        if (stack.empty()) die() << "no value on stack.";
        auto condition = pop();
        check_empty();
        const auto label_pos = location();
        const auto target = word();
        jump_labels.emplace(target, label_pos);
        function.code.push_back(ast::jumpc{std::move(condition), target});
      } else if (command == "call") {
        if (stack.empty()) die(l) << "no value on stack.";
        auto callee = pop();
        function.code.push_back(ast::call{{std::move(callee), call_args()}});
        check_empty();
      } else if (command == "ret") {
        function.code.push_back(ast::ret{});
      } else if (command == "retw") {
        function.code.push_back(ast::retw{pop()});
        check_empty();
      } else if (command == "storew") {
        if (stack.size() < 2) die(l) << "not enough values on stack.";
        auto b = pop();
        function.code.push_back(ast::storew{pop(), std::move(b)});
      } else if (peek() == ':') {
        eat(':');
        check_empty();
        defined_labels.emplace(command, l);
        function.code.push_back(ast::label{command});
      } else {
        die(l) << "no such command: " << command;
      }
    }
    for (const auto& [label, location] : defined_labels) {
      if (!jump_labels.contains(label)) {
        message(io::message::warning, location)
            << "label '" << label << "' is unused.";
      }
    }
    for (const auto& [target, location] : jump_labels) {
      if (!defined_labels.contains(target)) {
        die(location) << "label '" << target << "' is undefined.";
      }
    }
    return {name, std::move(function)};
  }

  // Precondition: is_identifier(peek_word())
  std::pair<std::string_view, ast::rodata> parse_data() {
    const auto name = word();
    skip_hspace();
    eat('=');
    skip_hspace();
    return {name, parse_value()};
  }

  // Precondition: peek_word() == "var"
  std::pair<std::string_view, ast::zeros> parse_var() {
    assert(peek_word() == "var");
    advance(3);
    skip_hspace();
    const auto name = word();
    if (name.empty()) die() << "expected variable name.";
    eat('[');
    const auto size = parse_integer();
    eat(']');
    return {name, ast::zeros(size)};
  }

  ast::program parse_program() {
    ast::program program;
    auto define = [&](std::string_view name, ast::definition d) {
      auto [i, is_new] = program.emplace(name, std::move(d));
      if (!is_new) {
        message(io::message::error, d.location)
            << "redefinition of '" << i->first << "'.";
        message(io::message::note, i->second.location)
            << "previous definition was here.";
        std::exit(1);
      }
    };
    while (true) {
      skip_whitespace_and_comments();
      if (eof()) return program;
      const auto word = peek_word();
      if (word.empty()) die() << "expected function or data definition.";
      const auto l = location();
      if (word == "function") {
        auto [name, value] = parse_function();
        define(name, {l, std::move(value)});
      } else if (word == "var") {
        auto [name, value] = parse_var();
        define(name, {l, std::move(value)});
      } else {
        auto [name, value] = parse_data();
        define(name, {l, std::move(value)});
      }
    }
  }
};

export ast::program parse(const char* filename) {
  return parser{filename}.parse_program();
}
