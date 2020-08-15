module;

#include <assert.h>

export module kano.parser;

export import kano.ast;
import io;
import <map>;

namespace kano {

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

  bool try_symbol(std::string_view symbol) {
    skip_whitespace_and_comments();
    return try_eat(symbol);
  }

  void symbol(std::string_view symbol) {
    if (!try_symbol(symbol)) die() << "expected '" << symbol << "'.";
  }

  void skip_comment() {
    assert(peek() == '#');
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

  ast::name parse_name() {
    const auto word = peek_word();
    if (!is_identifier(word)) die() << "expected name.";
    advance(word.size());
    return ast::name{word};
  }

  ast::expression parse_atom() {
    if (try_symbol("(")) {
      auto inner = parse_expression();
      skip_whitespace_and_comments();
      symbol(")");
      return inner;
    }
    const auto l = location();
    if (peek() == '"') return {l, parse_string_literal()};
    const auto word = peek_word();
    if (is_integer(word)) return {l, parse_integer()};
    if (is_identifier(word)) return {l, parse_name()};
    die() << "expected expression.";
  }

  ast::expression parse_suffix() {
    ast::expression result = parse_atom();
    while (true) {
      skip_whitespace_and_comments();
      const auto l = location();
      if (try_symbol("(")) {
        std::vector<ast::expression> arguments;
        if (!try_symbol(")")) {
          while (true) {
            arguments.push_back(parse_expression());
            if (try_symbol(")")) break;
            symbol(",");
          }
        }
        result = {l, ast::call{std::move(result), std::move(arguments)}};
      } else {
        return result;
      }
    }
  }

  ast::expression parse_prefix() {
    skip_whitespace_and_comments();
    const auto l = location();
    if (try_symbol("*")) return {l, ast::dereference{parse_prefix()}};
    if (try_symbol("&")) return {l, ast::address_of{parse_prefix()}};
    return parse_suffix();
  }

  ast::expression parse_sum() {
    ast::expression result = parse_prefix();
    while (true) {
      skip_whitespace_and_comments();
      const auto l = location();
      if (try_symbol("+")) {
        result = {l, ast::add{std::move(result), parse_prefix()}};
      } else if (try_symbol("-")) {
        result = {l, ast::sub{std::move(result), parse_prefix()}};
      } else {
        return result;
      }
    }
  }

  ast::expression parse_expression() {
    ast::expression left = parse_sum();
    skip_whitespace_and_comments();
    const auto l = location();
    if (try_symbol("<")) {
      return {l, ast::cmp_lt{std::move(left), parse_sum()}};
    } else if (try_symbol("==")) {
      return {l, ast::cmp_eq{std::move(left), parse_sum()}};
    } else {
      return left;
    }
  }

  ast::types::type parse_type() {
    skip_whitespace_and_comments();
    const auto l = location();
    if (try_symbol("*")) return {l, ast::types::pointer{parse_type()}};
    const auto name = word();
    if (name.empty()) die() << "expected type name.";
    return {l, ast::types::name{name}};
  }

  ast::if_statement parse_if_statement() {
    assert(peek_word() == "if");
    word();
    skip_whitespace_and_comments();
    eat('(');
    auto condition = parse_expression();
    skip_whitespace_and_comments();
    eat(')');
    skip_whitespace_and_comments();
    auto then_branch = parse_statement();
    skip_whitespace_and_comments();
    if (try_eat("else")) {
      return ast::if_statement{std::move(condition), std::move(then_branch),
                               parse_statement()};
    } else {
      return ast::if_statement{
          std::move(condition), std::move(then_branch), {}};
    }
  }

  ast::while_statement parse_while_statement() {
    assert(peek_word() == "while");
    word();
    skip_whitespace_and_comments();
    eat('(');
    auto condition = parse_expression();
    skip_whitespace_and_comments();
    eat(')');
    skip_whitespace_and_comments();
    auto body = parse_statement();
    skip_whitespace_and_comments();
    return ast::while_statement{std::move(condition), std::move(body)};
  }

  ast::break_statement parse_break_statement() {
    assert(peek_word() == "break");
    word();
    eat(';');
    return {};
  }

  ast::continue_statement parse_continue_statement() {
    assert(peek_word() == "continue");
    word();
    eat(';');
    return {};
  }

  ast::return_statement parse_return_statement() {
    assert(peek_word() == "return");
    word();
    if (try_symbol(";")) return {};
    auto value = parse_expression();
    symbol(";");
    return {std::move(value)};
  }

  std::vector<ast::statement> parse_block() {
    eat('{');
    std::vector<ast::statement> block;
    while (true) {
      skip_whitespace_and_comments();
      if (try_eat('}')) return block;
      block.push_back(parse_statement());
    }
  }

  ast::variable_declaration parse_variable_declaration() {
    assert(peek_word() == "var");
    word();
    skip_whitespace_and_comments();
    const auto name = word();
    if (name.empty()) die() << "expected variable name.";
    symbol(":");
    auto type = parse_type();
    if (try_symbol(";")) return {name, std::move(type)};
    symbol("=");
    auto initializer = parse_expression();
    symbol(";");
    return {name, std::move(type), std::move(initializer)};
  }

  ast::statement parse_statement() {
    skip_whitespace_and_comments();
    const auto l = location();
    if (peek() == '{') return {l, parse_block()};
    const auto word = peek_word();
    if (word == "if") return {l, parse_if_statement()};
    if (word == "while") return {l, parse_while_statement()};
    if (word == "break") return {l, parse_break_statement()};
    if (word == "continue") return {l, parse_continue_statement()};
    if (word == "return") return {l, parse_return_statement()};
    if (word == "var") return {l, parse_variable_declaration()};
    auto expression = parse_expression();
    if (try_eat('=')) {
      auto value = parse_expression();
      eat(';');
      return {l, ast::assignment{std::move(expression), std::move(value)}};
    }
    eat(';');
    if (auto* call = std::get_if<ast::call>(expression.value.get())) {
      return {l, ast::call_statement{std::move(*call)}};
    } else {
      die(l) << "expression result is discarded.";
    }
  }

  ast::function parse_function() {
    assert(peek_word() == "function");
    word();
    skip_whitespace_and_comments();
    const auto name = word();
    if (name.empty()) die() << "expected function name.";
    symbol("(");
    std::vector<ast::variable_declaration> parameters;
    if (!try_symbol(")")) {
      while (true) {
        skip_whitespace_and_comments();
        const auto parameter = word();
        if (parameter.empty()) die() << "expected parameter name.";
        symbol(":");
        auto type = parse_type();
        parameters.push_back({parameter, std::move(type)});
        if (try_symbol(")")) break;
        symbol(",");
      }
    }
    symbol(":");
    auto return_type = parse_type();
    skip_whitespace_and_comments();
    auto body = parse_block();
    return {name, std::move(return_type), std::move(parameters),
            std::move(body)};
  }

  ast::module parse_module() {
    std::vector<ast::definition> definitions;
    while (true) {
      skip_whitespace_and_comments();
      if (eof()) {
        return ast::module{std::string(filename()), std::move(definitions)};
      }
      const auto l = location();
      const auto word = peek_word();
      if (word == "function") {
        definitions.push_back({l, parse_function()});
      } else if (word == "var") {
        definitions.push_back({l, parse_variable_declaration()});
      } else {
        die() << "expected function definition.";
      }
    }
  }
};

export ast::module parse(const char* filename) {
  return parser{filename}.parse_module();
}

}  // namespace kano
