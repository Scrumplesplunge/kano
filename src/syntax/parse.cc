// Parser for Kano source code.

module;

#include <cassert>

export module syntax.parse;

export import syntax.ast;
import io;
import <charconv>;

namespace syntax {

constexpr bool is_integer(std::string_view value) {
  if (value.empty()) return false;
  if (value[0] == '0' && value.size() > 1) return false;  // Disallow 0 padding.
  for (char c : value)
    if (!io::is_digit(c)) return false;
  return true;
}

constexpr bool is_identifier(std::string_view value) {
  if (value.empty()) return false;
  if (io::is_digit(value[0])) return false;
  for (char c : value)
    if (!io::is_word(c)) return false;
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

  std::uint64_t parse_integer() {
    const auto number = peek_word();
    if (!is_integer(number)) die() << "expected integer literal.";
    std::uint64_t value;
    const auto first = number.data(), last = first + number.size();
    auto [x, error] = std::from_chars(first, last, value);
    if (error == std::errc() && x == last) {
      advance(number.size());
      return value;
    }
    if (error == std::errc::result_out_of_range) {
      die() << "integer literal is too large.";
    } else {
      die() << "expected integer literal.";
    }
  }

  ast::identifier parse_identifier() {
    const auto name = peek_word();
    if (!is_identifier(name)) die() << "expected identifier.";
    advance(name.size());
    return ast::identifier{std::string(name)};
  }

  ast::expression parse_qualified() {
    skip_whitespace_and_comments();
    const auto l = location();
    ast::expression result = {l, parse_identifier()};
    while (true) {
      skip_whitespace_and_comments();
      const auto l = location();
      if (!try_symbol(".")) return result;
      result = {l, ast::dot{std::move(result), parse_identifier()}};
    }
  }

  ast::expression parse_type() {
    skip_whitespace_and_comments();
    const auto l = location();
    if (try_symbol("[")) {
      auto index = parse_expression();
      symbol("]");
      return {l, ast::array_type{std::move(index), parse_type()}};
    } else if (try_symbol("*")) {
      return {l, ast::dereference{parse_type()}};
    } else {
      return parse_qualified();
    }
  }

  ast::expression parse_atom() {
    if (try_symbol("(")) {
      auto inner = parse_expression();
      skip_whitespace_and_comments();
      symbol(")");
      return inner;
    }
    if (peek() == '[') return parse_type();
    const auto l = location();
    if (peek() == '"') return {l, ast::literal_string{parse_string_literal()}};
    const auto word = peek_word();
    if (is_integer(word)) return {l, ast::literal_integer{parse_integer()}};
    if (is_identifier(word)) return parse_qualified();
    die() << "expected expression.";
  }

  ast::literal_aggregate::element_type parse_aggregate_value() {
    const auto l = location();
    if (try_symbol("[")) {
      // This could be an index_assignment like `[0] = 42` or it could be an
      // expression starting with a type like `[1]int{0}`.
      auto index = parse_expression();
      symbol("]");
      if (try_symbol("=")) {
        return ast::literal_aggregate::index_assignment{std::move(index),
                                                        parse_expression()};
      } else {
        // This is the only place in the parser which requires backtracking :(
        // TODO: Figure out a way to avoid backtracking here.
        rewind(l);
        return parse_expression();
      }
    } else if (try_symbol(".")) {
      auto id = parse_identifier();
      symbol("=");
      return ast::literal_aggregate::field_assignment{std::move(id),
                                                      parse_expression()};
    } else {
      return parse_expression();
    }
  }

  ast::expression parse_suffix() {
    ast::expression result = parse_atom();
    while (true) {
      skip_whitespace_and_comments();
      const auto l = location();
      if (try_symbol(".")) {
        result = {l, ast::dot{std::move(result), parse_identifier()}};
      } else if (try_symbol("[")) {
        auto index = parse_expression();
        symbol("]");
        result = {l, ast::index{std::move(result), std::move(index)}};
      } else if (try_symbol("(")) {
        std::vector<ast::expression> arguments;
        if (!try_symbol(")")) {
          while (true) {
            arguments.push_back(parse_expression());
            if (try_symbol(")")) break;
            symbol(",");
          }
        }
        result = {l, ast::call{std::move(result), std::move(arguments)}};
      } else if (try_symbol("{")) {
        std::vector<ast::literal_aggregate::element_type> arguments;
        if (!try_symbol("}")) {
          while (true) {
            arguments.push_back(parse_aggregate_value());
            if (try_symbol("}")) break;
            symbol(",");
          }
        }
        result = {
            l, ast::literal_aggregate{std::move(result), std::move(arguments)}};
      } else {
        return result;
      }
    }
  }

  ast::expression parse_prefix() {
    skip_whitespace_and_comments();
    const auto l = location();
    if (try_symbol("!")) return {l, ast::logical_not{parse_prefix()}};
    if (try_symbol("-")) return {l, ast::negate{parse_prefix()}};
    if (try_symbol("*")) return {l, ast::dereference{parse_prefix()}};
    if (try_symbol("&")) return {l, ast::address_of{parse_prefix()}};
    return parse_suffix();
  }

  ast::expression parse_product() {
    ast::expression result = parse_prefix();
    while (true) {
      skip_whitespace_and_comments();
      const auto l = location();
      if (try_symbol("*")) {
        result = {l, ast::multiply{std::move(result), parse_prefix()}};
      } else if (try_symbol("/")) {
        result = {l, ast::divide{std::move(result), parse_prefix()}};
      } else if (try_symbol("%")) {
        result = {l, ast::modulo{std::move(result), parse_prefix()}};
      } else {
        return result;
      }
    }
  }

  ast::expression parse_sum() {
    ast::expression result = parse_product();
    while (true) {
      skip_whitespace_and_comments();
      const auto l = location();
      if (try_symbol("+")) {
        result = {l, ast::add{std::move(result), parse_product()}};
      } else if (try_symbol("-")) {
        result = {l, ast::subtract{std::move(result), parse_product()}};
      } else {
        return result;
      }
    }
  }

  ast::expression parse_comparison() {
    ast::expression result = parse_sum();
    skip_whitespace_and_comments();
    const auto l = location();
    if (try_symbol("==")) {
      return {l, ast::compare_eq{std::move(result), parse_sum()}};
    } else if (try_symbol("!=")) {
      return {l, ast::compare_ne{std::move(result), parse_sum()}};
    } else if (try_symbol("<")) {
      return {l, ast::compare_lt{std::move(result), parse_sum()}};
    } else if (try_symbol("<=")) {
      return {l, ast::compare_le{std::move(result), parse_sum()}};
    } else if (try_symbol(">")) {
      return {l, ast::compare_gt{std::move(result), parse_sum()}};
    } else if (try_symbol(">=")) {
      return {l, ast::compare_ge{std::move(result), parse_sum()}};
    } else {
      return result;
    }
  }

  ast::expression parse_conjunction() {
    ast::expression result = parse_comparison();
    while (true) {
      skip_whitespace_and_comments();
      const auto l = location();
      if (try_symbol("&&")) {
        result = {l, ast::logical_and{std::move(result), parse_comparison()}};
      } else {
        return result;
      }
    }
  }

  ast::expression parse_disjunction() {
    ast::expression result = parse_conjunction();
    while (true) {
      skip_whitespace_and_comments();
      const auto l = location();
      if (try_symbol("||")) {
        result = {l, ast::logical_and{std::move(result), parse_conjunction()}};
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
      return {l, ast::compare_lt{std::move(left), parse_sum()}};
    } else if (try_symbol("==")) {
      return {l, ast::compare_eq{std::move(left), parse_sum()}};
    } else {
      return left;
    }
  }

  ast::statement parse_import_statement() {
    skip_whitespace_and_comments();
    const auto l = location();
    assert(peek_word() == "import");
    word();
    std::vector<std::string> path;
    do {
      skip_whitespace_and_comments();
      path.push_back(parse_identifier().value);
    } while (try_symbol("."));
    symbol(";");
    return {l, ast::import_statement{std::move(path)}};
  }

  ast::definition parse_variable_definition() {
    assert(peek_word() == "var");
    word();
    skip_whitespace_and_comments();
    const auto l = location();
    auto name = parse_identifier();
    symbol(":");
    auto type = parse_type();
    if (try_symbol(";")) {
      return {l, ast::variable_definition{std::move(name), std::move(type),
                                          std::nullopt}};
    }
    symbol("=");
    auto value = parse_expression();
    symbol(";");
    return {l, ast::variable_definition{std::move(name), std::move(type),
                                        std::move(value)}};
  }

  ast::definition parse_alias_definition() {
    assert(peek_word() == "type");
    word();
    skip_whitespace_and_comments();
    const auto l = location();
    auto name = parse_identifier();
    symbol("=");
    auto type = parse_type();
    symbol(";");
    return {l, ast::alias_definition{std::move(name), std::move(type)}};
  }

  ast::definition parse_function_definition() {
    assert(peek_word() == "function");
    word();
    skip_whitespace_and_comments();
    const auto l = location();
    auto name = parse_identifier();
    symbol("(");
    std::vector<ast::function_definition::parameter> parameters;
    if (!try_symbol(")")) {
      while (true) {
        skip_whitespace_and_comments();
        const auto l = location();
        auto parameter = parse_identifier();
        symbol(":");
        auto type = parse_type();
        parameters.push_back({l, std::move(parameter), std::move(type)});
        if (try_symbol(")")) break;
        symbol(",");
      }
    }
    symbol(":");
    auto return_type = parse_type();
    skip_whitespace_and_comments();
    auto body = parse_block();
    return {l,
            ast::function_definition{std::move(name), std::move(parameters),
                                     std::move(return_type), std::move(body)}};
  }

  ast::definition parse_class_definition() {
    assert(peek_word() == "class");
    word();
    skip_whitespace_and_comments();
    const auto l = location();
    auto name = parse_identifier();
    auto body = parse_block();
    return {l, ast::class_definition{std::move(name), std::move(body)}};
  }

  ast::definition parse_definition() {
    skip_whitespace_and_comments();
    const auto w = peek_word();
    if (w == "var") return parse_variable_definition();
    if (w == "type") return parse_alias_definition();
    if (w == "function") return parse_function_definition();
    if (w == "class") return parse_class_definition();
    die() << "expected definition.";
  }

  ast::statement parse_if_statement() {
    assert(peek_word() == "if");
    const auto l = location();
    word();
    skip_whitespace_and_comments();
    symbol("(");
    auto condition = parse_expression();
    skip_whitespace_and_comments();
    symbol(")");
    skip_whitespace_and_comments();
    auto then_branch = parse_statement();
    skip_whitespace_and_comments();
    if (try_eat("else")) {
      return {l, ast::if_statement{std::move(condition), std::move(then_branch),
                                   parse_statement()}};
    } else {
      return {l, ast::if_statement{
                     std::move(condition), std::move(then_branch), {}}};
    }
  }

  ast::statement parse_while_statement() {
    assert(peek_word() == "while");
    const auto l = location();
    word();
    skip_whitespace_and_comments();
    symbol("(");
    auto condition = parse_expression();
    skip_whitespace_and_comments();
    symbol(")");
    skip_whitespace_and_comments();
    auto body = parse_statement();
    skip_whitespace_and_comments();
    return {l, ast::while_statement{std::move(condition), std::move(body)}};
  }

  ast::statement parse_break_statement() {
    assert(peek_word() == "break");
    const auto l = location();
    word();
    symbol(";");
    return {l, ast::break_statement{}};
  }

  ast::statement parse_continue_statement() {
    assert(peek_word() == "continue");
    const auto l = location();
    word();
    symbol(";");
    return {l, ast::continue_statement{}};
  }

  ast::statement parse_return_statement() {
    assert(peek_word() == "return");
    const auto l = location();
    word();
    if (try_symbol(";")) return {};
    auto value = parse_expression();
    symbol(";");
    return {l, ast::return_statement{std::move(value)}};
  }

  ast::statement parse_exported_definition() {
    assert(peek_word() == "export");
    const auto l = location();
    word();
    return {l, ast::exported_definition{parse_definition()}};
  }

  ast::statement parse_statement() {
    skip_whitespace_and_comments();
    const auto l = location();
    if (peek() == '{') return {l, parse_block()};
    const auto w = peek_word();
    if (w == "if") return parse_if_statement();
    if (w == "while") return parse_while_statement();
    if (w == "break") return parse_break_statement();
    if (w == "continue") return parse_continue_statement();
    if (w == "return") return parse_return_statement();
    if (w == "import") return parse_import_statement();
    if (w == "export") return parse_exported_definition();
    if (w == "var" || w == "type" || w == "function" || w == "class") {
      return {l, parse_definition()};
    }
    auto expression = parse_expression();
    if (try_symbol("=")) {
      auto value = parse_expression();
      try_symbol(";");
      return {l, ast::assignment{std::move(expression), std::move(value)}};
    }
    symbol(";");
    return {l, ast::expression_statement{std::move(expression)}};
  }

  ast::block_statement parse_block() {
    symbol("{");
    std::vector<ast::statement> block;
    while (true) {
      skip_whitespace_and_comments();
      if (try_symbol("}")) return ast::block_statement{std::move(block)};
      block.push_back(parse_statement());
    }
  }

  ast::module parse() {
    std::vector<ast::statement> result;
    while (true) {
      skip_whitespace_and_comments();
      if (eof()) return ast::module{std::move(result)};
      result.push_back(parse_statement());
    }
  }
};

export ast::module parse(const char* filename) {
  return parser{filename}.parse();
}

}  // namespace syntax
