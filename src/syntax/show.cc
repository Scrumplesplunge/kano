// Pretty-printing for Kano source AST nodes.

module;

#include <cassert>

export module syntax.show;

import syntax.ast;
import <iostream>;

export namespace syntax::ast {

std::ostream& operator<<(std::ostream&, const identifier&);
std::ostream& operator<<(std::ostream&, literal_integer);
std::ostream& operator<<(std::ostream&, const literal_string&);
std::ostream& operator<<(std::ostream&, const literal_aggregate&);
std::ostream& operator<<(std::ostream&, const array_type&);
std::ostream& operator<<(std::ostream&, const dot&);
std::ostream& operator<<(std::ostream&, const dereference&);
std::ostream& operator<<(std::ostream&, const address_of&);
std::ostream& operator<<(std::ostream&, const index&);
std::ostream& operator<<(std::ostream&, const negate&);
std::ostream& operator<<(std::ostream&, const add&);
std::ostream& operator<<(std::ostream&, const subtract&);
std::ostream& operator<<(std::ostream&, const multiply&);
std::ostream& operator<<(std::ostream&, const divide&);
std::ostream& operator<<(std::ostream&, const modulo&);
std::ostream& operator<<(std::ostream&, const compare_eq&);
std::ostream& operator<<(std::ostream&, const compare_ne&);
std::ostream& operator<<(std::ostream&, const compare_lt&);
std::ostream& operator<<(std::ostream&, const compare_le&);
std::ostream& operator<<(std::ostream&, const compare_gt&);
std::ostream& operator<<(std::ostream&, const compare_ge&);
std::ostream& operator<<(std::ostream&, const logical_and&);
std::ostream& operator<<(std::ostream&, const logical_or&);
std::ostream& operator<<(std::ostream&, const logical_not&);
std::ostream& operator<<(std::ostream&, const call&);

std::ostream& operator<<(std::ostream& output, const identifier& i) {
  return output << i.value;
}

std::ostream& operator<<(std::ostream& output, literal_integer l) {
  return output << l.value;
}

std::ostream& operator<<(std::ostream& output, const literal_string& l) {
  output << '"';
  for (char c : l.value) {
    switch (c) {
      case '\0':
        output << "\\0";
        break;
      case '\"':
        output << "\\\"";
        break;
      case '\\':
        output << "\\\\";
        break;
      case '\n':
        output << "\\n";
        break;
      case '\r':
        output << "\\r";
        break;
      case '\t':
        output << "\\t";
        break;
      default:
        if (isgraph(c)) {
          output << c;
        } else {
          constexpr char hex[] = "0123456789abcdef";
          int low = static_cast<unsigned char>(c) & 0xF;
          int high = static_cast<unsigned char>(c) >> 4;
          output << "\\x" << hex[high] << hex[low];
        }
        break;
    }
  }
  return output << '"';
}

std::ostream& operator<<(std::ostream& output,
                         const literal_aggregate::index_assignment& i) {
  return output << '[' << i.index << "] = " << i.value;
}

std::ostream& operator<<(std::ostream& output,
                         const literal_aggregate::field_assignment& i) {
  return output << '.' << i.field << " = " << i.value;
}

std::ostream& operator<<(std::ostream& output, const literal_aggregate& l) {
  output << '(' << l.type << "){";
  if (l.arguments.empty()) return output << '}';
  std::visit([&](const auto& x) { output << x; }, l.arguments.front());
  for (int i = 1, n = l.arguments.size(); i < n; i++) {
    output << ", ";
    std::visit([&](const auto& x) { output << x; }, l.arguments[i]);
  }
  return output << '}';
}

std::ostream& operator<<(std::ostream& output, const array_type& a) {
  return output << '[' << a.size << "](" << a.element << ")";
}

std::ostream& operator<<(std::ostream& output, const dot& d) {
  return output << '(' << d.from << ")." << d.id;
}

std::ostream& operator<<(std::ostream& output, const dereference& d) {
  return output << "*(" << d.from << ")";
}

std::ostream& operator<<(std::ostream& output, const address_of& a) {
  return output << "&(" << a.inner << ")";
}

std::ostream& operator<<(std::ostream& output, const index& i) {
  return output << '(' << i.from << ")[" << i.index << ']';
}

std::ostream& operator<<(std::ostream& output, const negate& n) {
  return output << "-(" << n.inner << ")";
}

std::ostream& operator<<(std::ostream& output, const add& a) {
  return output << '(' << a.left << ") + (" << a.right << ')';
}

std::ostream& operator<<(std::ostream& output, const subtract& s) {
  return output << '(' << s.left << ") - (" << s.right << ')';
}

std::ostream& operator<<(std::ostream& output, const multiply& m) {
  return output << '(' << m.left << ") * (" << m.right << ')';
}

std::ostream& operator<<(std::ostream& output, const divide& d) {
  return output << '(' << d.left << ") / (" << d.right << ')';
}

std::ostream& operator<<(std::ostream& output, const modulo& m) {
  return output << '(' << m.left << ") % (" << m.right << ')';
}

std::ostream& operator<<(std::ostream& output, const compare_eq& c) {
  return output << '(' << c.left << ") == (" << c.right << ')';
}

std::ostream& operator<<(std::ostream& output, const compare_ne& c) {
  return output << '(' << c.left << ") != (" << c.right << ')';
}

std::ostream& operator<<(std::ostream& output, const compare_lt& c) {
  return output << '(' << c.left << ") < (" << c.right << ')';
}

std::ostream& operator<<(std::ostream& output, const compare_le& c) {
  return output << '(' << c.left << ") <= (" << c.right << ')';
}

std::ostream& operator<<(std::ostream& output, const compare_gt& c) {
  return output << '(' << c.left << ") > (" << c.right << ')';
}

std::ostream& operator<<(std::ostream& output, const compare_ge& c) {
  return output << '(' << c.left << ") >= (" << c.right << ')';
}

std::ostream& operator<<(std::ostream& output, const logical_and& l) {
  return output << '(' << l.left << ") && (" << l.right << ')';
}

std::ostream& operator<<(std::ostream& output, const logical_or& l) {
  return output << '(' << l.left << ") || (" << l.right << ')';
}

std::ostream& operator<<(std::ostream& output, const logical_not& l) {
  return output << "!(" << l.inner << ')';
}

std::ostream& operator<<(std::ostream& output, const call& c) {
  output << '(' << c.callee << ")(";
  if (c.arguments.empty()) return output << ')';
  output << c.arguments.front();
  for (int i = 1, n = c.arguments.size(); i < n; i++) {
    output << ", ";
    output << c.arguments[i];
  }
  return output << ')';
}

std::ostream& operator<<(std::ostream& output, const expression& e) {
  if (!e) return output << "<null>";
  e.visit([&](const auto& x) { output << x; });
  return output;
}

struct statement_printer {
  std::ostream& output;
  int indent;

  void print(const import_statement& i) {
    output << "import ";
    assert(!i.path.empty());
    output << i.path.front();
    for (int j = 1, n = i.path.size(); j < n; j++) {
      output << "." << i.path[j];
    }
    output << ';';
  }

  void print(const variable_definition& v) {
    output << "var " << v.id << " : " << v.type;
    if (v.initializer) {
      output << " = " << *v.initializer;
    }
    output << ';';
  }

  void print(const alias_definition& a) {
    output << "type " << a.id << " : " << a.type << ';';
  }

  void print(const function_definition::parameter& p) {
    output << p.id << " : " << p.type;
  }

  void print(const function_definition& f) {
    output << "function " << f.id << '(';
    if (!f.parameters.empty()) {
      print(f.parameters.front());
      for (int i = 1, n = f.parameters.size(); i < n; i++) {
        output << ", ";
        print(f.parameters[i]);
      }
    }
    output << ") : " << f.return_type << ' ';
    print(f.body);
  }

  void print(const class_definition& c) {
    output << "class " << c.id << " ";
    print(c.body);
  }

  void print(const definition& d) {
    d.visit([this](const auto& x) { print(x); });
  }

  void print(const exported_definition& d) {
    output << "export ";
    print(d.value);
  }

  void print(const assignment& a) {
    output << a.destination << " = " << a.value << ';';
  }

  void print(const if_statement& i) {
    output << "if (" << i.condition << ") ";
    print(i.then_branch);
    if (i.else_branch) {
      output << " else ";
      print(*i.else_branch);
    }
  }

  void print(const while_statement& w) {
    output << "while (" << w.condition << ") ";
    print(w.body);
  }

  void print(const break_statement&) {
    output << "break;";
  }

  void print(const continue_statement&) {
    output << "continue;";
  }

  void print(const return_statement& r) {
    if (r.value) {
      output << "return " << *r.value << ';';
    } else {
      output << "return;";
    }
  }

  void print(const expression_statement& e) {
    output << e.expression << ';';
  }

  void print(const block_statement& b) {
    if (b.statements.empty()) {
      output << "{}";
      return;
    }
    output << "{\n";
    indent += 2;
    for (const auto& s : b.statements) {
      output << std::string(indent, ' ');
      print(s);
      output << "\n";
    }
    indent -= 2;
    output << std::string(indent, ' ') << "}";
  }

  void print(const statement& s) {
    return s.visit([this](const auto& x) { print(x); });
  }

  void print(const module& m) {
    for (const auto& s : m.statements) {
      print(s);
      std::cout << '\n';
    }
  }
};

std::ostream& operator<<(std::ostream& output, const variable_definition& v) {
  statement_printer{output, 0}.print(v);
  return output;
}

std::ostream& operator<<(std::ostream& output, const alias_definition& a) {
  statement_printer{output, 0}.print(a);
  return output;
}

std::ostream& operator<<(std::ostream& output, const function_definition& f) {
  statement_printer{output, 0}.print(f);
  return output;
}

std::ostream& operator<<(std::ostream& output, const class_definition& c) {
  statement_printer{output, 0}.print(c);
  return output;
}

std::ostream& operator<<(std::ostream& output, const assignment& a) {
  statement_printer{output, 0}.print(a);
  return output;
}

std::ostream& operator<<(std::ostream& output, const if_statement& i) {
  statement_printer{output, 0}.print(i);
  return output;
}

std::ostream& operator<<(std::ostream& output, const while_statement& w) {
  statement_printer{output, 0}.print(w);
  return output;
}

std::ostream& operator<<(std::ostream& output, const break_statement& b) {
  statement_printer{output, 0}.print(b);
  return output;
}

std::ostream& operator<<(std::ostream& output, const continue_statement& c) {
  statement_printer{output, 0}.print(c);
  return output;
}

std::ostream& operator<<(std::ostream& output, const return_statement& r) {
  statement_printer{output, 0}.print(r);
  return output;
}

std::ostream& operator<<(std::ostream& output, const expression_statement& e) {
  statement_printer{output, 0}.print(e);
  return output;
}

std::ostream& operator<<(std::ostream& output, const block_statement& b) {
  statement_printer{output, 0}.print(b);
  return output;
}

std::ostream& operator<<(std::ostream& output, const module& m) {
  statement_printer{output, 0}.print(m);
  return output;
}

}  // namespace syntax::ast
