#ifndef PARSER_H
#define PARSER_H

#include "parser/token.h"
#include "ast/expr.h"
#include <vector>

class Parser {
public:
  explicit Parser(std::vector<Token> tokens);
  bool has_error() const { return has_error_; }
  std::unique_ptr<Expr> parse();

private:
  std::vector<Token> tokens_;
  int current_{0};
  bool has_error_{false};

  std::unique_ptr<Expr> parse_expression();
  std::unique_ptr<Expr> parse_equality();
  std::unique_ptr<Expr> parse_comparison();
  std::unique_ptr<Expr> parse_term();
  std::unique_ptr<Expr> parse_factor();
  std::unique_ptr<Expr> parse_unary();
  std::unique_ptr<Expr> parse_primary();

  bool match(std::vector<TokenType> tkTypes);
  bool check(TokenType type);
  Token advance();
  Token previous();
  bool is_at_end();
  Token peek();
  void error(Token token, std::string message);
};

#endif