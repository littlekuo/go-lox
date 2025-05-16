#ifndef EXPR_H
#define EXPR_H

#include "parser/token.h"
#include <iostream>
#include <memory>
#include <string>
#include <variant>

/**
expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
*/

namespace llvm {
  class Value;
}

using ExprResult = std::variant<double, bool, std::string, llvm::Value*, std::monostate>;

class BinaryExpr;
class GroupingExpr;
class LiteralExpr;
class UnaryExpr;

class ExprVisitor {
public:
  virtual ~ExprVisitor() = default;
  virtual ExprResult visit_binary_expr(const BinaryExpr &expr) = 0;
  virtual ExprResult visit_grouping_expr(const GroupingExpr &expr) = 0;
  virtual ExprResult visit_literal_expr(const LiteralExpr &expr) = 0;
  virtual ExprResult visit_unary_expr(const UnaryExpr &expr) = 0;
};

class Expr {
public:
  virtual ~Expr() = default;
  virtual ExprResult accept(class ExprVisitor &visitor) const = 0;
};

class BinaryExpr : public Expr {
private:
  std::unique_ptr<Expr> left_;
  std::unique_ptr<Expr> right_;
  Token op_;

public:
  BinaryExpr(std::unique_ptr<Expr> left, std::unique_ptr<Expr> right,
             Token&& op);
  const Token& get_op() const;
  const Expr& get_left() const;
  const Expr& get_right() const;
  virtual ExprResult accept(class ExprVisitor &visitor) const override;
};

class GroupingExpr : public Expr {
private:
  std::unique_ptr<Expr> expr_;

public:
  GroupingExpr(std::unique_ptr<Expr> expr);
  const Expr& get_expr() const;
  virtual ExprResult accept(class ExprVisitor &visitor) const override;
};

class LiteralExpr : public Expr {
private:
  ExprResult value_;
  Token token_;

public:
  LiteralExpr(ExprResult&& value, Token&& token);
  const Token& get_token() const;
  const ExprResult& get_value() const;
  virtual ExprResult accept(class ExprVisitor &visitor) const override;
};

class UnaryExpr : public Expr {
private:
  std::unique_ptr<Expr> right_;
  Token op_;

public:
  UnaryExpr(std::unique_ptr<Expr> right, Token&& op);
  const Token& get_op() const;
  const Expr& get_right() const;
  virtual ExprResult accept(class ExprVisitor &visitor) const override;
};

#endif