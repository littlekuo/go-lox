#include "ast/expr.h"

BinaryExpr::BinaryExpr(std::unique_ptr<Expr> left, std::unique_ptr<Expr> right,
                       Token&& op)
    : left_(std::move(left)), right_(std::move(right)), op_(std::move(op)) {}

const Token& BinaryExpr::get_op() const { return op_; }
const Expr& BinaryExpr::get_left() const { return *left_; }
const Expr& BinaryExpr::get_right() const { return *right_; }
ExprResult BinaryExpr::accept(ExprVisitor &visitor) const {
  return visitor.visit_binary_expr(*this);
}

GroupingExpr::GroupingExpr(std::unique_ptr<Expr> expr)
    : expr_(std::move(expr)) {}
const Expr& GroupingExpr::get_expr() const { return *expr_; }
ExprResult GroupingExpr::accept(ExprVisitor &visitor) const {
  return visitor.visit_grouping_expr(*this);
}

LiteralExpr::LiteralExpr(ExprResult&& value, Token&& token)
    : value_(std::move(value)), token_(std::move(token)) {}
const ExprResult& LiteralExpr::get_value() const { return value_; }
const Token& LiteralExpr::get_token() const { return token_; }
ExprResult LiteralExpr::accept(ExprVisitor &visitor) const {
  return visitor.visit_literal_expr(*this);
}

UnaryExpr::UnaryExpr(std::unique_ptr<Expr> right, Token&& op)
    : right_(std::move(right)), op_(std::move(op)) {}
ExprResult UnaryExpr::accept(ExprVisitor &visitor) const {
  return visitor.visit_unary_expr(*this);
}
const Token& UnaryExpr::get_op() const { return op_; }
const Expr& UnaryExpr::get_right() const { return *right_; }