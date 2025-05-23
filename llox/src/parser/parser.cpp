#include "parser/parser.h"
#include "ast/expr.h"
#include "parser/token.h"

Parser::Parser(std::vector<Token> tokens) : tokens_(std::move(tokens)) {}

std::unique_ptr<Expr> Parser::parse() {
    return parse_expression();
}

std::unique_ptr<Expr> Parser::parse_expression() {
    return parse_equality();
}

std::unique_ptr<Expr> Parser::parse_equality() {
    auto expr = parse_comparison();
    if(expr == nullptr) {
        return nullptr;
    }
    while (match({TokenType::TOKEN_BANG_EQUAL, TokenType::TOKEN_EQUAL_EQUAL})) {
        auto op = previous();
        auto right = parse_comparison();
        if(right == nullptr) {
            return nullptr;
        }
        expr = std::make_unique<BinaryExpr>(std::move(expr), std::move(right), std::move(op));
    }
    return expr;
}    

std::unique_ptr<Expr> Parser::parse_comparison() {
    auto expr = parse_term();
    if(expr == nullptr) {
        return nullptr;
    }
    while (match({TokenType::TOKEN_GREATER, TokenType::TOKEN_GREATER_EQUAL, TokenType::TOKEN_LESS, TokenType::TOKEN_LESS_EQUAL})) {
        auto op = previous();
        auto right = parse_term();
        if (right == nullptr) {
            return nullptr;
        }
        expr = std::make_unique<BinaryExpr>(std::move(expr), std::move(right),  std::move(op));
    }
    return expr;
}

std::unique_ptr<Expr> Parser::parse_term() {
    auto expr = parse_factor();
    if(expr == nullptr) {
        return nullptr;
    }
    while (match({TokenType::TOKEN_MINUS, TokenType::TOKEN_PLUS})) {
        auto op = previous();
        auto right = parse_factor();
        if(right == nullptr) {
            return nullptr;
        }
        expr = std::make_unique<BinaryExpr>(std::move(expr),  std::move(right), std::move(op));
    }
    return expr;
}

std::unique_ptr<Expr> Parser::parse_factor() {
    auto expr = parse_unary();
    if(expr == nullptr) {
        return nullptr;
    }
    while (match({TokenType::TOKEN_SLASH, TokenType::TOKEN_STAR})) {
        auto op = previous();
        auto right = parse_unary();
        if(right == nullptr) {
            return nullptr;
        }
        expr = std::make_unique<BinaryExpr>(std::move(expr), std::move(right), std::move(op));
    }
    return expr;
}

std::unique_ptr<Expr> Parser::parse_unary() {
    if (match({TokenType::TOKEN_BANG, TokenType::TOKEN_MINUS})) {
        auto op = previous();
        auto right = parse_unary();
        if(right == nullptr) {
            return nullptr;
        }
        return std::make_unique<UnaryExpr>(std::move(right), std::move(op));
    }
    return parse_primary();
}

std::unique_ptr<Expr> Parser::parse_primary() {
    if (match({TokenType::TOKEN_FALSE})) {
        return std::make_unique<LiteralExpr>(false, previous());
    }
    if (match({TokenType::TOKEN_TRUE})) {
        return std::make_unique<LiteralExpr>(true, previous());
    }
    if (match({TokenType::TOKEN_NIL})) {
        return std::make_unique<LiteralExpr>(LoxValue(), previous());
    }
    if (match({TokenType::TOKEN_NUMBER})) {
        std::string numStr(previous().start, previous().length);
        double num = std::stod(numStr);
        return std::make_unique<LiteralExpr>(num, previous());
    }
    if (match({TokenType::TOKEN_STRING})) {
        std::string str(previous().start, previous().length);
        return std::make_unique<LiteralExpr>(
            LoxValue(std::move(str)),  
            previous());
    }
    if (match({TokenType::TOKEN_LEFT_PAREN})) {
        auto expr = parse_expression();
        if (!match({TokenType::TOKEN_RIGHT_PAREN})) {
            error(peek(), "expected ')' after expression");
            return nullptr;
        }
        return std::make_unique<GroupingExpr>(std::move(expr));
    }
    error(peek(), "expected expression");
    return nullptr;
}

bool Parser::match(std::vector<TokenType> tkTypes) {
    for (auto& tkType : tkTypes) {
        if (check(tkType)) {
            advance();
            return true;
        }
    }
    return false;
}

bool Parser::check(TokenType type) {
    if (is_at_end()) {
        return false;
    }
    return peek().type == type;
}

Token Parser::advance() {
    if (!is_at_end()) {
        ++current_;
    }
    return tokens_[current_ - 1];
}

Token Parser::previous() {
    return tokens_[current_ - 1];
}

bool Parser::is_at_end() {
    return peek().type == TokenType::TOKEN_EOF;
}

Token Parser::peek() {
    return tokens_[current_];
}

void Parser::error(Token token, std::string message) {
    std::cout << "[line " << token.line << "] " << "[col " << token.column << "] error";
    if (token.type == TokenType::TOKEN_EOF) {
        std::cout << " at end";
    }
    std::cout << " : " << message << std::endl;
    has_error_ = true;
}

void Parser::synchronize() {
    advance();

    while (!is_at_end()) {
      if (previous().type == TokenType::TOKEN_SEMICOLON) return;

      switch (peek().type) {
        case TokenType::TOKEN_CLASS:
        case TokenType::TOKEN_FUN:
        case TokenType::TOKEN_VAR:
        case TokenType::TOKEN_FOR:
        case TokenType::TOKEN_IF:
        case TokenType::TOKEN_WHILE:
        case TokenType::TOKEN_PRINT:
        case TokenType::TOKEN_RETURN:
          return;
        default:
         advance();  
      }

    }
}