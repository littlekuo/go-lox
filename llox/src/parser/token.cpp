#include "parser/token.h"
#include <sstream>
#include <string>
#include <unordered_map>

const std::unordered_map<TokenType, std::string> TokenTypeStr = {
    {TokenType::TOKEN_LEFT_PAREN, "TOKEN_LEFT_PAREN"},
    {TokenType::TOKEN_RIGHT_PAREN, "TOKEN_RIGHT_PAREN"},
    {TokenType::TOKEN_LEFT_BRACE, "TOKEN_LEFT_BRACE"},
    {TokenType::TOKEN_RIGHT_BRACE, "TOKEN_RIGHT_BRACE"},
    {TokenType::TOKEN_COMMA, "TOKEN_COMMA"},
    {TokenType::TOKEN_DOT, "TOKEN_DOT"},
    {TokenType::TOKEN_MINUS, "TOKEN_MINUS"},
    {TokenType::TOKEN_PLUS, "TOKEN_PLUS"},
    {TokenType::TOKEN_SEMICOLON, "TOKEN_SEMICOLON"},
    {TokenType::TOKEN_SLASH, "TOKEN_SLASH"},
    {TokenType::TOKEN_STAR, "TOKEN_STAR"},

    {TokenType::TOKEN_BANG, "TOKEN_BANG"},
    {TokenType::TOKEN_BANG_EQUAL, "TOKEN_BANG_EQUAL"},
    {TokenType::TOKEN_EQUAL, "TOKEN_EQUAL"},
    {TokenType::TOKEN_EQUAL_EQUAL, "TOKEN_EQUAL_EQUAL"},
    {TokenType::TOKEN_GREATER, "TOKEN_GREATER"},
    {TokenType::TOKEN_GREATER_EQUAL, "TOKEN_GREATER_EQUAL"},
    {TokenType::TOKEN_LESS, "TOKEN_LESS"},
    {TokenType::TOKEN_LESS_EQUAL, "TOKEN_LESS_EQUAL"},

    {TokenType::TOKEN_IDENTIFIER, "TOKEN_IDENTIFIER"},
    {TokenType::TOKEN_STRING, "TOKEN_STRING"},
    {TokenType::TOKEN_NUMBER, "TOKEN_NUMBER"},

    {TokenType::TOKEN_AND, "TOKEN_AND"},
    {TokenType::TOKEN_CLASS, "TOKEN_CLASS"},
    {TokenType::TOKEN_ELSE, "TOKEN_ELSE"},
    {TokenType::TOKEN_FALSE, "TOKEN_FALSE"},
    {TokenType::TOKEN_FUN, "TOKEN_FUN"},
    {TokenType::TOKEN_FOR, "TOKEN_FOR"},
    {TokenType::TOKEN_IF, "TOKEN_IF"},
    {TokenType::TOKEN_NIL, "TOKEN_NIL"},
    {TokenType::TOKEN_OR, "TOKEN_OR"},
    {TokenType::TOKEN_PRINT, "TOKEN_PRINT"},
    {TokenType::TOKEN_RETURN, "TOKEN_RETURN"},
    {TokenType::TOKEN_SUPER, "TOKEN_SUPER"},
    {TokenType::TOKEN_THIS, "TOKEN_THIS"},
    {TokenType::TOKEN_TRUE, "TOKEN_TRUE"},
    {TokenType::TOKEN_VAR, "TOKEN_VAR"},
    {TokenType::TOKEN_WHILE, "TOKEN_WHILE"},
    {TokenType::TOKEN_BREAK, "TOKEN_BREAK"},
    {TokenType::TOKEN_CONTINUE, "TOKEN_CONTINUE"},

    {TokenType::TOKEN_EOF, "TOKEN_EOF"}};

Token::Token(TokenType t, const char *s, int len, int line, int col)
    : type(t), start(s), length(len), line(line), column(col) {}

std::string Token::toString() const {
  std::ostringstream oss;

  auto typeName = TokenTypeStr.find(type) != TokenTypeStr.end()
                      ? TokenTypeStr.at(type)
                      : "UNKNOWN";

  std::string lexeme(start, length);

  oss << typeName << " [" << lexeme << "] at line " << line << ", col "
      << column;

  return oss.str();
}