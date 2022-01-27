#pragma once
#include <vector>
#include <string>
#include <unordered_set>

namespace anzu {

static const std::unordered_set<std::string_view> keywords = {
    "if",
    "elif",
    "else",
    "while",
    "break",
    "continue",
    "do",
    "end",
    "true",
    "false",
    "function",
    "return"
};

static const std::unordered_set<std::string_view> symbols = {
    "+", "-", "*", "/", "%", "=", "(",  ")", ":", "[", "]", ",", ".",
    "==", "!=", "<", "<=", ">", ">=", "||", "&&"
};

enum class token_type
{
    keyword,
    symbol,
    name,
    number,
    string
};
auto to_string(token_type type) -> std::string;

struct token
{
    std::string text;
    int         line;
    int         col;
    token_type  type;
};

auto lex(const std::string& file) -> std::vector<anzu::token>;

}