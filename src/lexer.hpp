#pragma once
#include <vector>
#include <string>

namespace anzu {

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