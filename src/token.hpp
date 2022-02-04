#pragma once
#include <string>
#include <vector>

namespace anzu {

enum class token_type
{
    keyword,
    symbol,
    name,
    number,
    string
};

struct token
{
    std::string text;
    int         line;
    int         col;
    token_type  type;
};

auto to_string(token_type type) -> std::string;
auto print_tokens(const std::vector<anzu::token>& tokens) -> void;
    
}