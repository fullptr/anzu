#pragma once
#include <vector>
#include <string>

namespace anzu {

struct token
{
    std::string text;
    int         line;
    int         col;
};

auto lex(const std::string& file) -> std::vector<anzu::token>;

}