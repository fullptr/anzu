#pragma once
#include "utility/peekstream.hpp"

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

class tokenstream : public anzu::peekstream<std::vector<token>>
{
public:
    tokenstream(const std::vector<token>& tokens);
    auto consume_maybe(std::string_view text) -> bool;
    auto consume_only(std::string_view text) -> void;

    // TODO: Rename these and the peekstream functions to be more consistent
    auto peek(std::string_view text) -> bool;
    auto peek_next(std::string_view text) -> bool;
};
    
}