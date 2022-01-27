#pragma once
#include "lexer.hpp"
#include "compiler.hpp"

#include <vector>
#include <memory>
#include <unordered_map>
#include <string>

namespace anzu {

using token_iterator = std::vector<anzu::token>::const_iterator;
using node_ptr       = std::unique_ptr<anzu::node>;

// Context used while constructing an AST. Has non-owning pointers into the
// tokens as well as keeping track of function names.
struct parser_context
{
    struct function_info
    {
        std::int64_t argc;
    };

    token_iterator       curr;
    const token_iterator end;

    std::unordered_map<std::string, function_info> functions;
};
auto parse(const std::vector<anzu::token>& tokens) -> std::unique_ptr<anzu::node>;

}