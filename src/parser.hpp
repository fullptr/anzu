#pragma once
#include "lexer.hpp"
#include "ast.hpp"
#include "vocabulary.hpp"

#include <vector>
#include <memory>
#include <unordered_map>
#include <string>

namespace anzu {

using token_iterator = std::vector<anzu::token>::const_iterator;

// Context used while constructing an AST. Has non-owning pointers into the
// tokens as well as keeping track of function names.
struct parser_context
{
    struct function_info
    {
        std::int64_t argc;
    };

    anzu::token_iterator       curr;
    const anzu::token_iterator end;

    std::unordered_map<std::string, function_info> functions;
};

auto parse(const std::vector<anzu::token>& tokens) -> anzu::node_stmt_ptr;

}