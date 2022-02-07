#pragma once
#include "token.hpp"
#include "ast.hpp"

#include <unordered_map>
#include <string>
#include <stack>

namespace anzu {

struct parser_context
{
    anzu::tokenstream tokens;
    std::unordered_map<std::string, function_signature> functions;
    std::stack<std::unordered_map<std::string, std::string>> object_types;
};

auto parse(const std::vector<anzu::token>& tokens) -> anzu::node_stmt_ptr;

}