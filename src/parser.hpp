#pragma once
#include "token.hpp"
#include "ast.hpp"

#include <unordered_map>
#include <string>
#include <stack>

namespace anzu {

// A new scope is currently only entered when parsing a function body.
struct scope
{
    std::unordered_map<std::string, function_signature> functions;
    std::unordered_map<std::string, std::string>        variables;
};

struct parser_context
{
    anzu::tokenstream tokens;
    std::stack<scope> scopes;

    auto current_scope() -> scope& { return scopes.top(); }
    auto current_scope() const -> const scope& { return scopes.top(); }
};

auto parse(const std::vector<anzu::token>& tokens) -> anzu::node_stmt_ptr;

}