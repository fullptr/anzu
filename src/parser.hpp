#pragma once
#include "token.hpp"
#include "ast.hpp"

#include <vector>
#include <unordered_set>
#include <string>

namespace anzu {

struct anzu_module
{
    node_stmt_ptr                   root;
    std::unordered_set<std::string> required_modules;
};

auto parse(const std::vector<anzu::token>& tokens) -> anzu_module;

}