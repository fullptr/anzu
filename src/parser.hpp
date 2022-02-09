#pragma once
#include "token.hpp"
#include "ast.hpp"

#include <vector>

namespace anzu {

auto parse(const std::vector<anzu::token>& tokens) -> anzu::node_stmt_ptr;

}