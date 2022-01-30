#pragma once
#include "lexer.hpp"
#include "ast.hpp"

namespace anzu {

auto parse(const std::vector<anzu::token>& tokens) -> anzu::node_stmt_ptr;

}