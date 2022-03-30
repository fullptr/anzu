#pragma once
#include "ast.hpp"
#include "program.hpp"

namespace anzu {

auto compile(const node_stmt_ptr& root) -> anzu::program;

}