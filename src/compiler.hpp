#pragma once
#include "ast.hpp"
#include "program.hpp"
#include "typecheck.hpp"

namespace anzu {

auto compile(const node_stmt_ptr& root, const type_info& types) -> anzu::program;

}