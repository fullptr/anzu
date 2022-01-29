#pragma once
#include "ast.hpp"
#include "op_codes.hpp"

#include <memory>
#include <vector>
#include <unordered_map>
#include <string_view>

namespace anzu {

// Struct used to store information while compiling an AST. Contains the output program
// as well as information such as function definitions.
struct compiler_context
{
    struct function_def
    {
        std::vector<std::string> arg_names;
        std::intptr_t ptr;
    };

    std::vector<anzu::op> program;
    std::unordered_map<std::string, function_def> functions;
};

auto compile(const anzu::node_stmt_ptr& root) -> std::vector<anzu::op>;

}