#pragma once
#include "token.hpp"
#include "ast.hpp"

#include <vector>
#include <set>
#include <string>
#include <filesystem>

namespace anzu {

struct file_ast
{
    node_stmt_ptr root;

    std::set<std::filesystem::path> required_modules;
};

auto parse(
    const std::filesystem::path& file,
    const std::vector<anzu::token>& token
) -> file_ast;

}