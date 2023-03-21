#pragma once
#include "lexer.hpp"
#include "ast.hpp"

#include <vector>
#include <set>
#include <string>
#include <filesystem>

namespace anzu {

struct anzu_module
{
    std::unique_ptr<std::string> source_code; // TODO: make this a std::unique_ptr<char[]>
    std::set<std::filesystem::path> required_modules;
    node_stmt_ptr root;
};

auto parse(const std::filesystem::path& file) -> anzu_module;

}