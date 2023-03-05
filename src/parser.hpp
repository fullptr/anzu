#pragma once
#include "lexer.hpp"
#include "ast.hpp"

#include <vector>
#include <set>
#include <string>
#include <filesystem>

namespace anzu {

struct parse_result
{
    std::filesystem::path        source_file;
    std::unique_ptr<std::string> source_code;

    node_stmt_ptr root;
    std::set<std::filesystem::path> required_modules;
};

auto parse(lex_result&& lex_res) -> parse_result;

}