#pragma once
#include "ast.hpp"
#include "program.hpp"
#include "parser.hpp"

#include <filesystem>
#include <map>
#include <string>

namespace anzu {

auto compile(
    const std::filesystem::path& main_dir,
    const std::map<std::filesystem::path, file_ast>& modules
) -> program;

}