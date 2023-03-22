#pragma once
#include "ast.hpp"
#include "parser.hpp"
#include "bytecode.hpp"

#include <filesystem>
#include <map>
#include <string>

namespace anzu {

auto compile(
    const std::filesystem::path& main_dir,
    const std::map<std::filesystem::path, anzu_module>& modules,
    bool debug = true
) -> bytecode_program;

}