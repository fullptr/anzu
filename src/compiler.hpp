#pragma once
#include "ast.hpp"
#include "program.hpp"
#include "parser.hpp"

#include <filesystem>
#include <map>
#include <string>

namespace anzu {

auto compile(const std::map<std::filesystem::path, file_ast>& moduiles) -> program;

}