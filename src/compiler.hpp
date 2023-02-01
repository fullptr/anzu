#pragma once
#include "ast.hpp"
#include "program.hpp"
#include "parser.hpp"

#include <map>
#include <string>

namespace anzu {

auto compile(const std::map<std::string, file_ast>& moduiles) -> program;

}