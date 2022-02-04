#pragma once
#include <vector>
#include <string>

#include "token.hpp"

namespace anzu {

auto lex(const std::string& file) -> std::vector<anzu::token>;

}