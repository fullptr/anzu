#pragma once
#include <vector>
#include <string>

namespace anzu {

auto lex(const std::string& file) -> std::vector<std::string>;

}