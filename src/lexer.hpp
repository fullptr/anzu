#pragma once
#include <vector>
#include <filesystem>

#include "token.hpp"

namespace anzu {

auto lex(const std::filesystem::path& file) -> std::vector<anzu::token>;

}