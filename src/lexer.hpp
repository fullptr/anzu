#pragma once
#include <vector>
#include <filesystem>
#include <format>
#include <cstdint>
#include <string>
#include <memory>
#include <string_view>

#include "token.hpp"

namespace anzu {

struct lex_result
{
    std::filesystem::path        source_file;
    std::unique_ptr<std::string> source_code;
    std::vector<lex_token>       tokens;
};

auto lex(const std::filesystem::path& file) -> lex_result;

auto print_tokens(const lex_result& res) -> void;

}