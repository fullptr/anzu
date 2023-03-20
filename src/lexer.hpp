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

struct lex_context
{
    std::string_view::const_iterator start, curr, end;
    std::size_t line = 1;
    std::size_t col = 1;
};

struct lex_result
{
    std::filesystem::path        source_file;
    std::unique_ptr<std::string> source_code;
    std::vector<token>           tokens;
};

auto lex(const std::filesystem::path& file) -> lex_result;

auto print_tokens(const lex_result& res) -> void;

auto read_file(const std::filesystem::path& file) -> std::unique_ptr<std::string>;
auto lex_start(std::string_view source_code) -> lex_context;
auto lex_next(lex_context& ctx) -> token;
auto lex_print(std::string_view source_code) -> void;

}