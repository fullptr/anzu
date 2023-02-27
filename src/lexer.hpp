#pragma once
#include <vector>
#include <filesystem>
#include <format>
#include <cstdint>
#include <string>
#include <string_view>

#include "token.hpp"

namespace anzu {

enum class lex_token_type
{
    eof,
    placeholder,
    number,
    identifier,
    kw_assert,
    kw_bool,
    kw_break,
    kw_char,
    kw_continue,
    kw_default,
    kw_delete,
    kw_else,
    kw_f64,
    kw_false,
    kw_for,
    kw_function,
    kw_i32,
    kw_i64,
    kw_if,
    kw_import,
    kw_in,
    kw_loop,
    kw_new,
    kw_null,
    kw_return,
    kw_sizeof,
    kw_struct,
    kw_true,
    kw_typeof,
    kw_u64,
    kw_while,
};

auto to_string(lex_token_type tt) -> std::string_view;

struct lex_token
{
    std::string_view text;
    std::size_t      line;
    std::size_t      col;
    lex_token_type   type;

    [[noreturn]] void error(std::string_view msg) const;

    template <typename... Args>
    [[noreturn]] void error(std::string_view msg, Args&&... args) const
    {
        error(std::format(msg, std::forward<Args>(args)...));
    }

    template <typename... Args>
    void assert(bool condition, std::string_view msg, Args&&... args) const
    {
        if (!condition) error(std::format(msg, std::forward<Args>(args)...));
    }

    template <typename... Args>
    void assert_eq(const auto& lhs, const auto& rhs, std::string_view msg, Args&&... args) const
    {
        if (lhs != rhs) {
            const auto user_msg = std::format(msg, std::forward<Args>(args)...);
            error("{}: expected {}, got {}", user_msg, rhs, lhs);
        }
    }
};

struct lex_result
{
    std::string            source_code;
    std::vector<lex_token> tokens;
};

auto lex(const std::filesystem::path& file) -> lex_result;

auto print_tokens(const lex_result& res) -> void;

}