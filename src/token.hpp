#pragma once
#include "vocabulary.hpp"
#include "utility/peekstream.hpp"

#include <string>
#include <format>
#include <vector>

namespace anzu {

enum class lex_token_type
{
    eof,
    placeholder,
    number,
    floating_point,
    identifier,
    string,
    character,
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
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    semicolon,
    comma,
    dot,
    minus,
    plus,
    slash,
    star,
    bang_equal,
    bang,
    equal_equal,
    equal,
    less_equal,
    less,
    greater_equal,
    greater,
    ampersand,
    ampersand_ampersand,
    colon_equal,
    colon,
    left_bracket,
    right_bracket,
    percent,
    bar_bar,
    bar,
    arrow,
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

auto print_tokens(const std::vector<anzu::lex_token>& tokens) -> void;

class tokenstream : public anzu::peekstream<std::vector<lex_token>>
{
public:
    tokenstream(const std::vector<lex_token>& tokens);
    auto consume_maybe(std::string_view text) -> bool;
    auto consume_only(std::string_view text) -> lex_token;
    auto consume_i64() -> std::int64_t;
    auto consume_u64() -> std::uint64_t;

    template <typename Func>
    auto consume_comma_separated_list(std::string_view sentinel, Func&& callback) -> void
    {
        if (consume_maybe(sentinel)) { // Empty list
            return;
        }
        callback(); // Parse first
        while (!peek(sentinel)) {
            consume_only(tk_comma);
            callback();
        }
        consume_only(sentinel);
    }

    // TODO: Rename these and the peekstream functions to be more consistent
    auto peek(std::string_view text) -> bool;
    auto peek_next(std::string_view text) -> bool;
};

using token = lex_token;
using token_type = lex_token_type;
    
}

template <> struct std::formatter<anzu::lex_token_type> : std::formatter<std::string_view> {
    auto format(const anzu::lex_token_type& tt, auto& ctx) {
        return std::formatter<std::string_view>::format(to_string(tt), ctx);
    }
};