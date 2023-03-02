#pragma once
#include <string>
#include <format>
#include <source_location>
#include <vector>

namespace anzu {

enum class lex_token_type
{
    int32,
    int64,
    uint64,
    float64,
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

class tokenstream
{
    std::vector<lex_token>::const_iterator d_begin;
    std::vector<lex_token>::const_iterator d_curr;
    std::vector<lex_token>::const_iterator d_end;

public:
    tokenstream(const std::vector<lex_token>& tokens);

    auto valid() const -> bool { return d_curr != d_end; }
    auto has_next() const -> bool { return valid() && std::next(d_curr) != d_end; }

    auto curr() const -> const lex_token& { return *d_curr; }
    auto next() const -> const lex_token& { return *std::next(d_curr); }
    auto position() const -> std::int64_t { return std::distance(d_begin, d_curr) + 1; }

    auto consume() -> lex_token
    {
        auto ret = curr();
        ++d_curr;
        return ret;
    }

    auto consume_maybe(lex_token_type tt) -> bool;
    auto consume_only(lex_token_type tt, std::source_location loc = std::source_location::current()) -> lex_token;
    auto consume_i64() -> std::int64_t;
    auto consume_u64() -> std::uint64_t;
    auto peek(lex_token_type tt) -> bool;
    auto peek_next(lex_token_type tt) -> bool;

    template <typename Func>
    auto consume_comma_separated_list(lex_token_type tt, Func&& callback) -> void
    {
        if (consume_maybe(tt)) { // Empty list
            return;
        }
        callback(); // Parse first
        while (!peek(tt)) {
            consume_only(lex_token_type::comma);
            callback();
        }
        consume_only(tt);
    }
};

using token = lex_token;
using token_type = lex_token_type;
    
}

template <> struct std::formatter<anzu::lex_token_type> : std::formatter<std::string_view> {
    auto format(const anzu::lex_token_type& tt, auto& ctx) {
        return std::formatter<std::string_view>::format(to_string(tt), ctx);
    }
};