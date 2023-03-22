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

auto read_file(const std::filesystem::path& file) -> std::unique_ptr<std::string>;

class lexer
{
    std::string_view::const_iterator d_start;
    std::string_view::const_iterator d_curr;
    std::string_view::const_iterator d_end;
    std::size_t d_line = 1;
    std::size_t d_col = 1;

    auto valid() const -> bool;
    auto peek() const -> char;
    auto peek_next() const -> char;
    auto advance() -> char;
    auto match(std::string_view expected) -> bool;

    auto make_token(token_type type) const -> token;
    auto make_identifier() -> token;
    auto make_number() -> token;
    auto make_literal(char delimiter, token_type tt) -> token;
    auto make_string() -> token;
    auto make_char() -> token;

public:
    lexer(std::string_view source_code);
    auto get_token() -> token;
};

class tokenstream
{
    lexer d_ctx;
    token d_curr;
    token d_next;

public:
    tokenstream(std::string_view source_code);

    auto valid() const -> bool { return d_curr.type != token_type::eof; }
    auto has_next() const -> bool { return d_next.type != token_type::eof; }

    auto curr() const -> const token& { return d_curr; }
    auto next() const -> const token& { return d_next; }
    auto position() const -> std::int64_t { return 0; }

    auto consume() -> token
    {
        return std::exchange(d_curr, std::exchange(d_next, d_ctx.get_token()));
    }

    auto consume_maybe(token_type tt) -> bool;
    auto consume_only(token_type tt, std::source_location loc = std::source_location::current()) -> token;
    auto consume_i64() -> std::int64_t;
    auto consume_u64() -> std::uint64_t;
    auto peek(token_type tt) -> bool;
    auto peek_next(token_type tt) -> bool;

    template <typename Func>
    auto consume_comma_separated_list(token_type tt, Func&& callback) -> void
    {
        if (consume_maybe(tt)) { // Empty list
            return;
        }
        callback(); // Parse first
        while (!peek(tt)) {
            consume_only(token_type::comma);
            callback();
        }
        consume_only(tt);
    }
};

}