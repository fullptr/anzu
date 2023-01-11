#include "lexer.hpp"
#include "object.hpp"
#include "vocabulary.hpp"
#include "utility/print.hpp"
#include "utility/peekstream.hpp"

#include <algorithm>
#include <ranges>
#include <fstream>
#include <sstream>
#include <optional>

namespace anzu {
namespace {

using line_iterator = peekstream<std::string>;

auto is_int(std::string_view token) -> bool
{
    return !token.empty() && std::ranges::all_of(token, [](char c) { return std::isdigit(c); });
}

auto is_i32(std::string_view token) -> bool
{
    const auto has_suffix = token.ends_with(tk_i32);
    if (!has_suffix) { return false; }
    token.remove_suffix(tk_i32.size());
    return is_int(token);
}

auto is_i64(std::string_view token) -> bool
{
    if (token.ends_with(tk_i64)) {
        token.remove_suffix(tk_i64.size());
    }
    return is_int(token);
}

auto is_u64(std::string_view token) -> bool
{
    const auto has_suffix = token.ends_with("u");
    if (!has_suffix) {
        return false;
    }
    token.remove_suffix(1);
    return is_int(token);
}

auto is_f64(std::string_view token) -> bool
{
    if (token.ends_with(tk_f64)) {
        token.remove_suffix(tk_f64.size());
    }
    return std::ranges::all_of(token, [](char c) { return std::isdigit(c) || c == '.'; })
        && (std::ranges::count(token, '.') <= 1);
}

auto is_alphanumeric(const line_iterator& iter) -> bool
{
    return iter.valid() && (std::isalnum(iter.curr()) || iter.curr() == '_');
}

auto move_to_next(line_iterator& iter) -> bool
{
    while (iter.valid() && std::isspace(iter.curr())) { iter.consume(); }
    return iter.valid();
}

template <typename... Args>
[[noreturn]] auto lexer_error(
    std::int64_t lineno, std::int64_t col, std::string_view msg, Args&&... args
)
    -> void
{
    const auto formatted_msg = std::format(msg, std::forward<Args>(args)...);
    anzu::print("[ERROR] ({}:{}) {}\n", lineno, col, formatted_msg);
    std::exit(1);
}

auto parse_character_literal(std::int64_t lineno, line_iterator& iter) -> std::string
{
    const auto col = iter.position();
    std::string return_value;
    if (iter.consume_maybe('\\')) {
        switch (iter.consume()) {
            break; case 'n': return_value = "\n"; 
            break; case 't': return_value = "\t"; 
            break; case 'r': return_value = "\r";
            break; default: lexer_error(lineno, col, "unknown escape sequence for character");
        }
    } else {
        return_value = iter.consume();
    }
    if (!iter.consume_maybe('\'')) {
        lexer_error(lineno, col, "character literal not closed");
    }
    return return_value;
}

auto parse_string_literal(std::int64_t lineno, line_iterator& iter) -> std::string
{
    const auto col = iter.position();
    std::string return_value;
    while (true) {
        if (!iter.valid()) {
            lexer_error(lineno, col, "EOF reached before closing string literal");
        }
        else if (iter.consume_maybe('"')) {
            break;
        }
        return_value += iter.consume();
    }
    return return_value;
}

auto try_parse_symbol(line_iterator& iter) -> std::optional<std::string>
{
    if (iter.has_next()) {
        if (const auto pair = std::format("{}{}", iter.curr(), iter.next()); anzu::is_symbol(pair)) {
            iter.consume();
            iter.consume();
            return pair;
        }
    }
    if (const auto single = std::string{iter.curr()}; anzu::is_symbol(single)) {
        iter.consume();
        return single;
    }
    return std::nullopt;
}

auto parse_token(line_iterator& iter) -> std::string
{
    const auto is_digit = std::isdigit(iter.curr());
    std::string return_value;
    while (iter.valid() && (is_alphanumeric(iter) || (is_digit && iter.curr() == '.'))) {
        return_value += iter.consume();
    }
    if (is_digit && iter.valid() && iter.curr() == 'u') { // For unsigned
        return_value += iter.consume();
    }
    return return_value;
};

auto lex_line(
    std::vector<anzu::token>& tokens, const std::string& line, const std::int64_t lineno
)
    -> void
{
    const auto push_token = [&](const std::string& text, std::int64_t col, token_type type) {
        tokens.push_back({ text, lineno, col, type });
    };

    auto iter = line_iterator{line};
    while (move_to_next(iter)) {
        const auto col = iter.position();

        if (iter.consume_maybe('"')) {
            const auto literal = parse_string_literal(lineno, iter);
            push_token(literal, col, token_type::string);
        }
        else if (iter.consume_maybe('\'')) {
            const auto literal = parse_character_literal(lineno, iter);
            push_token(literal, col, token_type::character);
        }
        else if (iter.consume_maybe('#')) {
            return;
        }
        else if (const auto symbol = try_parse_symbol(iter); symbol.has_value()) {
            push_token(*symbol, col, token_type::symbol);
        }
        else {
            const auto token = parse_token(iter);
            if (!token.empty()) {
                if (is_keyword(token)) {
                    push_token(token, col, token_type::keyword);
                }
                else if (is_i32(token)) {
                    push_token(token, col, token_type::i32);
                }
                else if (is_i64(token)) {
                    push_token(token, col, token_type::i64);
                }
                else if (is_u64(token)) {
                    push_token(token, col, token_type::u64);
                }
                else if (is_f64(token)) {
                    push_token(token, col, token_type::f64);
                }
                else if (!std::isdigit(token[0])) {
                    push_token(token, col, token_type::name);
                }
                else {
                    lexer_error(lineno, col, "invalid name '{}' - names cannot start with a digit", token);
                }
            }
            else {
                lexer_error(lineno, col, "could not parse symbol '{}'\n", iter.curr());
            }
        }

    }
}

}

auto lex(const std::string& file) -> std::vector<anzu::token>
{
    // Loop over the lines in the program, and then split each line into tokens.
    // If a '//' comment symbol is hit, the rest of the line is ignored.
    std::vector<anzu::token> tokens;
    std::ifstream file_stream{file};
    std::string line;
    std::int64_t lineno = 1;
    while (std::getline(file_stream, line)) {
        lex_line(tokens, line, lineno);
        ++lineno;
    }
    return tokens;
}

}