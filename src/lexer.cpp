#include "lexer.hpp"
#include "object.hpp"
#include "print.hpp"

#include <algorithm>
#include <ranges>
#include <fstream>
#include <sstream>
#include <optional>

namespace anzu {
namespace {

using string_iter = std::string::const_iterator;

template <typename... Args>
auto lexer_error(std::string_view msg, Args&&... args) -> void
{
    const auto formatted_msg = std::format(msg, std::forward<Args>(args)...);
    anzu::print("[Lexer Error] {}\n", formatted_msg);
    std::exit(1);
}

auto parse_string_literal(string_iter& curr, int& col, string_iter end) -> std::string
{
    std::string return_value;
    ++curr; ++col; // Skip opening
    while (true) {
        if (curr == end) {
            lexer_error("EOF reached before closing string literal");
        }
        else if (*curr == '"') {
            ++curr; ++col;
            break;
        }
        return_value += *curr;
        ++curr; ++col;
    }
    return return_value;
}

auto try_parse_symbol(string_iter& curr, int& col, string_iter end) -> std::optional<std::string>
{
    if (const auto next = std::next(curr); next != end) {
        if (const auto pair = std::format("{}{}", *curr, *next); symbols.contains(pair)) {
            ++curr; ++col;
            ++curr; ++col;
            return pair;
        }
    }
    if (const auto single = std::string{*curr}; symbols.contains(single)) {
        ++curr; ++col;
        return single;
    }
    return std::nullopt;
}

auto parse_token(string_iter& curr, int& col, string_iter end) -> std::string
{
    std::string return_value;
    while (curr != end && (std::isalpha(*curr) || std::isdigit(*curr) || *curr == '_')) {
        return_value += *curr;
        ++curr; ++col;
    }
    return return_value;
};

auto lex_line(std::vector<anzu::token>& tokens, const std::string& line, const int lineno) -> void
{
    auto it = line.begin();
    auto col = 0;
    auto token_col = 0;
    while (it != line.end()) {
        
        while (it != line.end() && std::isspace(*it)) {
            ++it; ++col;
        }
        if (it == line.end()) {
            return;
        }
        token_col = col + 1;

        if (*it == '"') {
            const auto literal = parse_string_literal(it, col, line.end());
            tokens.push_back({
                .text=literal, .line=lineno, .col=token_col, .type=token_type::string
            });
        }
        else if (*it == '#') {
            break;
        }
        else if (const auto sym = try_parse_symbol(it, col, line.end()); sym.has_value()) {
            tokens.push_back({
                .text=*sym, .line=lineno, .col=token_col, .type=token_type::symbol
            });
        }

        const auto token = parse_token(it, col, line.end());
        if (!token.empty()) {
            if (keywords.contains(token)) {
                tokens.push_back({
                    .text=token, .line=lineno, .col=token_col, .type=token_type::keyword
                });
            }
            else if (anzu::is_int(token)) {
                tokens.push_back({
                    .text=token, .line=lineno, .col=token_col, .type=token_type::number
                });
            }
            else if (!std::isdigit(token[0])) {
                tokens.push_back({
                    .text=token, .line=lineno, .col=token_col, .type=token_type::name
                });
            }
            else {
                lexer_error("invalid name '{}' - names cannot start with a digit", token);
            }
        }
    }
}

}

auto to_string(token_type type) -> std::string
{
    switch (type) {
        break; case token_type::keyword: { return "keyword"; };
        break; case token_type::symbol:  { return "symbol"; };
        break; case token_type::name:    { return "name"; };
        break; case token_type::number:  { return "number"; };
        break; case token_type::string:  { return "string"; };
        break; default:                  { return "UNKNOWN"; };
    }
}

auto lex(const std::string& file) -> std::vector<anzu::token>
{
    // Loop over the lines in the program, and then split each line into tokens.
    // If a '//' comment symbol is hit, the rest of the line is ignored.
    std::vector<anzu::token> tokens;
    std::ifstream file_stream{file};
    std::string line;
    int lineno = 1;
    while (std::getline(file_stream, line)) {
        lex_line(tokens, line, lineno);
        ++lineno;
    }
    return tokens;
}

}