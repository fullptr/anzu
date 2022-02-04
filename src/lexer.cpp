#include "lexer.hpp"
#include "object.hpp"
#include "vocabulary.hpp"
#include "utility/print.hpp"

#include <algorithm>
#include <ranges>
#include <fstream>
#include <sstream>
#include <optional>

namespace anzu {
namespace {

using string_iter = std::string::const_iterator;

class line_iterator
{
    string_iter d_curr;
    string_iter d_end;
    int         d_col;

public:
    line_iterator(const std::string& line)
        : d_curr(line.begin()) , d_end(line.end()) , d_col(1)
    {
    }

    auto valid() const -> bool { return d_curr != d_end; }
    auto has_next() const -> bool { return std::next(d_curr) != d_end; }

    auto curr() const -> char { return *d_curr; }
    auto next() const -> char { return *std::next(d_curr); }
    auto col() const -> int { return d_col; }

    auto is_alphanumeric() const -> bool
    {
        return valid() && (std::isalpha(curr()) || std::isdigit(curr()) || curr() == '_');
    }

    auto consume() -> char
    {
        auto ret = curr();
        ++d_curr;
        ++d_col;
        return ret;
    }

    auto consume_maybe(char c) -> bool
    {
        if (curr() == c) {
            consume();
            return true;
        }
        return false;
    };
    
    auto move_to_next() -> bool
    {
        while (valid() && std::isspace(curr())) { consume(); }
        return valid();
    }
};


template <typename... Args>
[[noreturn]] void lexer_error(int lineno, int col, std::string_view msg, Args&&... args)
{
    const auto formatted_msg = std::format(msg, std::forward<Args>(args)...);
    anzu::print("[Lexer] ({}:{}) ERROR: {}\n", lineno, col, formatted_msg);
    std::exit(1);
}

auto parse_string_literal(int lineno, line_iterator& iter) -> std::string
{
    const auto col = iter.col() - 1;
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
    std::string return_value;
    while (iter.is_alphanumeric()) {
        return_value += iter.consume();
    }
    return return_value;
};

auto lex_line(std::vector<anzu::token>& tokens, const std::string& line, const int lineno) -> void
{
    const auto push_token = [&](const std::string& text, int col, token_type type) {
        tokens.push_back({ .text=text, .line=lineno, .col=col, .type=type });
    };

    auto iter = line_iterator{line};
    while (iter.move_to_next()) {
        const int col = iter.col();

        if (iter.consume_maybe('"')) {
            const auto literal = parse_string_literal(lineno, iter);
            push_token(literal, col, token_type::string);
        }
        else if (iter.consume_maybe('#')) {
            return;
        }
        else if (const auto symbol = try_parse_symbol(iter); symbol.has_value()) {
            push_token(*symbol, col, token_type::symbol);
        }

        const auto token = parse_token(iter);
        if (!token.empty()) {
            if (anzu::is_keyword(token)) {
                push_token(token, col, token_type::keyword);
            }
            else if (anzu::is_int(token)) {
                push_token(token, col, token_type::number);
            }
            else if (!std::isdigit(token[0])) {
                push_token(token, col, token_type::name);
            }
            else {
                lexer_error(lineno, col, "invalid name '{}' - names cannot start with a digit", token);
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
    int lineno = 1;
    while (std::getline(file_stream, line)) {
        lex_line(tokens, line, lineno);
        ++lineno;
    }
    return tokens;
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

auto print_tokens(const std::vector<anzu::token>& tokens) -> void
{
    for (const auto& token : tokens) {
        const auto text = std::format("'{}'", token.text);
        anzu::print(
            "{:<10} - {:<20} {:<5} {:<5}\n",
            anzu::to_string(token.type), text, token.line, token.col
        );
    }
}

}