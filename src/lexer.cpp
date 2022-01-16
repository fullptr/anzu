#include "lexer.hpp"
#include "object.hpp"
#include "print.hpp"

#include <algorithm>
#include <ranges>
#include <fstream>
#include <sstream>

namespace anzu {
namespace {

inline static constexpr auto enumerate = std::views::transform([i=0](const auto& elem) mutable {
    return std::pair(i++, std::cref(elem));
});

auto get_token_type(std::string_view text) -> anzu::token_type
{
    if (keywords.contains(text)) {
        return anzu::token_type::keyword;
    }
    if (bin_ops.contains(text)) {
        return anzu::token_type::bin_op;
    }
    if (symbols.contains(text)) {
        return anzu::token_type::symbol;
    }
    if (anzu::is_int(text)) {
        return anzu::token_type::number;
    }
    return anzu::token_type::name;
}

auto lex_line(std::vector<anzu::token>& tokens, const std::string& line, const int lineno) -> void
{
    std::string text;
    bool parsing_string_literal = false;
    int token_col = 1;

    const auto push_token = [&](token_type type) {
        if (text.empty()) { return; }
        tokens.push_back({
            .text=text, .line=lineno, .col=token_col + 1, .type=type
        });
        text.clear();
    };

    for (auto [col, c] : line | enumerate) {
        if (parsing_string_literal) {
            if (c == '"') { // End of literal
                push_token(token_type::string);
                parsing_string_literal = false;
            }
            else {
                text.push_back(c);
            }
        }
        else if (c == '"') { // Start of literal
            if (!text.empty()) {
                anzu::print("unknown string type: {}\n", text);
                std::exit(1);
            }
            token_col = col;
            parsing_string_literal = true;
        }
        else if (c == '#') {
            break;
        }
        else if (symbols.contains(std::string{c})) {
            push_token(get_token_type(text));
            text += c;
            token_col = col;
            push_token(token_type::symbol);
        }
        else if (!std::isspace(c)) {
            if (text.empty()) {
                token_col = col;
            }
            text += c;
        }
        else {
            push_token(get_token_type(text));
        }
    }

    push_token(get_token_type(text));

    if (parsing_string_literal) {
        anzu::print("lexing failed, string literal not closed\n");
        std::exit(1);
    }
}

}

auto to_string(token_type type) -> std::string
{
    switch (type) {
        break; case token_type::keyword: { return "keyword"; };
        break; case token_type::bin_op:  { return "bin_op"; };
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