#include "lexer.hpp"
#include "object.hpp"

#include <fmt/format.h>
#include <algorithm>
#include <ranges>
#include <fstream>
#include <sstream>

namespace anzu {
namespace {

// Returns an iterator to one past the end of the line (ignores comments)
auto end_of_line(const std::string& line) -> std::string::const_iterator
{
    const auto line_end = line.find_first_of("//");
    if (line_end != std::string::npos) {
        auto it = line.begin();
        std::advance(it, line_end);
        return it;
    }
    return line.end();
}

// If there is a bad backspace encountered while lexing, fail and exit.
auto bad_backspace()
{
    fmt::print("Backspace character did not escape anything\n");
    std::exit(1);
};

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
    int col = 1;
    int token_col = 1;

    // Called when the current text should be processed as a token.
    const auto& handle_token = [&]() {
        if (keywords.contains(text)) {
            tokens.push_back({
                .text=text,
                .line=lineno,
                .col=token_col,
                .type=token_type::keyword
            });
            text.clear();
        }
        else if (!text.empty()) {
            tokens.push_back({
                .text=text,
                .line=lineno,
                .col=token_col,
                .type=get_token_type(text)
            });
            text.clear();
        }
    };

    for (auto it = line.begin(); it != end_of_line(line); ++it) {
        if (parsing_string_literal) {
            if (*it == '"') { // End of literal
                tokens.push_back({
                    .text=text,
                    .line=lineno,
                    .col=token_col,
                    .type=token_type::string
                });
                text.clear();
                parsing_string_literal = false;
            }
            else if (*it == '\\') { // Special character
                if (++it == line.end()) { bad_backspace(); }
                switch (*it) {
                    break; case '\\': text.push_back('\\');
                    break; case '"': text.push_back('"');
                    break; case 'n': text.push_back('\n');
                    break; case 't': text.push_back('\t');
                    break; case 'r': text.push_back('\r');
                    break; default: bad_backspace();
                }
            }
            else {
                text.push_back(*it);
            }
        }
        else if (*it == '"') { // Start of literal
            if (!text.empty()) {
                fmt::print("unknown string type: {}\n", text);
                std::exit(1);
            }
            token_col = col;
            parsing_string_literal = true;
        }

        else if (!std::isspace(*it)) {
            if (text.empty()) {
                token_col = col;
            }
            text += *it;
        }

        else {
            handle_token();
        }
        ++col;
    }

    handle_token();

    if (parsing_string_literal) {
        fmt::print("lexing failed, string literal not closed\n");
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