#include "lexer.hpp"

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

auto lex_line(std::vector<std::string>& tokens, const std::string& line) -> void
{
    std::string token;
    bool parsing_string_literal = false;
    for (auto it = line.begin(); it != end_of_line(line); ++it) {
        // Backspace chars escape the next char, which must be of a certain group
        if (*it == '\\') {
            if (++it == line.end()) { bad_backspace(); }
            switch (*it) {
                break; case '\\': token.push_back('\\');
                break; case '"': token.push_back('"');
                break; case 'n': token.push_back('\n');
                break; case 't': token.push_back('\t');
                break; case 'r': token.push_back('\r');
                break; default: bad_backspace();
            }
        }
        else if (parsing_string_literal) {
            if (*it == '"') {
                tokens.push_back(token);
                token.clear();
                parsing_string_literal = false;
            }
            else {
                token.push_back(*it);
            }
        }

        else if (*it == '"') {
            if (!token.empty()) {
                tokens.push_back(token);
                token.clear();
            }
            tokens.push_back("__literal");
            parsing_string_literal = true;
        }

        else if (!std::isspace(*it)) {
            token += *it;
        }

        else if (!token.empty()) {
            tokens.push_back(token);
            token.clear();
        }
    }

    if (!token.empty()) {
        tokens.push_back(token);
    }

    if (parsing_string_literal) {
        fmt::print("lexing failed, string literal not closed\n");
        std::exit(1);
    }
}

}

auto lex(const std::string& file) -> std::vector<std::string>
{
    // Loop over the lines in the program, and then split each line into tokens.
    // If a '//' comment symbol is hit, the rest of the line is ignored.
    std::vector<std::string> tokens;
    std::ifstream file_stream{file};
    std::string line;
    while (std::getline(file_stream, line)) {
        lex_line(tokens, line);
    }
    return tokens;
}

}