#include "lexer.hpp"

#include <ranges>
#include <fstream>
#include <sstream>

namespace anzu {
namespace {

auto lex_line(std::vector<std::string>& tokens, const std::string& line) -> void
{
    std::string token;
    for (auto it = line.begin(); it != line.end(); ++it) {
        if (*it == '#') { // Ignore comments
            break;
        }

        if (!std::isspace(*it)) {
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