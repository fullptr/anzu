#include "lexer.hpp"

#include <ranges>
#include <fstream>
#include <sstream>

namespace anzu {

auto lex(const std::string& file) -> std::vector<std::string>
{
    // Loop over the lines in the program, and then split each line into tokens.
    // If a '//' comment symbol is hit, the rest of the line is ignored.
    std::vector<std::string> tokens;
    std::ifstream file_stream{file};
    std::string line;
    while (std::getline(file_stream, line)) {
        std::istringstream line_stream{line};
        for (const auto& token : std::ranges::istream_view<std::string>(line_stream)
                               | std::views::take_while([](auto&& tok) { return tok != "//"; }))
        {
            tokens.emplace_back(token);
        }
    }
    return tokens;
}

}