#include "value_stack.hpp"

#include <fmt/format.h>
#include <stack>
#include <string_view>
#include <ranges>
#include <vector>

#include <sstream> // Temp

bool is_int(const std::string& token)
{
    return token.find_first_not_of( "0123456789" ) == std::string::npos;
}

struct scope
{
    // To be used later. Scopes will be used to store named values. Function calls
    // will create new scopes. This is essentially a stack frame.
};

constexpr auto OP_PRINT    = std::string_view{"pr"};
constexpr auto OP_PUSH_INT = std::string_view{"pi"};
constexpr auto OP_ADD      = std::string_view{"+"};

int main()
{
    anzu::value_stack vs;

    // The stack of scopes for names values.
    //std::vector<scope> scopes;

    // Temporary hard coded program.
    std::stringstream program;
    program << "pi 5 pi 6 + pr";

    auto tokens = std::ranges::istream_view<std::string>(program);
    auto it = tokens.begin();
    while (it != tokens.end()) {
        const auto token = *it;

        if (token == OP_PRINT) {
            fmt::print("{}\n", vs.pop());
        }
        else if (token == OP_PUSH_INT) {
            ++it;
            const auto value = *it;
            if (is_int(value)) {
                vs.push(std::stoi(value));
            } else {
                fmt::print("Cannot currently load ints from scopes");
                std::exit(1);
            }
        }
        else if (token == OP_ADD) {
            const auto a = vs.pop();
            const auto b = vs.pop();
            vs.push(a + b);
        }

        ++it;
    }

    if (vs.empty()) {
        fmt::print("OK\n");
    }
}