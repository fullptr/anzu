#include "value_stack.hpp"

#include <fmt/format.h>
#include <stack>
#include <string_view>
#include <ranges>
#include <vector>
#include <unordered_map>

#include <sstream> // Temp

bool is_int(const std::string& token)
{
    return token.find_first_not_of( "0123456789" ) == std::string::npos;
}

int fetch_int(const std::string& token, const std::unordered_map<std::string, int>& symbols)
{
    if (is_int(token)) {
        return std::stoi(token);
    } else if (symbols.contains(token)) {
        return symbols.at(token);
    } else {
        fmt::print("Error: Unknown int");
        std::exit(1);
    }
}

constexpr auto OP_PRINT     = std::string_view{"pr"};
constexpr auto OP_PUSH_INT  = std::string_view{"pi"};
constexpr auto OP_ADD       = std::string_view{"+"};
constexpr auto OP_STORE_INT = std::string_view{"si"};

int main()
{
    anzu::value_stack vs;

    std::unordered_map<std::string, int> symbol_table;

    // Temporary hard coded program.
    std::stringstream program;
    program << "si tmp 5 si tmp 6 pi tmp pi 9 + pr";

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
            vs.push(fetch_int(value, symbol_table));
        }
        else if (token == OP_ADD) {
            const auto a = vs.pop();
            const auto b = vs.pop();
            vs.push(a + b);
        }
        else if (token == OP_STORE_INT) {
            ++it;
            const auto name = *it;
            ++it;
            const auto value = *it;
            symbol_table[name] = fetch_int(value, symbol_table);
        }

        ++it;
    }

    if (vs.empty()) {
        fmt::print("OK\n");
    }
}