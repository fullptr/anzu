#include "value_stack.hpp"

#include <fmt/format.h>
#include <array>
#include <stack>
#include <string_view>
#include <ranges>
#include <vector>
#include <unordered_map>
#include <tuple>
#include <utility>
#include <fstream> 

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
constexpr auto OP_STORE_INT = std::string_view{"si"};
constexpr auto OP_ADD       = std::string_view{"+"};
constexpr auto OP_SUB       = std::string_view{"-"};
constexpr auto OP_DUP       = std::string_view{"dup"};

int main(int argc, char** argv)
{
    if (argc != 2) {
        fmt::print("Print usage\n");
        return 0;
    }
    auto file = std::string{argv[1]};
    fmt::print("Running file {}\n", file);
    std::ifstream program{file};

    anzu::value_stack vs;
    std::unordered_map<std::string, int> symbol_table;

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
            const auto b = vs.pop();
            const auto a = vs.pop();
            vs.push(a + b);
        }
        else if (token == OP_SUB) {
            const auto b = vs.pop();
            const auto a = vs.pop();
            vs.push(a - b);
        }
        else if (token == OP_STORE_INT) {
            ++it;
            const auto name = *it;
            ++it;
            const auto value = *it;
            symbol_table[name] = fetch_int(value, symbol_table);
        }
        else if (token == OP_DUP) {
            vs.push(vs.peek());
        }
        else {
            fmt::print("Unknown op code: {}\n", token);
            return 1;
        }

        ++it;
    }

    if (vs.empty()) {
        fmt::print("OK\n");
    }
}