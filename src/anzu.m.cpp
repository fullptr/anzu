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

int to_int(const std::string& token)
{
    if (token.find_first_not_of("0123456789") != std::string::npos) {
        fmt::print("[Fatal] Could not parse int: {}\n", token);
    };
    return std::stoi(token);
}

std::string next(std::vector<std::string>::iterator& it)
{
    ++it;
    return *it;
}

std::vector<std::string> load_file(const std::string& file)
{
    std::ifstream program{file};
    std::vector<std::string> tokens;
    for (const auto& token : std::ranges::istream_view<std::string>(program)) {
        tokens.push_back(token);
    }
    return tokens;
}

constexpr auto OP_PRINT        = std::string_view{"pr"};
constexpr auto OP_POP          = std::string_view{"p"};
constexpr auto OP_PUSH_INT     = std::string_view{"pi"};
constexpr auto OP_STORE_INT    = std::string_view{"si"};
constexpr auto OP_PUSH_VAR     = std::string_view{"pv"};
constexpr auto OP_STORE_VAR    = std::string_view{"sv"};
constexpr auto OP_ADD          = std::string_view{"+"};
constexpr auto OP_SUB          = std::string_view{"-"};
constexpr auto OP_DUP          = std::string_view{"dup"};
constexpr auto OP_FUNC_BEGIN   = std::string_view{"func"};
constexpr auto OP_FUNC_END     = std::string_view{"end"};
constexpr auto OP_PRINT_FRAME  = std::string_view{"frame"};


int main(int argc, char** argv)
{
    if (argc != 2) {
        fmt::print("Print usage\n");
        return 0;
    }
    auto file = std::string{argv[1]};
    fmt::print("Running file {}\n", file);
    auto tokens = load_file(file);

    anzu::value_stack frame;

    auto it = tokens.begin();
    while (it != tokens.end()) {
        const auto token = *it;

        if (token == OP_PRINT) {
            fmt::print("{}\n", frame.pop());
        }
        else if (token == OP_POP) {
            frame.pop();
        }
        else if (token == OP_PUSH_INT) {
            const auto value = next(it);
            frame.push(to_int(value));
        }
        else if (token == OP_PUSH_VAR) {
            const auto value = next(it);
            frame.push(frame.fetch(value));
        }
        else if (token == OP_ADD) {
            const auto b = frame.pop();
            const auto a = frame.pop();
            frame.push(a + b);
        }
        else if (token == OP_SUB) {
            const auto b = frame.pop();
            const auto a = frame.pop();
            frame.push(a - b);
        }
        else if (token == OP_STORE_INT) {
            const auto name = next(it);
            const auto value = next(it);
            frame.load(name, to_int(value));
        }
        else if (token == OP_STORE_VAR) {
            const auto name = next(it);
            const auto value = next(it);
            frame.load(name, frame.fetch(value));
        }
        else if (token == OP_DUP) {
            frame.push(frame.peek());
        }
        else if (token == OP_FUNC_BEGIN) {
            std::vector<std::string> function;
            ++it;
            while (*it != OP_FUNC_END) {
                function.push_back(*it);
                ++it;
            }
            fmt::print("Loaded function:\n");
            for (const auto& f : function) {
                fmt::print(" - {}\n", f);
            }
        }
        else if (token == OP_PRINT_FRAME) {
            frame.print();
        }
        else {
            fmt::print("Unknown op code: {}\n", token);
            return 1;
        }

        ++it;
    }

    if (frame.empty()) {
        fmt::print("OK\n");
    }
}