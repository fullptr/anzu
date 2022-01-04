#include "stack_frame.hpp"
#include "op_codes.hpp"

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

constexpr auto OP_DUMP         = std::string_view{"."};
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

std::vector<anzu::opcode> load_program(const std::string& file)
{
    std::ifstream stream{file};
    std::vector<std::string> tokens;
    for (const auto& token : std::ranges::istream_view<std::string>(stream)) {
        tokens.push_back(token);
    }

    std::vector<anzu::opcode> program;

    auto it = tokens.begin();
    while (it != tokens.end()) {
        const auto& token = *it;

        if (token == OP_DUMP) {
            program.push_back(anzu::op::dump{});
        }
        else if (token == OP_POP) {
            program.push_back(anzu::op::pop{});
        }
        else if (token == OP_PUSH_INT) {
            program.push_back(anzu::op::push_int{
                .value=to_int(next(it))
            });
        }
        else if (token == OP_PUSH_VAR) {
            program.push_back(anzu::op::push_var{
                .name=next(it)
            });
        }
        else if (token == OP_ADD) {
            program.push_back(anzu::op::add{});
        }
        else if (token == OP_SUB) {
            program.push_back(anzu::op::sub{});
        }
        else if (token == OP_STORE_INT) {
            program.push_back(anzu::op::store_int{
                .name=next(it),
                .value=to_int(next(it))
            });
        }
        else if (token == OP_STORE_VAR) {
            program.push_back(anzu::op::store_var{
                .name=next(it),
                .source=next(it)
            });
        }
        else if (token == OP_DUP) {
            program.push_back(anzu::op::dup{});
        }
        else if (token == OP_PRINT_FRAME) {
            program.push_back(anzu::op::print_frame{});
        }
        else {
            fmt::print("Unknown op code: {}\n", token);
            std::exit(1);
        }
        ++it;
    }
    return program;
}

void run_program(const std::vector<anzu::opcode>& program)
{
    anzu::stack_frame frame;

    for (const auto& op : program) {
        std::visit([&](auto&& o) { o.apply(frame); }, op);
    }

    if (frame.empty()) {
        fmt::print("OK\n");
    }
}

int main(int argc, char** argv)
{
    if (argc != 2) {
        fmt::print("Print usage\n");
        return 0;
    }
    auto file = std::string{argv[1]};
    fmt::print("Running file {}\n", file);
    auto program = load_program(file);
    run_program(program);
    return 0;
}