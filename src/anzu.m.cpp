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
constexpr auto OP_PRINT_FRAME  = std::string_view{"frame"};
constexpr auto OP_BEGIN_IF     = std::string_view{"if"};
constexpr auto OP_ELSE_IF      = std::string_view{"else"};
constexpr auto OP_END_IF       = std::string_view{"end"};
constexpr auto OP_EQUALS       = std::string_view{"=="};

std::vector<anzu::opcode> load_program(const std::string& file)
{
    std::ifstream stream{file};
    std::vector<std::string> tokens;
    for (const auto& token : std::ranges::istream_view<std::string>(stream)) {
        tokens.push_back(token);
    }

    std::vector<anzu::opcode> program;
    std::stack<std::size_t> if_stack;

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
        else if (token == OP_BEGIN_IF) {
            if_stack.push(program.size());
            program.push_back(anzu::op::begin_if{
                .jump = -1
            });
        }
        else if (token == OP_ELSE_IF) {
            // Fetch the if from the top of the stack. Set it to jump to one-past this
            // new "else" token so flow enters the else block. Replace the "if" in if
            // stack with this new token so that the end can update it to jump to the end.
            auto index = if_stack.top();
            if (auto* begin = std::get_if<anzu::op::begin_if>(&program[index])) {
                begin->jump = static_cast<int>(program.size() - index + 1);
            } else {
                fmt::print("No if statement to attach to!\n");
                std::exit(1);
            }
            if_stack.top() = program.size();

            program.push_back(anzu::op::else_if{ .jump = -1 });
        }
        else if (token == OP_END_IF) {
            // Get the top element of the if stack. If its an if or an else, make them
            // jump to one past us.
            const auto index = if_stack.top();
            const auto jump = static_cast<int>(program.size() - index + 1);
            if (auto* stmt = std::get_if<anzu::op::begin_if>(&program[index])) {
                stmt->jump = jump;
            } else if (auto* stmt = std::get_if<anzu::op::else_if>(&program[index])) {
                stmt->jump = jump;
            } else {
                fmt::print("No if statement to close!\n");
                std::exit(1);
            }
            if_stack.pop();

            program.push_back(anzu::op::end_if{});
        }
        else if (token == OP_EQUALS) {
            program.push_back(anzu::op::equals{});
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
    std::size_t ptr = 0;

    while (ptr < program.size()) {
        std::visit([&](auto&& o) { ptr += o.apply(frame); }, program[ptr]);
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
    //for (const auto& op : program) { std::visit([](auto&& o) { o.print(); }, op); }

    return 0;
}