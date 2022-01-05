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

bool is_literal(const std::string& token)
{
    return token == "false"
        || token == "true"
        || token.find_first_not_of("0123456789") == std::string::npos;
}

anzu::stack_frame::type parse_literal(const std::string& token)
{
    if (token == "true") {
        return true;
    }
    if (token == "false") {
        return false;
    }
    if (token.find_first_not_of("0123456789") == std::string::npos) {
        return std::stoi(token);
    }
    fmt::print("[Fatal] Could not parse constant: {}\n", token);
    std::exit(1);
}

std::string next(std::vector<std::string>::iterator& it)
{
    ++it;
    return *it;
}

constexpr auto OP_DUMP         = std::string_view{"."};
constexpr auto OP_STORE        = std::string_view{"let"};
constexpr auto OP_POP          = std::string_view{"pop"};
constexpr auto OP_ADD          = std::string_view{"+"};
constexpr auto OP_SUB          = std::string_view{"-"};
constexpr auto OP_DUP          = std::string_view{"dup"};
constexpr auto OP_PRINT_FRAME  = std::string_view{"frame"};
constexpr auto OP_BEGIN_IF     = std::string_view{"if"};
constexpr auto OP_DO           = std::string_view{"do"};
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

    // Contains a stack of indices to previous control flow statements suchs as
    // 'if', 'do' and 'else' so the jumps can be set up correctly.
    std::stack<std::size_t> control_flow;

    auto it = tokens.begin();
    while (it != tokens.end()) {
        const auto& token = *it;

        if (token == OP_DUMP) {
            program.push_back(anzu::op::dump{});
        }
        else if (token == OP_POP) {
            program.push_back(anzu::op::pop{});
        }
        else if (token == OP_STORE) {
            auto name = next(it);
            auto val = next(it);
            if (is_literal(val)) {
                program.push_back(anzu::op::store_const{
                    .name=name,
                    .value=parse_literal(val)
                });
            } else {
                program.push_back(anzu::op::store_var{
                    .name=name,
                    .source=val
                });
            }
        }
        else if (token == OP_ADD) {
            program.push_back(anzu::op::add{});
        }
        else if (token == OP_SUB) {
            program.push_back(anzu::op::sub{});
        }
        else if (token == OP_DUP) {
            program.push_back(anzu::op::dup{});
        }
        else if (token == OP_PRINT_FRAME) {
            program.push_back(anzu::op::print_frame{});
        }
        else if (token == OP_BEGIN_IF) {
            control_flow.push(program.size());
            program.push_back(anzu::op::begin_if{});
        }
        else if (token == OP_DO) {
            if (control_flow.empty()) {
                fmt::print("'do' does not match any preceeding statement!\n");
                std::exit(1);
            }
            auto index = control_flow.top();
            if (auto* begin = std::get_if<anzu::op::begin_if>(&program[index])) {
                control_flow.top() = program.size(); // Replace the top of the stack with this.
            } else {
                fmt::print("'do' does not match any preceeding statement!\n");
                std::exit(1);
            }

            control_flow.push(program.size());
            program.push_back(anzu::op::do_stmt{
                .jump = -1
            });
        }
        else if (token == OP_ELSE_IF) {
            // Fetch the if from the top of the stack. Set it to jump to one-past this
            // new "else" token so flow enters the else block. Replace the "if" in if
            // stack with this new token so that the end can update it to jump to the end.
            if (control_flow.empty()) {
                fmt::print("'else' does not close a preceeding 'do'!\n");
                std::exit(1);
            }
            auto index = control_flow.top();
            if (auto* begin = std::get_if<anzu::op::do_stmt>(&program[index])) {
                begin->jump = static_cast<int>(program.size() - index + 1);
            } else {
                fmt::print("'else' does not close a preceeding 'do'!\n");
                std::exit(1);
            }
            control_flow.top() = program.size(); // Replace the top of the stack with this.

            program.push_back(anzu::op::else_if{ .jump = -1 });
        }
        else if (token == OP_END_IF) {
            // Get the top element of the if stack. If its an if or an else, make them
            // jump to one past us.
            if (control_flow.empty()) {
                fmt::print("'end' does not enclose any control flow block!\n");
                std::exit(1);
            }
            const auto index = control_flow.top();
            const auto jump = static_cast<int>(program.size() - index + 1);
            if (auto* stmt = std::get_if<anzu::op::do_stmt>(&program[index])) {
                stmt->jump = jump;
            } else if (auto* stmt = std::get_if<anzu::op::else_if>(&program[index])) {
                stmt->jump = jump;
            } else {
                fmt::print("'end' does not enclose any control flow block!\n");
                std::exit(1);
            }
            control_flow.pop();

            program.push_back(anzu::op::end_if{});
        }
        else if (token == OP_EQUALS) {
            program.push_back(anzu::op::equals{});
        }
        else if (is_literal(token)) {
            program.push_back(anzu::op::push_const{
                .value=parse_literal(token)
            });
        }
        else {
            program.push_back(anzu::op::push_var{
                .name=token
            });
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
    
    //run_program(program);
    for (const auto& op : program) { std::visit([](auto&& o) { o.print(); }, op); }

    return 0;
}