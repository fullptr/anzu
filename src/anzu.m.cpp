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
#include <sstream>

namespace {

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

}

constexpr auto OP_STORE       = std::string_view{"->"};
constexpr auto OP_DUMP        = std::string_view{"."};
constexpr auto OP_POP         = std::string_view{"pop"};
constexpr auto OP_ADD         = std::string_view{"+"};
constexpr auto OP_SUB         = std::string_view{"-"};
constexpr auto OP_MUL         = std::string_view{"*"};
constexpr auto OP_DIV         = std::string_view{"/"};
constexpr auto OP_MOD         = std::string_view{"%"};
constexpr auto OP_DUP         = std::string_view{"dup"};
constexpr auto OP_PRINT_FRAME = std::string_view{"frame"};
constexpr auto OP_DO          = std::string_view{"do"};
constexpr auto OP_WHILE       = std::string_view{"while"};
constexpr auto OP_IF          = std::string_view{"if"};
constexpr auto OP_ELSE        = std::string_view{"else"};
constexpr auto OP_END         = std::string_view{"end"};
constexpr auto OP_EQ          = std::string_view{"=="};
constexpr auto OP_NE          = std::string_view{"!="};
constexpr auto OP_LT          = std::string_view{"<"};
constexpr auto OP_LE          = std::string_view{"<="};
constexpr auto OP_GT          = std::string_view{">"};
constexpr auto OP_GE          = std::string_view{">="};
constexpr auto OP_OR          = std::string_view{"or"};
constexpr auto OP_AND         = std::string_view{"and"};
constexpr auto OP_INPUT       = std::string_view{"input"};

std::vector<anzu::op> load_program(const std::string& file)
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
            tokens.push_back(token);
        }
    }

    std::vector<anzu::op> program;

    // Contains a stack of indices to previous control flow statements suchs as
    // 'if', 'do' and 'else' so the jumps can be set up correctly.
    std::stack<std::size_t> control_flow;

    auto it = tokens.begin();
    while (it != tokens.end()) {
        const auto& token = *it;

        if (token == OP_STORE) {
            program.push_back(anzu::op_store{
                .name=next(it)
            });
        }
        else if (token == OP_DUMP) {
            program.push_back(anzu::op_dump{});
        }
        else if (token == OP_POP) {
            program.push_back(anzu::op_pop{});
        }
        else if (token == OP_ADD) {
            program.push_back(anzu::op_add{});
        }
        else if (token == OP_SUB) {
            program.push_back(anzu::op_sub{});
        }
        else if (token == OP_MUL) {
            program.push_back(anzu::op_mul{});
        }
        else if (token == OP_DIV) {
            program.push_back(anzu::op_div{});
        }
        else if (token == OP_MOD) {
            program.push_back(anzu::op_mod{});
        }
        else if (token == OP_DUP) {
            program.push_back(anzu::op_dup{});
        }
        else if (token == OP_PRINT_FRAME) {
            program.push_back(anzu::op_print_frame{});
        }
        else if (token == OP_WHILE) {
            control_flow.push(program.size());
            program.push_back(anzu::op_while{});
        }
        else if (token == OP_IF) {
            control_flow.push(program.size());
            program.push_back(anzu::op_if{});
        }
        else if (token == OP_DO) {
            // Verify that the 'do' is preceeded by a valid keyword
            if (control_flow.empty()) {
                fmt::print("'do' does not match any preceeding statement!\n");
                std::exit(1);
            }
            auto index = control_flow.top();
            if (auto* begin = std::get_if<anzu::op_if>(&program[index])) {
                // Pass
            } else if (auto* begin = std::get_if<anzu::op_while>(&program[index])) {
                // Pass
            } else {
                fmt::print("'do' does not match any preceeding statement!\n");
                std::exit(1);
            }

            control_flow.push(program.size());
            program.push_back(anzu::op_do{ .jump=-1 });
            // After this, the top of the control_flow stack is either 'if/do'
            // or 'while/do'.
        }
        else if (token == OP_ELSE) {
            // Fetch the if from the top of the stack. Set it to jump to one-past this
            // new "else" token so flow enters the else block. Replace the "if" in if
            // stack with this new token so that the end can update it to jump to the end.
            if (control_flow.empty()) {
                fmt::print("'else' does not close a preceeding 'do'!\n");
                std::exit(1);
            }
            auto index = control_flow.top();
            if (auto* begin = std::get_if<anzu::op_do>(&program[index])) {
                begin->jump = static_cast<int>(program.size() - index + 1);
            } else {
                fmt::print("'else' does not close a preceeding 'do'!\n");
                std::exit(1);
            }
            control_flow.pop(); // Pop 'do'
            control_flow.push(program.size()); // Push 'else'
            program.push_back(anzu::op_else{ .jump=-1 });
            // After this, the top of the control_flow stack is now 'if/else'.
        }
        else if (token == OP_END) {
            // Get the top element of the if stack. If its an if or an else, make them
            // jump to one past us.
            if (control_flow.size() < 2) {
                fmt::print("'end' does not enclose any control flow block!\n");
                std::exit(1);
            }

            // 'end' is preceeded by either 'if/do', 'if/else', 'while/do'
            const auto idx_clause = control_flow.top();
            control_flow.pop(); // do or else
            
            const auto idx_block = control_flow.top();
            control_flow.pop(); // if or while

            if (auto* op_if = std::get_if<anzu::op_if>(&program[idx_block])) {
                // for 'if/do' and 'if/else', the do and else blocks both jump
                // to one past the end. 
                if (auto* op_do = std::get_if<anzu::op_do>(&program[idx_clause])) {
                    op_do->jump = static_cast<int>(program.size() - idx_clause + 1);
                    program.push_back(anzu::op_end{ .jump=1 });
                }
                else if (auto* op_else = std::get_if<anzu::op_else>(&program[idx_clause])) {
                    // 'if/else' case
                    op_else->jump = static_cast<int>(program.size() - idx_clause + 1);
                    program.push_back(anzu::op_end{ .jump=1 });
                }
                else {
                    fmt::print("'end' does not enclose any control flow block!\n");
                    std::exit(1);
                }
            }
            else if (auto* op_while = std::get_if<anzu::op_while>(&program[idx_block])) {
                if (auto* op_do = std::get_if<anzu::op_do>(&program[idx_clause])) {
                    op_do->jump = static_cast<int>(program.size() - idx_clause + 1);
                    program.push_back(anzu::op_end{
                        .jump=static_cast<int>(idx_block - program.size())
                    });
                }
                else {
                    fmt::print("'end' does not enclose any control flow block!\n");
                    std::exit(1);
                }
            }
            else {
                fmt::print("'end' does not enclose any control flow block!\n");
                std::exit(1);
            }
        }
        else if (token == OP_EQ) {
            program.push_back(anzu::op_eq{});
        }
        else if (token == OP_NE) {
            program.push_back(anzu::op_ne{});
        }
        else if (token == OP_LT) {
            program.push_back(anzu::op_lt{});
        }
        else if (token == OP_LE) {
            program.push_back(anzu::op_le{});
        }
        else if (token == OP_GT) {
            program.push_back(anzu::op_gt{});
        }
        else if (token == OP_GE) {
            program.push_back(anzu::op_ge{});
        }
        else if (token == OP_OR) {
            program.push_back(anzu::op_or{});
        }
        else if (token == OP_AND) {
            program.push_back(anzu::op_and{});
        }
        else if (token == OP_INPUT) {
            program.push_back(anzu::op_input{});
        }
        else if (is_literal(token)) {
            program.push_back(anzu::op_push_const{
                .value=parse_literal(token)
            });
        }
        else {
            program.push_back(anzu::op_push_var{
                .name=token
            });
        }
        ++it;
    }
    return program;
}

void run_program(const std::vector<anzu::op>& program)
{
    anzu::stack_frame frame;

    while (frame.ptr() < program.size()) {
        std::visit([&](auto&& o) { o.apply(frame); }, program[frame.ptr()]);
    }

    if (frame.empty()) {
        fmt::print("OK\n");
    }
}

void print_program(const std::vector<anzu::op>& program)
{
    int lineno = 0;
    for (const auto& op : program) {
        std::visit([&](auto&& o) {
            fmt::print("{:>4} - ", lineno++);
            o.print();
        }, op);
    }
}

void print_usage()
{
    fmt::print("usage: anzu.exe (run|print) <program_file>\n\n");
    fmt::print("The Anzu Programming Language\n\n");
    fmt::print("options:\n");
    fmt::print("    run   - executes the program\n");
    fmt::print("    print - displays the program bytecode\n");
}

int main(int argc, char** argv)
{
    if (argc != 3) {
        print_usage();
        return 1;
    }

    const auto mode = std::string{argv[1]};
    const auto file = std::string{argv[2]};

    fmt::print("loading file '{}'\n", file);
    const auto program = load_program(file);
    
    if (mode == "print") {
        print_program(program);
    }
    else if (mode == "run") {
        run_program(program);
    }
    else {
        fmt::print("unknown mode: '{}'\n", mode);
        print_usage();
        return 1;
    }

    return 0;
}