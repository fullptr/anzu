#include "lexer.hpp"
#include "op_codes.hpp"
#include "object.hpp"

#include <stack>
#include <cstdint>
#include <ranges>
#include <vector>
#include <fstream>
#include <sstream>

namespace anzu {
namespace lexer {
namespace {

std::string next(std::vector<std::string>::iterator& it)
{
    return *(++it);
}

template <typename... Args>
void exit_bad(std::string_view format, Args&&... args)
{
    fmt::print(format, std::forward<Args>(args)...);
    std::exit(1);
}

template <typename T>
T pop_top(std::stack<T>& stack)
{
    T ret = stack.top();
    stack.pop();
    return ret;
}

void process_if_block(std::vector<anzu::op>& program, std::stack<std::ptrdiff_t>& stmt_stack)
{
    std::deque<std::ptrdiff_t> block;
    while (!std::get_if<anzu::op_if>(&program[stmt_stack.top()])) {
        block.push_front(pop_top(stmt_stack)); // do/else/elif/end
    }

    auto begin_ptr = pop_top(stmt_stack); // if
    auto end_ptr = block.back();

    for (std::size_t i = 0; i != block.size(); ++i) {
        std::ptrdiff_t ptr = block[i];

        if (auto* op = std::get_if<anzu::op_do>(&program[ptr])) {
            std::ptrdiff_t next_ptr = block[i+1]; // end or else
            op->jump = next_ptr + 1;
        }
        else if (auto* op = std::get_if<anzu::op_elif>(&program[ptr])) {
            op->jump = end_ptr;
        }
        else if (auto* op = std::get_if<anzu::op_else>(&program[ptr])) {
            op->jump = end_ptr;
        }
        else if (auto* op = std::get_if<anzu::op_end_if>(&program[ptr])) {
            // pass
        }
        else {
            exit_bad("unexepected op in if-statement\n");
        }
    }
}

void process_while_block(std::vector<anzu::op>& program, std::stack<std::ptrdiff_t>& stmt_stack)
{
    std::deque<std::ptrdiff_t> block;
    while (!std::get_if<anzu::op_while>(&program[stmt_stack.top()])) {
        block.push_front(pop_top(stmt_stack)); // do/break/continue/end
    }

    auto begin_ptr = pop_top(stmt_stack); // while
    auto end_ptr = block.back();

    for (std::ptrdiff_t ptr : block) {
        if (auto* op = std::get_if<anzu::op_do>(&program[ptr])) {
            op->jump = end_ptr + 1;
        }
        else if (auto* op = std::get_if<anzu::op_break>(&program[ptr])) {
            op->jump = end_ptr + 1;
        }
        else if (auto* op = std::get_if<anzu::op_continue>(&program[ptr])) {
            op->jump = begin_ptr;
        }
        else if (auto* op = std::get_if<anzu::op_end_while>(&program[ptr])) {
            op->jump = begin_ptr;
        }
        else {
            exit_bad("unexepected op in while-statement\n");
        }
    }
}

}

auto parse_file(const std::string& file) -> std::vector<anzu::op>
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
    std::stack<std::ptrdiff_t> if_stack;
    std::stack<std::ptrdiff_t> while_stack;
    std::stack<std::string>    blocks;

    auto it = tokens.begin();
    while (it != tokens.end()) {
        const auto& token = *it;

        if (token == STORE) {
            program.push_back(anzu::op_store{
                .name=next(it)
            });
        }
        else if (token == DUMP) {
            program.push_back(anzu::op_dump{});
        }
        else if (token == POP) {
            program.push_back(anzu::op_pop{});
        }
        else if (token == ADD) {
            program.push_back(anzu::op_add{});
        }
        else if (token == SUB) {
            program.push_back(anzu::op_sub{});
        }
        else if (token == MUL) {
            program.push_back(anzu::op_mul{});
        }
        else if (token == DIV) {
            program.push_back(anzu::op_div{});
        }
        else if (token == MOD) {
            program.push_back(anzu::op_mod{});
        }
        else if (token == DUP) {
            program.push_back(anzu::op_dup{});
        }
        else if (token == PRINT_FRAME) {
            program.push_back(anzu::op_print_frame{});
        }
        else if (token == IF) {
            if_stack.push(std::ssize(program));
            blocks.push("IF");
            program.push_back(anzu::op_if{});
        }
        else if (token == ELIF) {
            if_stack.push(std::ssize(program));
            program.push_back(anzu::op_elif{});
        }
        else if (token == ELSE) {
            if_stack.push(std::ssize(program));
            program.push_back(anzu::op_else{});
        }
        else if (token == WHILE) {
            while_stack.push(std::ssize(program));
            blocks.push("WHILE");
            program.push_back(anzu::op_while{});
        }
        else if (token == BREAK) {
            while_stack.push(std::ssize(program));
            program.push_back(anzu::op_break{});
        }
        else if (token == CONTINUE) {
            while_stack.push(std::ssize(program));
            program.push_back(anzu::op_continue{});
        }
        else if (token == DO) {
            if (blocks.top() == "IF") {
                if_stack.push(std::ssize(program));
            }
            else if (blocks.top() == "WHILE") {
                while_stack.push(std::ssize(program));
            }
            else {
                fmt::print("bad 'do', is not in a control flow block\n");
                std::exit(1);
            }
            program.push_back(anzu::op_do{});
        }
        else if (token == END) {
            if (blocks.top() == "IF") {
                if_stack.push(std::ssize(program));
                program.push_back(anzu::op_end_if{});
                process_if_block(program, if_stack);
            }
            else if (blocks.top() == "WHILE") {
                while_stack.push(std::ssize(program));
                program.push_back(anzu::op_end_while{});
                process_while_block(program, while_stack);
            }
            else {
                fmt::print("bad 'end', is not in a control flow block\n");
                std::exit(1);
            }
            blocks.pop();
        }
        else if (token == EQ) {
            program.push_back(anzu::op_eq{});
        }
        else if (token == NE) {
            program.push_back(anzu::op_ne{});
        }
        else if (token == LT) {
            program.push_back(anzu::op_lt{});
        }
        else if (token == LE) {
            program.push_back(anzu::op_le{});
        }
        else if (token == GT) {
            program.push_back(anzu::op_gt{});
        }
        else if (token == GE) {
            program.push_back(anzu::op_ge{});
        }
        else if (token == OR) {
            program.push_back(anzu::op_or{});
        }
        else if (token == AND) {
            program.push_back(anzu::op_and{});
        }
        else if (token == INPUT) {
            program.push_back(anzu::op_input{});
        }
        else if (anzu::is_literal(token)) {
            program.push_back(anzu::op_push_const{
                .value=anzu::parse_literal(token)
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

}
}