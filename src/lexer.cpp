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

auto drop_front_back(int front, int back)
{
    namespace sv = std::views;
    return sv::drop(front) | sv::reverse | sv::drop(back) | sv::reverse;
}

void process_if_block(std::vector<anzu::op>& program, std::stack<std::ptrdiff_t>& block)
{
    /*
    // We need to first strip out and breaks and continues to pass them back as they
    // are not part of this block.
    std::vector<std::ptrdiff_t> block;
    std::vector<std::ptrdiff_t> ret;
    for (auto ptr : block_raw) {
        if (auto* op = std::get_if<anzu::op_block_jump>(&program[ptr])) {
            if (op->type == "BREAK" || op->type == "CONTINUE") {
                ret.push_back(ptr);
            } else {
                block.push_back(ptr);
            }
        } else {
            block.push_back(ptr);
        }
    }

    std::ptrdiff_t end_op_ptr = block[0];

    if (block.size() == 3) { // if -> do -> end
        std::ptrdiff_t do_op_ptr   = block[1];

        auto* do_op = std::get_if<anzu::op_block_jump_if_false>(&program[do_op_ptr]);
        if (!do_op) { exit_bad("invalid block at index {}\n", do_op_ptr); }
        do_op->jump = end_op_ptr + 1;
    }
    else if (block.size() == 4) { // if -> do -> else -> end
        std::ptrdiff_t do_op_ptr   = block[2];
        std::ptrdiff_t else_op_ptr = block[1];

        auto* do_op = std::get_if<anzu::op_block_jump_if_false>(&program[do_op_ptr]);
        if (!do_op) { exit_bad("invalid block at index {}\n", do_op_ptr); }
        do_op->jump = else_op_ptr + 1;

        auto* else_op = std::get_if<anzu::op_block_jump>(&program[else_op_ptr]);
        if (!else_op) { exit_bad("invalid block at index {}\n", else_op_ptr); }
        else_op->jump = end_op_ptr + 1;
    }
    else {
        fmt::print("invalid if-statement\n");
        std::exit(1);
    }

    auto* end_op = std::get_if<anzu::op_block_end>(&program[end_op_ptr]);
    if (!end_op) { exit_bad("invalid block at index {}\n", end_op_ptr); }
    end_op->jump = end_op_ptr + 1;

    return ret;
    */
}

void process_while_block(std::vector<anzu::op>& program, std::stack<std::ptrdiff_t>& block)
{
/*
    if (block.size() >= 3) { // while -> do -> [ break -> continue -> ...] -> end
        std::ptrdiff_t while_op_ptr = block.back();
        std::ptrdiff_t do_op_ptr    = block.at(block.size() - 2);
        std::ptrdiff_t end_op_ptr   = block.front();

        auto* end_op = std::get_if<anzu::op_block_end>(&program[end_op_ptr]);
        if (!end_op) { exit_bad("invalid end at index {}\n", end_op_ptr); }
        end_op->jump = while_op_ptr;

        auto* do_op = std::get_if<anzu::op_block_jump_if_false>(&program[do_op_ptr]);
        if (!do_op) { exit_bad("invalid do at index {}\n", do_op_ptr); }
        do_op->jump = end_op_ptr + 1;

        for (std::ptrdiff_t ptr : block | drop_front_back(1, 2)) {
            auto* jump_op = std::get_if<anzu::op_block_jump>(&program[ptr]);
            if (!jump_op) { exit_bad("invalid jump at index {}\n", end_op_ptr); }
            if (jump_op->type == "BREAK") {
                jump_op->jump = end_op_ptr + 1;
            }
            else if (jump_op->type == "CONTINUE") {
                jump_op->jump = while_op_ptr;
            }
            else {
                exit_bad("invalid jump type in while loop: {}\n", jump_op->type);
            }
        }
    }
    else {
        fmt::print("invalid while-statement {}\n", block.size());
        std::exit(1);
    }
*/
}

/*
void process_control_block(
    std::vector<anzu::op>& program,
    std::stack<std::ptrdiff_t>& control_flow)
{
    std::vector<std::ptrdiff_t> block;
    while (!control_flow.empty() && !std::get_if<anzu::op_block_begin>(&program[control_flow.top()])) {
        block.push_back(control_flow.top());
        control_flow.pop();
    }

    // Pop the block_start too
    block.push_back(control_flow.top());
    control_flow.pop();

    std::ptrdiff_t begin_ptr = block.back();
    auto* begin = std::get_if<anzu::op_block_begin>(&program[begin_ptr]);
    //auto after_end_ptr = std::size(program);

    if (begin->type == "IF") {
        auto remaining = process_if_block(program, block);
        for (auto ptr : remaining) {
            control_flow.push(ptr);
        }
    }
    else if (begin->type == "WHILE") {
        process_while_block(program, block);
    }
    else {
        fmt::print("unknown OP_BLOCK_BEGIN: '{}'", begin->type);
        std::exit(1);
    }
}
*/

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