#include "lexer.hpp"
#include "op_codes.hpp"
#include "object.hpp"

#include <stack>
#include <cstdint>
#include <ranges>
#include <vector>
#include <fstream>
#include <sstream>

namespace anzu::lexer {
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

void process_if_block(
    std::vector<anzu::op>& program,
    std::vector<std::ptrdiff_t> block)
{
    std::ptrdiff_t end_op_ptr = block[0];

    if (block.size() == 3) { // if -> do -> end
        std::ptrdiff_t do_op_ptr   = block[1];

        auto* do_op = std::get_if<anzu::op_block_jump_if_false>(&program[do_op_ptr]);
        if (!do_op) { exit_bad("invalid block at index {}", do_op_ptr); }
        do_op->jump = end_op_ptr + 1;
    }
    else if (block.size() == 4) { // if -> do -> else -> end
        std::ptrdiff_t do_op_ptr   = block[2];
        std::ptrdiff_t else_op_ptr = block[1];

        auto* do_op = std::get_if<anzu::op_block_jump_if_false>(&program[do_op_ptr]);
        if (!do_op) { exit_bad("invalid block at index {}", do_op_ptr); }
        do_op->jump = else_op_ptr + 1;

        auto* else_op = std::get_if<anzu::op_block_jump>(&program[else_op_ptr]);
        if (!else_op) { exit_bad("invalid block at index {}", else_op_ptr); }
        else_op->jump = end_op_ptr + 1;
    }
    else {
        fmt::print("invalid if-statement\n");
        std::exit(1);
    }

    auto* end_op = std::get_if<anzu::op_block_end>(&program[end_op_ptr]);
    if (!end_op) { exit_bad("invalid block at index {}", end_op_ptr); }
    end_op->jump = end_op_ptr + 1;
}

void process_while_block(
    std::vector<anzu::op>& program,
    std::vector<std::ptrdiff_t> block)
{
    if (block.size() == 3) { // while -> do -> end
        std::ptrdiff_t while_op_ptr = block[2];
        std::ptrdiff_t do_op_ptr    = block[1];
        std::ptrdiff_t end_op_ptr   = block[0];

        auto* end_op = std::get_if<anzu::op_block_end>(&program[end_op_ptr]);
        end_op->jump = while_op_ptr;

        auto* do_op = std::get_if<anzu::op_block_jump_if_false>(&program[do_op_ptr]);
        do_op->jump = end_op_ptr + 1;
    }
    else {
        fmt::print("invalid while-statement\n");
        std::exit(1);
    }
}

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
        process_if_block(program, block);
    }
    else if (begin->type == "WHILE") {
        process_while_block(program, block);
    }
    else {
        fmt::print("unknown OP_BLOCK_BEGIN: '{}'", begin->type);
        std::exit(1);
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
    std::stack<std::ptrdiff_t> control_flow;

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
        else if (token == WHILE) {
            control_flow.push(std::ssize(program));
            program.push_back(anzu::op_block_begin{ .type="WHILE" });
        }
        else if (token == IF) {
            control_flow.push(std::ssize(program));
            program.push_back(anzu::op_block_begin{ .type="IF" });
        }
        else if (token == DO) {
            control_flow.push(std::ssize(program));
            program.push_back(anzu::op_block_jump_if_false{ .jump=-1 });
        }
        else if (token == ELSE) {
            control_flow.push(std::ssize(program));
            program.push_back(anzu::op_block_jump{ .type="ELSE", .jump=-1 });
        }
        else if (token == END) {
            control_flow.push(std::ssize(program));
            program.push_back(anzu::op_block_end{ .jump=-1 });
            process_control_block(program, control_flow);
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