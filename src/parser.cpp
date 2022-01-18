#include "parser.hpp"
#include "op_codes.hpp"
#include "object.hpp"
#include "print.hpp"

#include <stack>
#include <cstdint>
#include <ranges>
#include <vector>
#include <fstream>
#include <sstream>
#include <optional>

#include "functions.hpp"

namespace anzu {
namespace {

template <typename... Args>
inline void exit_bad(std::string_view format, Args&&... args)
{
    anzu::print(format, std::forward<Args>(args)...);
    std::exit(1);
}

template <typename T>
inline T pop_top(std::stack<T>& stack)
{
    T ret = stack.top();
    stack.pop();
    return ret;
}

void process_if_block(std::vector<anzu::op>& program, std::stack<std::intptr_t>& stmt_stack)
{
    std::deque<std::intptr_t> block;
    while (!program[stmt_stack.top()].get_if<anzu::op_if>()) {
        block.push_front(pop_top(stmt_stack)); // do/else/elif/end
    }

    auto begin_ptr = pop_top(stmt_stack); // if
    auto end_ptr = block.back();

    for (std::size_t i = 0; i != block.size(); ++i) {
        std::intptr_t ptr = block[i];
        auto& op = program[ptr];

        if (auto* data = op.get_if<anzu::op_do>()) {
            std::intptr_t next_ptr = block[i+1]; // end or else
            data->jump = next_ptr + 1;
        }
        else if (auto* data = op.get_if<anzu::op_elif>()) {
            data->jump = end_ptr + 1;
        }
        else if (auto* data = op.get_if<anzu::op_else>()) {
            data->jump = end_ptr + 1;
        }
        else if (auto* data = op.get_if<anzu::op_if_end>()) {
            // pass
        }
        else {
            exit_bad("unexepected op in if-statement: '{}'\n", op);
        }
    }
}

void process_while_block(std::vector<anzu::op>& program, std::stack<std::intptr_t>& stmt_stack)
{
    std::deque<std::intptr_t> block;
    while (!program[stmt_stack.top()].get_if<anzu::op_while>()) {
        block.push_front(pop_top(stmt_stack)); // do/break/continue/end
    }

    auto begin_ptr = pop_top(stmt_stack); // while
    auto end_ptr = block.back();

    for (std::intptr_t ptr : block) {
        auto& op = program[ptr];

        if (auto* data = op.get_if<anzu::op_do>()) {
            data->jump = end_ptr + 1;
        }
        else if (auto* data = op.get_if<anzu::op_break>()) {
            data->jump = end_ptr + 1;
        }
        else if (auto* data = op.get_if<anzu::op_continue>()) {
            data->jump = begin_ptr;
        }
        else if (auto* data = op.get_if<anzu::op_while_end>()) {
            data->jump = begin_ptr;
        }
        else {
            exit_bad("unexepected op in while-statement: '{}'\n", op);
        }
    }
}

auto handle_expression(
    std::vector<anzu::op>& program,
    std::vector<anzu::token>::const_iterator& it
)
    -> void
{

}

auto handle_list_literal(
    std::vector<anzu::op>& program,
    std::vector<anzu::token>::const_iterator& it
)
    -> void
{
    auto list = std::make_shared<std::vector<anzu::object>>();

    ++it; // Skip "["
    while (it->text != "]") {
        if (it->type == token_type::string) {
            list->push_back(it->text);
        }
        else if (it->type == token_type::number) {
            list->push_back(anzu::to_int(it->text));
        }
        else if (it->text == TRUE_LIT) {
            list->push_back(true);
        }
        else if (it->text == FALSE_LIT) {
            list->push_back(false);
        }
        else if (it->text == ",") {
            // Pass, delimiters currently optional, will enforce after this workes
        }
        else {
            anzu::print("could not recognise token while parsing list literal: {}\n", it->text);
            std::exit(1);
        }
        ++it;
    }
    ++it; // Skip "]"

    program.emplace_back(anzu::op_push_const{ .value=list });
}

}

struct function_def
{
    int            argc;
    int            retc;
    std::intptr_t ptr;
};

auto parse(const std::vector<anzu::token>& tokens) -> std::vector<anzu::op>
{
    std::vector<anzu::op> program;

    // Contains a stack of indices to previous control flow statements suchs as
    // 'if', 'do' and 'else' so the jumps can be set up correctly.
    std::stack<std::intptr_t> if_stack;
    std::stack<std::intptr_t> while_stack;

    // Functions info
    std::optional<std::string> curr_func;
    std::unordered_map<std::string, function_def> all_functions;

    // Keeps a stack of if/else/function blocks to handle 'end' and 'do' keywords
    std::stack<std::string> blocks;

    auto it = tokens.begin();
    while (it != tokens.end()) {
        const auto& token = it->text;

        if (it->type == token_type::string) {
            program.emplace_back(anzu::op_push_const{
                .value=it->text
            });
        }
        else if (it->type == token_type::number) {
            program.emplace_back(anzu::op_push_const{
                .value=anzu::to_int(it->text)
            });
        }
        else if (it->type == token_type::symbol) {
            if (it->text == "(") {
                handle_expression(program, it);
            }
            else if (it->text == "[") {
                handle_list_literal(program, it);
            }
            else {
                anzu::print("Could not handle symbol '{}'\n", it->text);
                std::exit(1);
            }
        }

        // Stack Manipulation
        else if (token == POP) {
            program.emplace_back(anzu::op_pop{});
        }
        else if (token == DUP) {
            program.emplace_back(anzu::op_dup{});
        }
        else if (token == SWAP) {
            program.emplace_back(anzu::op_swap{});
        }
        else if (token == ROT) {
            program.emplace_back(anzu::op_rot{});
        }
        else if (token == OVER) {
            program.emplace_back(anzu::op_over{});
        }

        // Store Manipulation
        else if (token == STORE) {
            program.emplace_back(anzu::op_store{ .name=(++it)->text });
        }

        // Control Flow
        else if (token == IF) {
            blocks.push("IF");
            if_stack.push(std::ssize(program));
            program.emplace_back(anzu::op_if{});
        }
        else if (token == ELIF) {
            if_stack.push(std::ssize(program));
            program.emplace_back(anzu::op_elif{});
        }
        else if (token == ELSE) {
            if_stack.push(std::ssize(program));
            program.emplace_back(anzu::op_else{});
        }
        else if (token == WHILE) {
            blocks.push("WHILE");
            while_stack.push(std::ssize(program));
            program.emplace_back(anzu::op_while{});
        }
        else if (token == BREAK) {
            while_stack.push(std::ssize(program));
            program.emplace_back(anzu::op_break{});
        }
        else if (token == CONTINUE) {
            while_stack.push(std::ssize(program));
            program.emplace_back(anzu::op_continue{});
        }
        else if (token == FUNCTION) {
            blocks.push("FUNCTION");
            if (curr_func.has_value()) {
                anzu::print(
                    "syntax error line {} col {}: cannot nest functions, '{}' has not been completed\n",
                    it->line,
                    it->col,
                    *curr_func);
                std::exit(1);
            }
            std::string name = (++it)->text;
            curr_func = name;

            function_def def = {
                .argc=anzu::to_int((++it)->text),
                .retc=anzu::to_int((++it)->text),
                .ptr=std::ssize(program)
            };
            all_functions.emplace(name, def);
            program.emplace_back(anzu::op_function{ .name=name });
        }
        else if (token == RETURN) {
            const auto& def = all_functions[*curr_func];
            program.emplace_back(anzu::op_return{ .retc=def.retc });
        }
        else if (token == DO) {
            if (blocks.top() == "IF") {
                if_stack.push(std::ssize(program));
            }
            else if (blocks.top() == "WHILE") {
                while_stack.push(std::ssize(program));
            }
            else {
                anzu::print("bad 'do', is not in a control flow block\n");
                std::exit(1);
            }
            program.emplace_back(anzu::op_do{});
        }
        else if (token == END) {
            if (blocks.top() == "IF") {
                if_stack.push(std::ssize(program));
                program.emplace_back(anzu::op_if_end{});
                process_if_block(program, if_stack);
            }
            else if (blocks.top() == "WHILE") {
                while_stack.push(std::ssize(program));
                program.emplace_back(anzu::op_while_end{});
                process_while_block(program, while_stack);
            }
            else if (blocks.top() == "FUNCTION") {
                auto def = all_functions[*curr_func];
                curr_func.reset();
                auto func = program[def.ptr].get_if<anzu::op_function>();
                func->jump = std::ssize(program) + 1;
                program.emplace_back(anzu::op_function_end{ .retc=def.retc });
            }
            else {
                anzu::print("bad 'end', is not in a control flow block\n");
                std::exit(1);
            }
            blocks.pop();
        }

        // Numerical Operators
        else if (token == ADD) {
            program.emplace_back(anzu::op_add{});
        }
        else if (token == SUB) {
            program.emplace_back(anzu::op_sub{});
        }
        else if (token == MUL) {
            program.emplace_back(anzu::op_mul{});
        }
        else if (token == DIV) {
            program.emplace_back(anzu::op_div{});
        }
        else if (token == MOD) {
            program.emplace_back(anzu::op_mod{});
        }

        // Logical Operators
        else if (token == EQ) {
            program.emplace_back(anzu::op_eq{});
        }
        else if (token == NE) {
            program.emplace_back(anzu::op_ne{});
        }
        else if (token == LT) {
            program.emplace_back(anzu::op_lt{});
        }
        else if (token == LE) {
            program.emplace_back(anzu::op_le{});
        }
        else if (token == GT) {
            program.emplace_back(anzu::op_gt{});
        }
        else if (token == GE) {
            program.emplace_back(anzu::op_ge{});
        }
        else if (token == OR) {
            program.emplace_back(anzu::op_or{});
        }
        else if (token == AND) {
            program.emplace_back(anzu::op_and{});
        }

        // IO
        else if (token == INPUT) {
            program.emplace_back(anzu::op_input{});
        }
        else if (token == DUMP) {
            program.emplace_back(anzu::op_dump{});
        }

        // Literals
        else if (anzu::is_int(token)) {
            program.emplace_back(anzu::op_push_const{
                .value=anzu::to_int(token)
            });
        }
        else if (token == TRUE_LIT) {
            program.emplace_back(anzu::op_push_const{
                .value=true
            });
        }
        else if (token == FALSE_LIT) {
            program.emplace_back(anzu::op_push_const{
                .value=false
            });
        }

        // Casts
        else if (token == TO_INT) {
            program.emplace_back(anzu::op_to_int{});
        }
        else if (token == TO_BOOL) {
            program.emplace_back(anzu::op_to_bool{});
        }
        else if (token == TO_STR) {
            program.emplace_back(anzu::op_to_str{});
        }

        // Rest
        else if (all_functions.contains(token)) {
            auto [argc, retc, ptr] = all_functions[token];
            program.emplace_back(anzu::op_function_call{
                .name=token, .argc=argc, .jump=ptr + 1
            });
        }
        else if (anzu::is_builtin(token)) {
            program.emplace_back(anzu::op_builtin_function_call{
                .name=token
            });
        }
        else {
            program.emplace_back(anzu::op_push_var{
                .name=token
            });
        }
        ++it;
    }

    if (!blocks.empty()) {
        anzu::print("syntax error: not all blocks have been closed\n");
        std::exit(1);
    }
    return program;
}

}