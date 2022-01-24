#include "stack_frame.hpp"
#include "op_codes.hpp"
#include "lexer.hpp"
#include "parser.hpp"
#include "print.hpp"
#include "ast.hpp"

#include <string>
#include <variant>

void print_tokens(const std::vector<anzu::token>& tokens)
{
    for (const auto& token : tokens) {
        const auto text = std::format("'{}'", token.text);
        anzu::print(
            "{:<10} - {:<20} {:<5} {:<5}\n",
            anzu::to_string(token.type), text, token.line, token.col
        );
    }
}

void print_program(const std::vector<anzu::op>& program)
{
    int lineno = 0;
    for (const auto& op : program) {
        anzu::print("{:>4} - {}\n", lineno++, op);
    }
}

void run_program(const std::vector<anzu::op>& program)
{
    anzu::context ctx;
    ctx.push({});

    while (ctx.top().ptr() < std::ssize(program)) {
        program[ctx.top().ptr()].apply(ctx);
    }
}

void run_program_debug(const std::vector<anzu::op>& program)
{
    anzu::context ctx;
    ctx.push({});

    while (ctx.top().ptr() < std::ssize(program)) {
        const auto& op = program[ctx.top().ptr()];
        anzu::print("{:>4} - {}\n", ctx.top().ptr(), op);
        op.apply(ctx);
    }
}

void print_usage()
{
    anzu::print("usage: anzu.exe <program_file> (lex|parse|tree|debug|run) [ast]\n\n");
    anzu::print("The Anzu Programming Language\n\n");
    anzu::print("options:\n");
    anzu::print("    lex   - displays the program after lexing into tokens\n");
    anzu::print("    parse - parses the program using the default parser and prints the bytecode\n");
    anzu::print("    tree  - prints the ast (obviously ast must be specified)\n");
    anzu::print("    debug - executes the program and prints each op code executed\n");
    anzu::print("    run   - executes the program\n");
    anzu::print("flags\n");
    anzu::print("    ast   - uses the new ast-based parser\n");
}

int main(int argc, char** argv)
{
    if (argc != 3 && argc != 4) {
        print_usage();
        return 1;
    }
    if (argc == 4 && std::string{argv[3]} != "ast") {
        print_usage();
        return 2;
    }

    const bool use_ast = argc == 4; 

    const auto file = std::string{argv[1]};
    const auto mode = std::string{argv[2]};

    anzu::print("loading file '{}'\n", file);

    const auto tokens = anzu::lex(file);

    if (mode == "lex") {
        print_tokens(tokens);
        return 0;
    }

    std::vector<anzu::op> program;
    if (use_ast) {
        const auto root = anzu::build_ast(tokens);
        anzu::ast_eval_context ctx;
        root->evaluate(ctx);
        program = std::move(ctx.program);

        if (mode == "tree") {
            root->print();
            return 0;
        }

    } else {
        program = anzu::parse(tokens);
    }

    if (mode == "parse") {
        print_program(program);
        return 0;
    }

    if (mode == "run") {
        run_program(program);
        return 0;
    }
    else if (mode == "debug") {
        run_program_debug(program);
        return 0;
    }

    anzu::print("unknown mode: '{}'\n", mode);
    print_usage();
    return 1;
}