#include "stack_frame.hpp"
#include "op_codes.hpp"
#include "print.hpp"
#include "lexer.hpp"
#include "parser.hpp"
#include "compiler.hpp"

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
    anzu::print("usage: anzu.exe <program_file> (lex|parse|com|debug|run)\n\n");
    anzu::print("The Anzu Programming Language\n\n");
    anzu::print("options:\n");
    anzu::print("    lex   - runs the lexer and prints the tokens\n");
    anzu::print("    parse - runs the parser and prints the AST\n");
    anzu::print("    com   - runs the compiler and prints the bytecode\n");
    anzu::print("    debug - runs the program and prints each op code executed\n");
    anzu::print("    run   - runs the program\n");
}

int main(int argc, char** argv)
{
    if (argc != 3 && argc != 4) {
        print_usage();
        return 1;
    }

    const auto file = std::string{argv[1]};
    const auto mode = std::string{argv[2]};

    anzu::print("loading file '{}'\n", file);

    const auto tokens = anzu::lex(file);
    if (mode == "lex") {
        print_tokens(tokens);
        return 0;
    }

    const auto root = anzu::parse(tokens);
    if (mode == "parse") {
        print_node(*root);
        return 0;
    }

    const auto program = anzu::compile(root);
    if (mode == "com") {
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