#include "stack_frame.hpp"
#include "op_codes.hpp"
#include "lexer.hpp"
#include "parser.hpp"

#include <fmt/format.h>
#include <string>
#include <variant>

void print_tokens(const std::vector<std::string>& tokens)
{
    for (const auto& token : tokens) {
        fmt::print("'{}'\n", token);
    }
}

void print_program(const std::vector<anzu::op>& program)
{
    int lineno = 0;
    for (const auto& op : program) {
        fmt::print("{:>4} - {}\n", lineno++, op);
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

void print_usage()
{
    fmt::print("usage: anzu.exe <program_file> (lex|parse|run)\n\n");
    fmt::print("The Anzu Programming Language\n\n");
    fmt::print("options:\n");
    fmt::print("    lex   - displays the program after lexing into tokens\n");
    fmt::print("    parse - displays the program after parsig to bytecode\n");
    fmt::print("    run   - executes the program\n");
}

int main(int argc, char** argv)
{
    if (argc != 3) {
        print_usage();
        return 1;
    }

    const auto file = std::string{argv[1]};
    const auto mode = std::string{argv[2]};

    fmt::print("loading file '{}'\n", file);

    const auto tokens = anzu::lex(file);

    if (mode == "lex") {
        print_tokens(tokens);
        return 0;
    }

    const auto program = anzu::parse(tokens);
    if (mode == "parse") {
        print_program(program);
        return 0;
    }

    if (mode == "run") {
        run_program(program);
        return 0;
    }

    fmt::print("unknown mode: '{}'\n", mode);
    print_usage();
    return 1;
}