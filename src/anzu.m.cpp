#include "lexer.hpp"
#include "parser.hpp"
#include "compiler.hpp"
#include "runtime.hpp"
#include "utility/print.hpp"

#include <string>

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

    const auto ast = anzu::parse(tokens);
    if (mode == "parse") {
        print_node(*ast);
        return 0;
    }

    const auto program = anzu::compile(ast);
    if (mode == "com") {
        anzu::print_program(program);
        return 0;
    }

    if (mode == "run") {
        anzu::run_program(program);
        return 0;
    }
    else if (mode == "debug") {
        anzu::run_program_debug(program);
        return 0;
    }

    anzu::print("unknown mode: '{}'\n", mode);
    print_usage();
    return 1;
}