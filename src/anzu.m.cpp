#include "lexer.hpp"
#include "parser.hpp"
#include "compiler.hpp"
#include "bytecode.hpp"
#include "runtime.hpp"
#include "utility/common.hpp"
#include "utility/memory.hpp"

#include <string>
#include <map>
#include <set>
#include <filesystem>
#include <print>

void print_usage()
{
    std::print("usage: anzu.exe <program_file> <option>\n\n");
    std::print("The Anzu Programming Language\n\n");
    std::print("options:\n");
    std::print("    lex      - runs the lexer and prints the tokens for a single file\n");
    std::print("    parse    - runs the parser and prints the AST for a single file\n");
    std::print("    com      - runs the compiler and prints the bytecode\n");
    std::print("    debug    - runs the program and prints each op code executed\n");
    std::print("    run      - runs the program\n");
}

auto main(const int argc, const char* argv[]) -> int
{
    if (argc != 3) {
        print_usage();
        return 1;
    }

    const auto timer = anzu::scope_timer{};
    const auto file = std::filesystem::canonical(argv[1]);
    const auto root = file.parent_path();
    const auto mode = std::string{argv[2]};

    if (mode == "lex") {
        std::print("Lexing file '{}'\n", file.string());
        const auto code = anzu::read_file(file);
        auto ctx = anzu::lexer{*code};
        for (auto token = ctx.get_token(); token.type != anzu::token_type::eof; token = ctx.get_token()) {
            print_token(token);
        }
        return 0;
    }

    std::print("-> Parsing\n");
    auto ast = anzu::parse(file);
    if (mode == "parse") {
        print_node(*ast.root);
        return 0;
    }

    std::print("-> Compiling\n");
    const auto program = anzu::compile(ast);
    if (mode == "com") {
        print_program(program);
        return 0;
    }

    std::print("-> Running\n\n");
    if (mode == "run") {
        anzu::run_program(program);
        return 0;
    }
    else if (mode == "debug") {
        anzu::run_program_debug(program);
        return 0;
    }

    std::print("unknown mode: '{}'\n", mode);
    print_usage();
    return 1;
}