#include "lexer.hpp"
#include "parser.hpp"
#include "compiler.hpp"
#include "runtime.hpp"
#include "utility/print.hpp"

#include <string>
#include <map>
#include <set>

void print_usage()
{
    anzu::print("usage: anzu.exe <program_file> <option>\n\n");
    anzu::print("The Anzu Programming Language\n\n");
    anzu::print("options:\n");
    anzu::print("    lex      - runs the lexer and prints the tokens for a single file\n");
    anzu::print("    parse    - runs the parser and prints the AST for a single file\n");
    anzu::print("    discover - runs the parser and prints all modules\n");
    anzu::print("    com      - runs the compiler and prints the bytecode\n");
    anzu::print("    debug    - runs the program and prints each op code executed\n");
    anzu::print("    run      - runs the program\n");
}

auto main(const int argc, const char* argv[]) -> int
{
    if (argc != 3) {
        print_usage();
        return 1;
    }

    const auto file = std::string{argv[1]};
    const auto mode = std::string{argv[2]};

    // In lex and parse mode, only process the specified file, not the entire program.
    if (mode == "lex" || mode == "parse") {
        anzu::print("Loading file '{}'\n", file);
        anzu::print("-> Lexing\n");
        const auto tokens = anzu::lex(file);
        if (mode == "lex") {
            print_tokens(tokens);
        } else {
            anzu::print("-> Parsing\n");
            const auto mod = anzu::parse(tokens);
            print_node(*mod.root);
        }
        return 0;
    }

    // Start with the specified file, lex and parse it, then check to see if it has any other
    // required modules. Pick one and compile, continue until all modules have been parsed
    auto parsed_program = std::map<std::string, anzu::file_ast>{};

    auto modules = std::set<std::string>{file};
    while (!modules.empty()) {
        const auto file = modules.extract(modules.begin()).value();
        anzu::print("-> Lexing '{}'\n", file);
        const auto tokens = anzu::lex(file);
        anzu::print("-> Parsing '{}'\n", file);
        auto ast = anzu::parse(tokens);
        for (const auto& m : ast.required_modules) {
            if (!parsed_program.contains(m)) {
                modules.emplace(m);
            }
        }
        parsed_program.emplace(file, std::move(ast));
    }

    if (mode == "discover") {
        anzu::print("\nFound modules:\n");
        for (const auto& [file, mod] : parsed_program) {
            anzu::print("- {}\n", file);
            for (const auto& dep : mod.required_modules) {
                anzu::print("  | - {}\n", dep);
            }
        }
        return 0;
    }

    anzu::print("-> Compiling\n");
    const auto program = anzu::compile(parsed_program[file].root);
    if (mode == "com") {
        anzu::print_program(program);
        return 0;
    }

    anzu::print("-> Running\n\n");
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