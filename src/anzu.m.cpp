#include "lexer.hpp"
#include "parser.hpp"
#include "compiler.hpp"
#include "bytecode.hpp"
#include "utility/print.hpp"
#include "utility/memory.hpp"

#include <string>
#include <map>
#include <set>
#include <filesystem>

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

    const auto file = std::filesystem::canonical(argv[1]);
    const auto root = file.parent_path();
    const auto mode = std::string{argv[2]};

    if (mode == "lex") {
        anzu::print("Loading file '{}'\n", file.string());
        anzu::print("-> Lexing\n");
        const auto code = anzu::read_file(file);
        auto ctx = anzu::lex_start(*code);
        for (auto token = anzu::lex_next(ctx); token.type != anzu::token_type::eof; token = anzu::lex_next(ctx)) {
            anzu::print_token(token);
        }
        return 0;
    }

    if (mode == "parse") {
        anzu::print("-> Parsing\n");
        const auto mod = anzu::parse(file);
        anzu::print_node(*mod.root);
        return 0;
    }

    // Start with the specified file, lex and parse it, then check to see if it has any other
    // required modules. Pick one and compile, continue until all modules have been parsed
    auto parsed_program = std::map<std::filesystem::path, anzu::anzu_module>{};

    auto modules = std::set<std::filesystem::path>{file};
    while (!modules.empty()) {
        const auto curr = modules.extract(modules.begin()).value();
        anzu::print("-> Processing '{}'\n", curr.lexically_relative(root).string());
        auto current_module = anzu::parse(curr);
        for (const auto& m : current_module.required_modules) {
            if (!parsed_program.contains(m)) {
                modules.emplace(m);
            }
        }
        parsed_program.emplace(curr, std::move(current_module));
    }

    if (mode == "discover") {
        anzu::print("\nFound modules:\n");
        for (const auto& [file, mod] : parsed_program) {
            anzu::print("- {}\n", file.string());
            for (const auto& dep : mod.required_modules) {
                anzu::print("  | - {}\n", dep.string());
            }
        }
        return 0;
    }

    anzu::print("-> Compiling\n");
    const auto program = anzu::compile(root, parsed_program, true); // TODO: Make debug a switch
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