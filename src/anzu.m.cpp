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
    using namespace anzu;
    auto p = std::vector<std::byte>();
    push_value(p, op2::push_literal_i64);
    push_value(p, std::uint64_t{5});
    push_value(p, op2::push_literal_i64);
    push_value(p, std::uint64_t{6});
    push_value(p, op2::u64_mul);
    push_value(p, op2::builtin_call);
    push_value(p, get_builtin_id("println", {i64_type()}));
    run_program(bytecode_program{p, {}});

    return 0;

    if (argc != 3) {
        print_usage();
        return 1;
    }

    const auto file = std::filesystem::canonical(argv[1]);
    const auto root = file.parent_path();
    const auto mode = std::string{argv[2]};


    // In lex and parse mode, only process the specified file, not the entire program.
    if (mode == "lex" || mode == "parse") {
        anzu::print("Loading file '{}'\n", file.string());
        anzu::print("-> Lexing\n");
        auto lex_result = anzu::lex(file);
        if (mode == "lex") {
            print_tokens(lex_result);
        } else {
            anzu::print("-> Parsing\n");
            const auto mod = anzu::parse(std::move(lex_result));
            print_node(*mod.root);
        }
        return 0;
    }

    // Start with the specified file, lex and parse it, then check to see if it has any other
    // required modules. Pick one and compile, continue until all modules have been parsed
    auto parsed_program = std::map<std::filesystem::path, anzu::parse_result>{};

    auto modules = std::set<std::filesystem::path>{file};
    while (!modules.empty()) {
        const auto curr = modules.extract(modules.begin()).value();
        anzu::print("-> Processing '{}'\n", curr.lexically_relative(root).string());
        anzu::print("    - Lexing\n");
        auto lex_res = anzu::lex(curr);
        anzu::print("    - Parsing\n");
        auto ast = anzu::parse(std::move(lex_res));
        for (const auto& m : ast.required_modules) {
            if (!parsed_program.contains(m)) {
                modules.emplace(m);
            }
        }
        parsed_program.emplace(curr, std::move(ast));
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

#if 0
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
#endif
    return 1;
}