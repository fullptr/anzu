#include "stack_frame.hpp"
#include "op_codes.hpp"
#include "lexer.hpp"

#include <fmt/format.h>
#include <string>
#include <variant>

void run_program(const std::vector<anzu::op>& program)
{
    anzu::stack_frame frame;
    while (frame.ptr() < std::ssize(program)) {
        program[frame.ptr()].apply(frame);
    }
}

void print_program(const std::vector<anzu::op>& program)
{
    int lineno = 0;
    for (const auto& op : program) {
        fmt::print("{:>4} - {}\n", lineno++, op);
    }
}

void print_usage()
{
    fmt::print("usage: anzu.exe (run|print) <program_file>\n\n");
    fmt::print("The Anzu Programming Language\n\n");
    fmt::print("options:\n");
    fmt::print("    run   - executes the program\n");
    fmt::print("    print - displays the program bytecode\n");
}

int main(int argc, char** argv)
{
    if (argc != 3) {
        print_usage();
        return 1;
    }

    const auto mode = std::string{argv[1]};
    const auto file = std::string{argv[2]};

    fmt::print("loading file '{}'\n", file);
    const auto program = anzu::lexer::parse_file(file);
    
    if (mode == "print") {
        print_program(program);
    }
    else if (mode == "run") {
        run_program(program);
    }
    else {
        fmt::print("unknown mode: '{}'\n", mode);
        print_usage();
        return 1;
    }

    return 0;
}