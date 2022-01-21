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
    anzu::print("usage: anzu.exe <program_file> (lex|parse|debug|run)\n\n");
    anzu::print("The Anzu Programming Language\n\n");
    anzu::print("options:\n");
    anzu::print("    lex   - displays the program after lexing into tokens\n");
    anzu::print("    parse - displays the program after parsig to bytecode\n");
    anzu::print("    debug - executes the program and prints each op code executed\n");
    anzu::print("    run   - executes the program\n");
}

int main(int argc, char** argv)
{
    const auto tok = anzu::token{
        .text = "foo",
        .line = 1,
        .col = 2,
        .type = anzu::token_type::symbol
    };

    auto root = std::make_unique<anzu::node_sequence>();
    root->sequence.push_back(std::make_unique<anzu::node_expression>(std::vector<anzu::token>{tok}));
    
    auto while_node = std::make_unique<anzu::node_while_statement>();

    auto while_cond = std::make_unique<anzu::node_bin_op>();
    while_cond->op = "+";
    while_cond->lhs = std::make_unique<anzu::node_literal>(anzu::object{1});
    while_cond->rhs = std::make_unique<anzu::node_literal>(anzu::object{2});

    auto while_body = std::make_unique<anzu::node_bin_op>();
    while_body->op = "+";
    while_body->lhs = std::make_unique<anzu::node_literal>(anzu::object{1});
    while_body->rhs = std::make_unique<anzu::node_literal>(anzu::object{2});

    while_node->condition = std::move(while_cond);
    while_node->body = std::move(while_body);
    root->sequence.push_back(std::move(while_node));

    root->print();
    std::vector<anzu::op> p;
    root->evaluate(p);
    print_program(p);
    return 0;

    if (argc != 3) {
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

    const auto program = anzu::parse(tokens);
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