#include "ast.hpp"
#include "lexer.hpp"
#include "object.hpp"
#include "print.hpp"

namespace anzu {

using token_iterator = std::vector<anzu::token>::const_iterator;
using node_ptr       = std::unique_ptr<anzu::node>;

namespace {

auto consume_maybe(token_iterator& it, std::string_view tok) -> bool
{
    if (it->text == tok) {
        ++it; // skip end
        return true;
    }
    return false;
}

auto consume_only(token_iterator& it, std::string_view tok) -> void
{
    if (!consume_maybe(it, tok)) {
        anzu::print("parse error: expected '{}', got '{}'\n", tok, it->text);
        std::exit(1);
    }
}

}

void node_expression::evaluate(std::vector<anzu::op>& program)
{
}

void node_expression::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}Statement:\n", spaces);
    for (const auto& tok : tokens) {
        anzu::print("{}    {}\n", spaces, tok.text);
    }
}

void node_op::evaluate(std::vector<anzu::op>& program)
{
    program.push_back(op);
}

void node_op::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}Op: {}\n", spaces, op);
}

void node_sequence::evaluate(std::vector<anzu::op>& program)
{
    for (const auto& node : sequence) {
        node->evaluate(program);
    }
}

void node_sequence::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}Sequence:\n", spaces);
    for (const auto& node : sequence) {
        node->print(indent + 1);
    }
}

void node_while_statement::evaluate(std::vector<anzu::op>& program)
{
    const auto while_pos = std::ssize(program);
    program.emplace_back(anzu::op_while{});

    condition->evaluate(program);
    
    const auto do_pos = std::ssize(program);
    program.emplace_back(anzu::op_do{});

    body->evaluate(program);

    program.emplace_back(anzu::op_while_end{ .jump=while_pos }); // Jump back to start
    program[do_pos].as<anzu::op_do>().jump = std::ssize(program); // Jump past the end if false
}

void node_while_statement::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}While:\n", spaces);
    anzu::print("{}- Condition:\n", spaces);
    condition->print(indent + 1);
    anzu::print("{}- Body:\n", spaces);
    body->print(indent + 1);
}

void node_if_statement::evaluate(std::vector<anzu::op>& program)
{
    const auto if_pos = std::ssize(program);
    program.emplace_back(anzu::op_if{});

    condition->evaluate(program);
    
    const auto do_pos = std::ssize(program);
    program.emplace_back(anzu::op_do{});
    body->evaluate(program);

    auto else_pos = std::intptr_t{-1};
    if (else_body) {
        else_pos = std::ssize(program);
        program.emplace_back(anzu::op_else{});
        else_body->evaluate(program);
    }

    program.emplace_back(anzu::op_if_end{});
    if (else_pos == -1) {
        program[do_pos].as<anzu::op_do>().jump = std::ssize(program); // Jump past the end if false
    } else {
        program[do_pos].as<anzu::op_do>().jump = else_pos + 1; // Jump into the else block if false
        program[else_pos].as<anzu::op_else>().jump = std::ssize(program); // Jump past the end if false
    }
}

void node_if_statement::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}If:\n", spaces);
    anzu::print("{}- Condition:\n", spaces);
    condition->print(indent + 1);
    anzu::print("{}- Body:\n", spaces);
    body->print(indent + 1);
    if (else_body) {
        anzu::print("{}- Else:\n", spaces);
        else_body->print(indent + 1);
    }
}

void node_function_definition::evaluate(std::vector<anzu::op>& program)
{
}

void node_function_definition::print(int indent)
{
}

void node_literal::evaluate(std::vector<anzu::op>& program)
{
    program.emplace_back(anzu::op_push_const{ .value=value });
}

void node_literal::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}Literal: {}\n", spaces, value);
}

namespace {

// A temporary function during migration between the old and new parsers. This will
// get smaller and smaller as we move these operations to be stored in proper tree
// nodes.
auto parse_op(token_iterator& it, token_iterator end) -> anzu::op
{
    const auto& token = it->text;
    ++it;
    if (token == STORE)   return op_store{ .name=(it++)->text };
    if (token == POP)     return op_pop{};
    if (token == DUP)     return op_dup{};
    if (token == SWAP)    return op_swap{};
    if (token == ROT)     return op_rot{};
    if (token == OVER)    return op_over{};
    if (token == ADD)     return op_add{};
    if (token == SUB)     return op_sub{};
    if (token == MUL)     return op_mul{};
    if (token == DIV)     return op_div{};
    if (token == MOD)     return op_mod{};
    if (token == EQ)      return op_eq{};
    if (token == NE)      return op_ne{};
    if (token == LT)      return op_lt{};
    if (token == LE)      return op_le{};
    if (token == GT)      return op_gt{};
    if (token == GE)      return op_ge{};
    if (token == OR)      return op_or{};
    if (token == AND)     return op_and{};
    if (token == INPUT)   return op_input{};
    if (token == DUMP)    return op_dump{};
    if (token == TO_INT)  return op_to_int{};
    if (token == TO_BOOL) return op_to_bool{};
    if (token == TO_STR)  return op_to_str{};
    return op_push_var{.name=token};
}

auto parse_statement(token_iterator& it, token_iterator end) -> node_ptr;

// statement_list:
//     | statement
//     | statement statement_list
auto parse_statement_list(token_iterator& it, token_iterator end) -> node_ptr
{
    static const auto sentinel = std::unordered_set<std::string_view>{"end", "elif", "do", "else"};

    auto stmt_list = std::make_unique<anzu::node_sequence>();
    while (it != end && !sentinel.contains(it->text)) {
        stmt_list->sequence.push_back(parse_statement(it, end));
    }
    return stmt_list;
}

// if_body:
//     | statement_list 'do' statement_list 'elif' if_body
//     | statement_list 'do' statement_list 'end'
auto parse_if_body(token_iterator& it, token_iterator end) -> node_ptr
{
    auto if_stmt = std::make_unique<anzu::node_if_statement>();
    if_stmt->condition = parse_statement_list(it, end);
    consume_only(it, "do");
    if_stmt->body = parse_statement_list(it, end);

    if (consume_maybe(it, "elif")) {
        if_stmt->else_body = parse_if_body(it, end);
    }
    else if (consume_maybe(it, "else")) {
        if_stmt->else_body = parse_statement_list(it, end);
        consume_only(it, "end");
    }
    else {
        consume_only(it, "end");
    }
    
    return if_stmt;
}

auto handle_list_literal(token_iterator& it, token_iterator end) -> anzu::object_list
{
    auto list = std::make_shared<std::vector<anzu::object>>();

    consume_only(it, "[");
    while (it != end && it->text != "]") {
        if (it->text == "[") { // Nested list literal
            list->push_back(handle_list_literal(it, end));
        }
        else if (it->type == token_type::string) {
            list->push_back(it->text);
        }
        else if (it->type == token_type::number) {
            list->push_back(anzu::to_int(it->text));
        }
        else if (it->text == "true") {
            list->push_back(true);
        }
        else if (it->text == "false") {
            list->push_back(false);
        }
        else if (it->text == ",") {
            // Pass, delimiters currently optional, will enforce after this workes
        }
        else {
            anzu::print("could not recognise token while parsing list literal: {}\n", it->text);
            std::exit(1);
        }
        ++it;
    }
    if (it == end) {
        anzu::print("end of file reached while parsing string literal\n");
        std::exit(1);
    }
    consume_only(it, "]");

    return list;
}

// statement:
//     | 'while' statement_list 'do' statement_list 'end'
//     | 'while' statement_list 'do' 'end'
//     | 'if' if_body
//     | num_literal
//     | string_literal
//     | builtin
//     | identifier
// TODO: ALlow for break, continue, else and elif
auto parse_statement(token_iterator& it, token_iterator end) -> node_ptr
{
    if (consume_maybe(it, "while")) {
        auto while_stmt = std::make_unique<anzu::node_while_statement>();
        while_stmt->condition = parse_statement_list(it, end);
        consume_only(it, "do");
        while_stmt->body = parse_statement_list(it, end);
        consume_only(it, "end");
        return while_stmt;
    }
    else if (consume_maybe(it, "if")) {
        return parse_if_body(it, end);
    }
    else if (it->type == token_type::number) {
        const auto literal = anzu::object{anzu::to_int(it->text)};
        ++it;
        return std::make_unique<anzu::node_literal>(literal);
    }
    else if (it->type == token_type::string) {
        const auto literal = anzu::object{it->text};
        ++it;
        return std::make_unique<anzu::node_literal>(literal);
    }
    else if (it->text == "[") {
        auto list = handle_list_literal(it, end);
        return std::make_unique<anzu::node_literal>(anzu::object{list});
    }
    else if (it != end) {
        return std::make_unique<anzu::node_op>(parse_op(it, end));
    }
    return nullptr;
}

}

auto build_ast(const std::vector<anzu::token>& tokens) -> node_ptr
{
    auto it = tokens.begin();
    auto root = std::make_unique<anzu::node_sequence>();
    while (it != tokens.end()) {
        root->sequence.push_back(parse_statement(it, tokens.end()));
    }
    return root;
}

}