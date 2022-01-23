#include "ast.hpp"
#include "print.hpp"

namespace anzu {

using token_iterator = std::vector<anzu::token>::const_iterator;
using node_ptr       = std::unique_ptr<anzu::node>;

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
        body->print(indent + 1);
    }
}

void node_bin_op::evaluate(std::vector<anzu::op>& program)
{
    lhs->evaluate(program);
    rhs->evaluate(program);
    if (op == "+") {
        program.emplace_back(anzu::op_add{});
    }
    else if (op == "*") {
        program.emplace_back(anzu::op_mul{});
    }
}

void node_bin_op::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}Bin op:\n", spaces);
    anzu::print("{}- Op: {}\n", spaces, op);
    anzu::print("{}- LHS:\n", spaces);
    lhs->print(indent + 1);
    anzu::print("{}- RHS:\n", spaces);
    rhs->print(indent + 1);
}

void node_literal::evaluate(std::vector<anzu::op>& program)
{
    program.emplace_back(anzu::op_push_const{ .value=value });
}

void node_literal::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}Literal = {}\n", spaces, value);
}

namespace {

auto parse_statement(token_iterator& it, token_iterator end) -> node_ptr;

// statement_list:
//     | statement
//     | statement statement_list
auto parse_statement_list(token_iterator& it, token_iterator end) -> node_ptr
{
    auto stmt_list = std::make_unique<anzu::node_sequence>();
    while (it != end) {
        if (it->text == "end"  ||
            it->text == "elif" ||
            it->text == "do"   ||
            it->text == "else")
        {
            break;
        }

        auto stmt = parse_statement(it, end);
        stmt_list->sequence.push_back(std::move(stmt));
    }
    return stmt_list;
}

// if_body:
//     | statement_list 'do' statement_list 'elif' if_body
//     | statement_list 'do' statement_list 'end'
auto parse_if_body(token_iterator& it, token_iterator end) -> node_ptr
{
    auto condition = parse_statement_list(it, end);
    if (it->text != "do") {
        anzu::print("(if) parse error, expected 'do', got '{}'\n", it->text);
        std::exit(1);
    }
    ++it; // skip do

    auto if_stmt = std::make_unique<anzu::node_if_statement>();
    if_stmt->condition = std::move(condition);
    if_stmt->body = parse_statement_list(it, end);
    if_stmt->else_body = nullptr;

    if (it->text == "end") { // no else or elif
        ++it; // skip end
        return if_stmt;
    }
    
    if (it->text == "else") {
        ++it; // skip else
        auto else_body = parse_statement_list(it, end);
        if (it->text == "end") {
            if_stmt->else_body = std::move(else_body);
            ++it; // skip end
            return if_stmt;
        }
    }

    if (it->text == "elif") {
        ++it; // skip elif
        if_stmt->else_body = parse_if_body(it, end);
        return if_stmt;
    }

    anzu::print("(if) parse error, expected 'end|elif|else', got '{}'\n", it->text);
    std::exit(1);
}

auto handle_list_literal(token_iterator& it, token_iterator end) -> anzu::object_list
{
    auto list = std::make_shared<std::vector<anzu::object>>();

    ++it; // Skip "["
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
    ++it; // Skip "]"

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
    if (it->text == "while") {
        ++it; // skip while
        auto condition = parse_statement_list(it, end);
        if (it->text != "do") {
            anzu::print("(while) parse error, expected 'do', got '{}'\n", it->text);
            std::exit(1);
        }
        ++it; // skip do
        auto body = parse_statement_list(it, end);
        if (it->text != "end") {
            anzu::print("(while) parse error, expected 'end', got '{}'\n", it->text);
            std::exit(1);
        }
        ++it; // skip end
        auto while_stmt = std::make_unique<anzu::node_while_statement>();
        while_stmt->condition = std::move(condition);
        while_stmt->body = std::move(body);
        return while_stmt;
    }
    else if (it->text == "if") {
        ++it; // skip if
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
    auto stmt = std::make_unique<anzu::node_expression>();
    stmt->tokens.push_back(*it);
    ++it;
    return stmt;
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