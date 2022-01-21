#include "ast.hpp"
#include "print.hpp"

namespace anzu {

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

auto build_ast(const std::vector<anzu::token>& tokens) -> std::unique_ptr<anzu::node>
{
    auto root = std::make_unique<anzu::node_sequence>();
    auto it = tokens.begin();
    
    while (it != tokens.end()) {
        auto next = std::next(it);
        if (it->text == "while") {
            auto while_op = std::make_unique<anzu::node_while_statement>();
            auto condition = std::make_unique<anzu::node_expression>();
            ++it;
            while (it != tokens.end() && it->text != "do") {
                condition->tokens.push_back(*it);
                ++it;
            }
            auto body = std::make_unique<anzu::node_expression>();
            ++it;
            while (it != tokens.end() && it->text != "end") {
                body->tokens.push_back(*it);
                ++it;
            }
            while_op->condition = std::move(condition);
            while_op->body = std::move(body);

            root->sequence.push_back(std::move(while_op));
        }
        else {
            auto stmt_op = std::make_unique<anzu::node_expression>();
            stmt_op->tokens.push_back(*it);
            ++it;
            while (it != tokens.end() && it->text != "while") {
                stmt_op->tokens.push_back(*it);
                ++it;
            }
            root->sequence.push_back(std::move(stmt_op));
        }
    }

    return root;
}

}