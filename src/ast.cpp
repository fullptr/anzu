#include "ast.hpp"
#include "print.hpp"

namespace anzu {

void node_temp::evaluate(std::vector<anzu::op>& program)
{
}

void node_temp::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}Statement:\n", spaces);
    for (const auto& tok : tokens) {
        anzu::print("{}    {}\n", spaces, tok.text);
    }
}

void node_seq::evaluate(std::vector<anzu::op>& program)
{
    for (const auto& node : sequence) {
        node->evaluate(program);
    }
}

void node_seq::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}Sequnce:\n", spaces);
    for (const auto& node : sequence) {
        node->print(indent + 1);
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

}