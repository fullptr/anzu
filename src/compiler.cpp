#include "compiler.hpp"
#include "lexer.hpp"
#include "object.hpp"
#include "print.hpp"
#include "parser.hpp"

#include <string_view>
#include <optional>
#include <tuple>

namespace anzu {

void node_sequence::evaluate(compiler_context& ctx)
{
    for (const auto& node : sequence) {
        node->evaluate(ctx);
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

void node_while_statement::evaluate(compiler_context& ctx)
{
    const auto while_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_while{});

    condition->evaluate(ctx);
    
    const auto do_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_do{});

    body->evaluate(ctx);

    const auto end_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_while_end{ .jump=while_pos }); // Jump back to start

    // Setup the control flow jumps
    ctx.program[do_pos].as<anzu::op_do>().jump = end_pos + 1; // Jump past the end if false
    for (std::intptr_t idx = do_pos + 1; idx != end_pos; ++idx) {
        // Only set break and continue jumps if they are currently not set, they may be set it
        // there are nested while loops
        if (auto op = ctx.program[idx].get_if<anzu::op_break>(); op && op->jump == -1) {
            op->jump = end_pos + 1;
        }
        else if (auto op = ctx.program[idx].get_if<anzu::op_continue>(); op && op->jump == -1) {
            op->jump = while_pos;
        }
    }
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

void node_if_statement::evaluate(compiler_context& ctx)
{
    const auto if_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_if{});

    condition->evaluate(ctx);
    
    const auto do_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_do{});
    body->evaluate(ctx);

    auto else_pos = std::intptr_t{-1};
    if (else_body) {
        else_pos = std::ssize(ctx.program);
        ctx.program.emplace_back(anzu::op_else{});
        else_body->evaluate(ctx);
    }

    ctx.program.emplace_back(anzu::op_if_end{});
    if (else_pos == -1) {
        ctx.program[do_pos].as<anzu::op_do>().jump = std::ssize(ctx.program); // Jump past the end if false
    } else {
        ctx.program[do_pos].as<anzu::op_do>().jump = else_pos + 1; // Jump into the else block if false
        ctx.program[else_pos].as<anzu::op_else>().jump = std::ssize(ctx.program); // Jump past the end if false
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

void node_literal::evaluate(compiler_context& ctx)
{
    ctx.program.emplace_back(anzu::op_push_const{ .value=value });
}

void node_literal::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}Literal: {}\n", spaces, value.to_repr());
}

void node_variable::evaluate(compiler_context& ctx)
{
    ctx.program.emplace_back(anzu::op_push_var{ .name=name });
}

void node_variable::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}Variable: {}\n", spaces, name);
}

void node_break::evaluate(compiler_context& ctx)
{
    ctx.program.emplace_back(anzu::op_break{});
}

void node_break::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}Break\n", spaces);
}

void node_continue::evaluate(compiler_context& ctx)
{
    ctx.program.emplace_back(anzu::op_continue{});
}

void node_continue::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}Continue\n", spaces);
}

void node_bin_op::evaluate(compiler_context& ctx)
{
    lhs->evaluate(ctx);
    rhs->evaluate(ctx);
    if      (op == "+")  { ctx.program.push_back(anzu::op_add{}); }
    else if (op == "-")  { ctx.program.push_back(anzu::op_sub{}); }
    else if (op == "*")  { ctx.program.push_back(anzu::op_mul{}); }
    else if (op == "/")  { ctx.program.push_back(anzu::op_div{}); }
    else if (op == "%")  { ctx.program.push_back(anzu::op_mod{}); }
    else if (op == "<")  { ctx.program.push_back(anzu::op_lt{}); }
    else if (op == "<=") { ctx.program.push_back(anzu::op_le{}); }
    else if (op == ">")  { ctx.program.push_back(anzu::op_gt{}); }
    else if (op == ">=") { ctx.program.push_back(anzu::op_ge{}); }
    else if (op == "==") { ctx.program.push_back(anzu::op_eq{}); }
    else if (op == "!=") { ctx.program.push_back(anzu::op_ne{}); }
    else if (op == "||") { ctx.program.push_back(anzu::op_or{}); }
    else if (op == "&&") { ctx.program.push_back(anzu::op_and{}); }
    else {
        anzu::print("syntax error: unknown binary operator: '{}'\n", op);
        std::exit(1);
    }
}

void node_bin_op::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}BinOp\n", spaces);
    anzu::print("{}- Op: {}\n", spaces, op);
    anzu::print("{}- Lhs:\n", spaces);
    if (!lhs) {
        anzu::print("bin op has no lhs\n");
        std::exit(1);
    }
    lhs->print(indent + 1);
    anzu::print("{}- Rhs:\n", spaces);
    if (!rhs) {
        anzu::print("bin op has no rhs\n");
        std::exit(1);
    }
    rhs->print(indent + 1);
}

void node_assign_expression::evaluate(compiler_context& ctx)
{
    expr->evaluate(ctx);
    ctx.program.emplace_back(anzu::op_store{ .name=name });
}

void node_assign_expression::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}AssignExpr\n", spaces);
    anzu::print("{}- Name: {}\n", spaces, name);
    anzu::print("{}- Value:\n", spaces);
    expr->print(indent + 1);
}

void node_discard_expression::evaluate(compiler_context& ctx)
{
    expr->evaluate(ctx);
    ctx.program.emplace_back(anzu::op_pop{});
}

void node_discard_expression::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    expr->print(indent);
}

void node_function_def::evaluate(compiler_context& ctx)
{
    const auto start_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_function{ .name=name, .arg_names=arg_names });
    ctx.functions[name] = { .arg_names=arg_names ,.ptr=start_pos };

    body->evaluate(ctx);

    const auto end_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_function_end{});

    ctx.program[start_pos].as<anzu::op_function>().jump = end_pos + 1;
}

void node_function_def::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}Function: {}", spaces, name);
    for (const auto& arg : arg_names) {
        anzu::print(" {}", arg);
    }
    anzu::print("\n");
    body->print(indent + 1);
}

void node_function_call::evaluate(compiler_context& ctx)
{
    // Push the args to the stack
    for (const auto& arg : args) {
        arg->evaluate(ctx);
    }

    const auto& function_def = ctx.functions.at(function_name);

    // Call the function
    ctx.program.emplace_back(anzu::op_function_call{
        .name=function_name,
        .ptr=function_def.ptr + 1, // Jump into the function
        .arg_names=function_def.arg_names
    });
}

void node_function_call::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}FunctionCall: {}\n", spaces, function_name);
    anzu::print("{}- Args:\n", spaces);
    for (const auto& arg : args) {
        arg->print(indent + 1);
    }
}

void node_function_call_statement::evaluate(compiler_context& ctx)
{
    // Push the args to the stack
    for (const auto& arg : args) {
        arg->evaluate(ctx);
    }

    const auto& function_def = ctx.functions.at(function_name);

    // Call the function
    ctx.program.emplace_back(anzu::op_function_call{
        .name=function_name,
        .ptr=function_def.ptr + 1, // Jump into the function
        .arg_names=function_def.arg_names
    });
}

void node_function_call_statement::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}FunctionCall: {}\n", spaces, function_name);
    anzu::print("{}- Args:\n", spaces);
    for (const auto& arg : args) {
        arg->print(indent + 1);
    }
}

void node_builtin_call::evaluate(compiler_context& ctx)
{
    // Push the args to the stack
    for (const auto& arg : args) {
        arg->evaluate(ctx);
    }

    // Call the function
    ctx.program.emplace_back(anzu::op_builtin_function_call{
        .name=function_name,
        .func=anzu::fetch_builtin(function_name)
    });
    ctx.program.emplace_back(anzu::op_pop{});
}


void node_builtin_call::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}BuiltinCall: {}\n", spaces, function_name);
    anzu::print("{}- Args:\n", spaces);
    for (const auto& arg : args) {
        arg->print(indent + 1);
    }
}

void node_builtin_call_statement::evaluate(compiler_context& ctx)
{
    // Push the args to the stack
    for (const auto& arg : args) {
        arg->evaluate(ctx);
    }

    // Call the function
    ctx.program.emplace_back(anzu::op_builtin_function_call{
        .name=function_name,
        .func=anzu::fetch_builtin(function_name)
    });
    ctx.program.emplace_back(anzu::op_pop{});
}


void node_builtin_call_statement::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}BuiltinCall: {}\n", spaces, function_name);
    anzu::print("{}- Args:\n", spaces);
    for (const auto& arg : args) {
        arg->print(indent + 1);
    }
}

void node_return::evaluate(compiler_context& ctx)
{
    return_value->evaluate(ctx);
    ctx.program.emplace_back(anzu::op_return{});
}

void node_return::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}Return:\n", spaces);
    return_value->print(indent + 1);
}

auto compile(const std::unique_ptr<anzu::node>& root) -> std::vector<anzu::op>
{
    anzu::compiler_context ctx;
    root->evaluate(ctx);
    return ctx.program;
}

}