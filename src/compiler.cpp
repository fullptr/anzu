#include "compiler.hpp"
#include "lexer.hpp"
#include "object.hpp"
#include "print.hpp"
#include "parser.hpp"

#include <string_view>
#include <optional>
#include <tuple>

namespace anzu {
namespace {

auto compile_node(const node_expr& root, compiler_context& ctx) -> void;
auto compile_node(const node_stmt& root, compiler_context& ctx) -> void;

void compile_node(const node_literal_expr& node, compiler_context& ctx)
{
    ctx.program.emplace_back(anzu::op_push_const{ .value=node.value });
}

void compile_node(const node_variable_expr& node, compiler_context& ctx)
{
    ctx.program.emplace_back(anzu::op_push_var{ .name=node.name });
}

void compile_node(const node_bin_op_expr& node, compiler_context& ctx)
{
    compile_node(*node.lhs, ctx);
    compile_node(*node.rhs, ctx);
    if      (node.op == "+")  { ctx.program.push_back(anzu::op_add{}); }
    else if (node.op == "-")  { ctx.program.push_back(anzu::op_sub{}); }
    else if (node.op == "*")  { ctx.program.push_back(anzu::op_mul{}); }
    else if (node.op == "/")  { ctx.program.push_back(anzu::op_div{}); }
    else if (node.op == "%")  { ctx.program.push_back(anzu::op_mod{}); }
    else if (node.op == "<")  { ctx.program.push_back(anzu::op_lt{}); }
    else if (node.op == "<=") { ctx.program.push_back(anzu::op_le{}); }
    else if (node.op == ">")  { ctx.program.push_back(anzu::op_gt{}); }
    else if (node.op == ">=") { ctx.program.push_back(anzu::op_ge{}); }
    else if (node.op == "==") { ctx.program.push_back(anzu::op_eq{}); }
    else if (node.op == "!=") { ctx.program.push_back(anzu::op_ne{}); }
    else if (node.op == "||") { ctx.program.push_back(anzu::op_or{}); }
    else if (node.op == "&&") { ctx.program.push_back(anzu::op_and{}); }
    else {
        anzu::print("syntax error: unknown binary operator: '{}'\n", node.op);
        std::exit(1);
    }
}

void compile_node(const node_function_call_expr& node, compiler_context& ctx)
{
    // Push the args to the stack
    for (const auto& arg : node.args) {
        compile_node(*arg, ctx);
    }

    const auto& function_def = ctx.functions.at(node.function_name);

    // Call the function
    ctx.program.emplace_back(anzu::op_function_call{
        .name=node.function_name,
        .ptr=function_def.ptr + 1, // Jump into the function
        .arg_names=function_def.arg_names
    });
}

void compile_node(const node_builtin_call_expr& node, compiler_context& ctx)
{
    // Push the args to the stack
    for (const auto& arg : node.args) {
        compile_node(*arg, ctx);
    }

    // Call the function
    ctx.program.emplace_back(anzu::op_builtin_function_call{
        .name=node.function_name,
        .func=anzu::fetch_builtin(node.function_name)
    });
}

void compile_node(const node_sequence_stmt& node, compiler_context& ctx)
{
    for (const auto& seq_node : node.sequence) {
        compile_node(*seq_node, ctx);
    }
}

void compile_node(const node_while_stmt& node, compiler_context& ctx)
{
    const auto while_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_while{});

    compile_node(*node.condition, ctx);
    
    const auto do_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_do{});

    compile_node(*node.body, ctx);

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

void compile_node(const node_if_stmt& node, compiler_context& ctx)
{
    const auto if_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_if{});

    compile_node(*node.condition, ctx);
    
    const auto do_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_do{});

    compile_node(*node.body, ctx);

    auto else_pos = std::intptr_t{-1};
    if (node.else_body) {
        else_pos = std::ssize(ctx.program);
        ctx.program.emplace_back(anzu::op_else{});
        compile_node(*node.else_body, ctx);
    }

    ctx.program.emplace_back(anzu::op_if_end{});
    if (else_pos == -1) {
        ctx.program[do_pos].as<anzu::op_do>().jump = std::ssize(ctx.program); // Jump past the end if false
    } else {
        ctx.program[do_pos].as<anzu::op_do>().jump = else_pos + 1; // Jump into the else block if false
        ctx.program[else_pos].as<anzu::op_else>().jump = std::ssize(ctx.program); // Jump past the end if false
    }
}

void compile_node(const node_break_stmt&, compiler_context& ctx)
{
    ctx.program.emplace_back(anzu::op_break{});
}

void compile_node(const node_continue_stmt&, compiler_context& ctx)
{
    ctx.program.emplace_back(anzu::op_continue{});
}

void compile_node(const node_assignment_stmt& node, compiler_context& ctx)
{
    compile_node(*node.expr, ctx);
    ctx.program.emplace_back(anzu::op_store{ .name=node.name });
}

void compile_node(const node_function_def_stmt& node, compiler_context& ctx)
{
    const auto start_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_function{ .name=node.name, .arg_names=node.arg_names });
    ctx.functions[node.name] = { .arg_names=node.arg_names ,.ptr=start_pos };

    compile_node(*node.body, ctx);

    const auto end_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_function_end{});

    ctx.program[start_pos].as<anzu::op_function>().jump = end_pos + 1;
}

void compile_node(const node_function_call_stmt& node, compiler_context& ctx)
{
    // Push the args to the stack
    for (const auto& arg : node.args) {
        compile_node(*arg, ctx);
    }

    const auto& function_def = ctx.functions.at(node.function_name);

    // Call the function
    ctx.program.emplace_back(anzu::op_function_call{
        .name=node.function_name,
        .ptr=function_def.ptr + 1, // Jump into the function
        .arg_names=function_def.arg_names
    });
    ctx.program.emplace_back(anzu::op_pop{});
}

void compile_node(const node_builtin_call_stmt& node, compiler_context& ctx)
{
    // Push the args to the stack
    for (const auto& arg : node.args) {
        compile_node(*arg, ctx);
    }

    // Call the function
    ctx.program.emplace_back(anzu::op_builtin_function_call{
        .name=node.function_name,
        .func=anzu::fetch_builtin(node.function_name)
    });
    ctx.program.emplace_back(anzu::op_pop{});
}

void compile_node(const node_return_stmt& node, compiler_context& ctx)
{
    compile_node(*node.return_value, ctx);
    ctx.program.emplace_back(anzu::op_return{});
}

auto compile_node(const node_expr& root, compiler_context& ctx) -> void
{
    std::visit([&](const auto& node) { compile_node(node, ctx); }, root);
}

auto compile_node(const node_stmt& root, compiler_context& ctx) -> void
{
    std::visit([&](const auto& node) { compile_node(node, ctx); }, root);
}

}

auto compile(const std::unique_ptr<node_stmt>& root) -> std::vector<anzu::op>
{
    anzu::compiler_context ctx;
    compile_node(*root, ctx);
    return ctx.program;
}

}