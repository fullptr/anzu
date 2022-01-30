#include "compiler.hpp"
#include "lexer.hpp"
#include "object.hpp"
#include "print.hpp"
#include "parser.hpp"

#include <string_view>
#include <optional>
#include <tuple>
#include <unordered_map>

namespace anzu {

// Struct used to store information while compiling an AST. Contains the output program
// as well as information such as function definitions.
struct compiler_context
{
    struct function_def
    {
        std::vector<std::string> arg_names;
        std::intptr_t ptr;
    };

    anzu::program program;
    std::unordered_map<std::string, function_def> functions;
};

namespace {

// Both for and while loops have the form [<begin> <condition> <do> <body> <end>].
// This function links the do to jump to one past the end if false, makes breaks
// jump past the end, and makes continues jump back to the beginning.
auto link_up_jumps(
    compiler_context& ctx,
    std::intptr_t loop_begin,
    std::intptr_t loop_do,
    std::intptr_t loop_end
)
    -> void
{
    // Jump past the end if false
    std::get<anzu::op_jump_if_false>(ctx.program[loop_do]).jump = loop_end + 1;
        
    // Only set unset jumps, there may be other already set from nested loops
    for (std::intptr_t idx = loop_do + 1; idx != loop_end; ++idx) {
        if (auto op = std::get_if<anzu::op_break>(&ctx.program[idx]); op && op->jump == -1) {
            op->jump = loop_end + 1;
        }
        else if (auto op = std::get_if<anzu::op_continue>(&ctx.program[idx]); op && op->jump == -1) {
            op->jump = loop_begin;
        }
    }
}

auto compile_node(const node_expr& root, compiler_context& ctx) -> void;
auto compile_node(const node_stmt& root, compiler_context& ctx) -> void;

void compile_function_call(
    const std::string& function,
    const std::vector<node_expr_ptr>& args,
    compiler_context& ctx
)
{
    // Push the args to the stack
    for (const auto& arg : args) {
        compile_node(*arg, ctx);
    }

    if (const auto it = ctx.functions.find(function); it != ctx.functions.end()) {
        const auto& function_def = it->second;
        ctx.program.emplace_back(anzu::op_function_call{
            .name=function,
            .ptr=function_def.ptr + 1, // Jump into the function
            .arg_names=function_def.arg_names
        });
    }
    else {
        ctx.program.emplace_back(anzu::op_builtin_call{
            .name=function,
            .func=anzu::fetch_builtin(function)
        });
    }
}

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
    if      (node.op == "+")  { ctx.program.emplace_back(anzu::op_add{}); }
    else if (node.op == "-")  { ctx.program.emplace_back(anzu::op_sub{}); }
    else if (node.op == "*")  { ctx.program.emplace_back(anzu::op_mul{}); }
    else if (node.op == "/")  { ctx.program.emplace_back(anzu::op_div{}); }
    else if (node.op == "%")  { ctx.program.emplace_back(anzu::op_mod{}); }
    else if (node.op == "<")  { ctx.program.emplace_back(anzu::op_lt{}); }
    else if (node.op == "<=") { ctx.program.emplace_back(anzu::op_le{}); }
    else if (node.op == ">")  { ctx.program.emplace_back(anzu::op_gt{}); }
    else if (node.op == ">=") { ctx.program.emplace_back(anzu::op_ge{}); }
    else if (node.op == "==") { ctx.program.emplace_back(anzu::op_eq{}); }
    else if (node.op == "!=") { ctx.program.emplace_back(anzu::op_ne{}); }
    else if (node.op == "||") { ctx.program.emplace_back(anzu::op_or{}); }
    else if (node.op == "&&") { ctx.program.emplace_back(anzu::op_and{}); }
    else {
        anzu::print("syntax error: unknown binary operator: '{}'\n", node.op);
        std::exit(1);
    }
}

void compile_node(const node_function_call_expr& node, compiler_context& ctx)
{
    compile_function_call(node.function_name, node.args, ctx);
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
    ctx.program.emplace_back(anzu::op_jump_if_false{});

    compile_node(*node.body, ctx);

    const auto end_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_while_end{ .jump=while_pos }); // Jump back to start

    link_up_jumps(ctx, while_pos, do_pos, end_pos);
}

void compile_node(const node_if_stmt& node, compiler_context& ctx)
{
    const auto if_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_if{});

    compile_node(*node.condition, ctx);
    
    const auto do_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_jump_if_false{});

    compile_node(*node.body, ctx);

    auto else_pos = std::intptr_t{-1};
    if (node.else_body) {
        else_pos = std::ssize(ctx.program);
        ctx.program.emplace_back(anzu::op_else{});
        compile_node(*node.else_body, ctx);
    }

    ctx.program.emplace_back(anzu::op_if_end{});
    if (else_pos == -1) {
        std::get<anzu::op_jump_if_false>(ctx.program[do_pos]).jump = std::ssize(ctx.program); // Jump past the end if false
    } else {
        std::get<anzu::op_jump_if_false>(ctx.program[do_pos]).jump = else_pos + 1; // Jump into the else block if false
        std::get<anzu::op_else>(ctx.program[else_pos]).jump = std::ssize(ctx.program); // Jump past the end if false
    }
}

void compile_node(const node_for_stmt& node, compiler_context& ctx)
{
    // Push the container to the stack
    if (std::holds_alternative<anzu::node_variable_expr>(*node.container)) {
        const auto& cont = std::get<anzu::node_variable_expr>(*node.container).name;
        ctx.program.emplace_back(anzu::op_push_var{ .name=cont });
    }
    else if (std::holds_alternative<anzu::node_literal_expr>(*node.container)) {
        const auto& cont = std::get<anzu::node_literal_expr>(*node.container).value;
        ctx.program.emplace_back(anzu::op_push_const{ .value=cont });
    }
    else {
        anzu::print("unknown container for a for-loop\n");
        std::exit(1);
    }

    // Push the container size to the stack
    ctx.program.emplace_back(anzu::op_copy_index{0});
    ctx.program.emplace_back(anzu::op_builtin_call{
        .name="list_size",
        .func=anzu::fetch_builtin("list_size")
    });

    // Push the counter to the stack
    ctx.program.emplace_back(anzu::op_push_const{ .value=0 });

    // Stack: list, size, counter(0)

    const auto for_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_for{});

    ctx.program.emplace_back(anzu::op_copy_index{1}); // Push size to stack
    ctx.program.emplace_back(anzu::op_copy_index{1}); // Push index to stack
    ctx.program.emplace_back(anzu::op_ne{});   // Eval size != index

    const auto do_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_jump_if_false{});   // If size == index, jump to end

    // Stack: list, size, index(0)
    ctx.program.emplace_back(anzu::op_copy_index{2}); // Push container
    ctx.program.emplace_back(anzu::op_copy_index{1}); // Push index
    ctx.program.emplace_back(anzu::op_builtin_call{
        .name="list_at",
        .func=anzu::fetch_builtin("list_at")
    });
    ctx.program.emplace_back(anzu::op_store{ .name=node.var }); // Store in var

    compile_node(*node.body, ctx);

    // Increment the index
    ctx.program.emplace_back(anzu::op_push_const{ .value=1 });
    ctx.program.emplace_back(anzu::op_add{});

    const auto end_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_for_end{ .jump=for_pos }); // Jump back to start

    ctx.program.emplace_back(anzu::op_pop{}); // Pop index
    ctx.program.emplace_back(anzu::op_pop{}); // Pop size
    ctx.program.emplace_back(anzu::op_pop{}); // Pop container

    link_up_jumps(ctx, for_pos, do_pos, end_pos);
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

    std::get<anzu::op_function>(ctx.program[start_pos]).jump = end_pos + 1;
}

void compile_node(const node_function_call_stmt& node, compiler_context& ctx)
{
    compile_function_call(node.function_name, node.args, ctx);
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

auto compile(const std::unique_ptr<node_stmt>& root) -> anzu::program
{
    anzu::compiler_context ctx;
    compile_node(*root, ctx);
    return ctx.program;
}

}