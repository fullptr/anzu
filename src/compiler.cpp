#include "compiler.hpp"
#include "lexer.hpp"
#include "object.hpp"
#include "parser.hpp"
#include "functions.hpp"
#include "utility/print.hpp"
#include "utility/overloaded.hpp"

#include <string_view>
#include <optional>
#include <tuple>
#include <vector>
#include <unordered_map>

namespace anzu {

struct compiler_frame
{
    struct function_def
    {
        signature     sig;
        std::intptr_t ptr;
    };

    std::unordered_map<std::string, function_def> functions;
    std::unordered_map<std::string, std::size_t>  variables;
};

// Struct used to store information while compiling an AST. Contains the output program
// as well as information such as function definitions.
struct compiler_context
{
    anzu::program program;

    struct function_def
    {
        signature     sig;
        std::intptr_t ptr;
    };

    std::unordered_map<std::string, function_def> functions;

    using var_locations = std::unordered_map<std::string, std::size_t>;

    var_locations globals;
    std::optional<var_locations> locals;

    expr_types expr_types;
};

template <typename T>
auto back(std::vector<T>& elements) -> T&
{
    return elements.back();
}

template <typename T>
auto penult(std::vector<T>& elements) -> T&
{
    auto it = elements.end();
    std::advance(it, -2);
    return *it;
}

template <typename T>
auto append_op(compiler_context& ctx, T&& op) -> std::intptr_t
{
    ctx.program.emplace_back(std::forward<T>(op));
    return std::ssize(ctx.program) - 1;
}

// Registers the given name in the current scope
auto declare_variable_name(compiler_context& ctx, const std::string& name) -> void
{
    auto& vars = ctx.locals.has_value() ? ctx.locals.value() : ctx.globals;
    vars.emplace(name, vars.size());
}

auto save_variable(compiler_context& ctx, const std::string& name) -> void
{
    if (ctx.locals && ctx.locals->contains(name)) {
        ctx.program.emplace_back(anzu::op_save_local{
            .name=name, .offset=ctx.locals->at(name)
        });
        return;
    }
    
    if (ctx.globals.contains(name)) {
        ctx.program.emplace_back(anzu::op_save_global{
            .name=name, .position=ctx.globals.at(name)
        });
        return;
    }

    anzu::print("BAD! Could not assign to variable\n");
    std::exit(1);
}

auto load_variable(compiler_context& ctx, const std::string& name) -> void
{
    if (ctx.locals) {
        if (auto it = ctx.locals->find(name); it != ctx.locals->end()) {
            ctx.program.emplace_back(anzu::op_load_local{
                .name=name, .offset=it->second
            });
            return;
        }
    }
    if (auto it = ctx.globals.find(name); it != ctx.globals.end()) {
        ctx.program.emplace_back(anzu::op_load_global{
            .name=name, .position=it->second
        });
    }
}

auto call_builtin(compiler_context& ctx, const std::string& function_name) -> void
{
    const auto& func = anzu::fetch_builtin(function_name);
    ctx.program.emplace_back(anzu::op_builtin_call{
        .name=function_name, .ptr=func.ptr, .sig=func.sig
    });
}

auto find_function(const compiler_context& ctx, const std::string& function)
    -> const compiler_context::function_def*
{
    if (const auto it = ctx.functions.find(function); it != ctx.functions.end()) {
        return &it->second;
    }
    return nullptr;
}

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
    std::get<op_jump_if_false>(ctx.program[loop_do]).jump = loop_end + 1;
        
    // Only set unset jumps, there may be other already set from nested loops
    for (std::intptr_t idx = loop_do + 1; idx != loop_end; ++idx) {
        std::visit(overloaded{
            [&](op_break& op) {
                if (op.jump == -1) { op.jump = loop_end + 1; }
            },
            [&](op_continue& op) {
                if (op.jump == -1) { op.jump = loop_begin; }
            },
            [](auto&&) {}
        }, ctx.program[idx]);
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

    if (const auto function_def = find_function(ctx, function)) {
        ctx.program.emplace_back(anzu::op_function_call{
            .name=function,
            .ptr=function_def->ptr + 1, // Jump into the function
            .sig=function_def->sig
        });
    }
    else {
        const auto& builtin = anzu::fetch_builtin(function);
        ctx.program.emplace_back(anzu::op_builtin_call{
            .name=function,
            .ptr=builtin.ptr,
            .sig=builtin.sig
        });
    }
}

void compile_node(const node_literal_expr& node, compiler_context& ctx)
{
    if (node.value.data.size() != 1) {
        anzu::print("Objects with block-size != 1 not currently supported\n");
        std::exit(1);
    }
    ctx.program.emplace_back(anzu::op_load_literal{ .value=node.value });
}

void compile_node(const node_variable_expr& node, compiler_context& ctx)
{
    load_variable(ctx, node.name);
}

// This is a copy of the logic from typecheck.cpp now, pretty bad, we should make it more
// generic and combine the logic.
void compile_node(const node_bin_op_expr& node, compiler_context& ctx)
{
    const auto lhs_type = ctx.expr_types[node.lhs.get()];
    const auto rhs_type = ctx.expr_types[node.rhs.get()];
    compile_node(*node.lhs, ctx);
    compile_node(*node.rhs, ctx);
    const auto op = node.token.text;

    const auto invalid_expr = [&]() {
        anzu::print("[{}:{}] could not evaluate '{} {} {}'", node.token.line, node.token.col, lhs_type, op, rhs_type);
        std::exit(1);
    };

    if (lhs_type != rhs_type) {
        invalid_expr();
    }

    const auto& type = lhs_type; // == rhs_type too

    if (match(type, generic_list_type()).has_value()) {// No support for having these in binary ops.
        invalid_expr();
    }

    // An example of how we can remove the original bin op codes. We will want to move
    // to this when we have custom types with operators.
    if (type == int_type()) {
        if (op == "+") {
            ctx.program.emplace_back(op_builtin_mem_op{
                .name = std::format("{0} {1} {0}", type, op),
                .ptr = +[](std::vector<block>& mem) {
                    const auto& rhs_val = std::get<block_int>(back(mem));
                    auto& lhs_val = std::get<block_int>(penult(mem));
                    lhs_val = lhs_val + rhs_val;
                    mem.pop_back();
                }
            });
        }
        else if (op == "-") {
            ctx.program.emplace_back(op_builtin_mem_op{
                .name = std::format("{0} {1} {0}", type, op),
                .ptr = +[](std::vector<block>& mem) {
                    const auto& rhs_val = std::get<block_int>(back(mem));
                    auto& lhs_val = std::get<block_int>(penult(mem));
                    lhs_val = lhs_val - rhs_val;
                    mem.pop_back();
                }
            });
        }
        else if (op == "*") {
            ctx.program.emplace_back(op_builtin_mem_op{
                .name = std::format("{0} {1} {0}", type, op),
                .ptr = +[](std::vector<block>& mem) {
                    const auto& rhs_val = std::get<block_int>(back(mem));
                    auto& lhs_val = std::get<block_int>(penult(mem));
                    lhs_val = lhs_val * rhs_val;
                    mem.pop_back();
                }
            });
        }
        else if (op == "/") {
            ctx.program.emplace_back(op_builtin_mem_op{
                .name = std::format("{0} {1} {0}", type, op),
                .ptr = +[](std::vector<block>& mem) {
                    const auto& rhs_val = std::get<block_int>(back(mem));
                    auto& lhs_val = std::get<block_int>(penult(mem));
                    if (rhs_val == 0) {
                        anzu::print("Division by zero error\n");
                        std::exit(1);
                    }
                    lhs_val = lhs_val / rhs_val;
                    mem.pop_back();
                }
            });
        }
        else if (op == "%") {
            ctx.program.emplace_back(op_builtin_mem_op{
                .name = std::format("{0} {1} {0}", type, op),
                .ptr = +[](std::vector<block>& mem) {
                    const auto& rhs_val = std::get<block_int>(back(mem));
                    auto& lhs_val = std::get<block_int>(penult(mem));
                    lhs_val = lhs_val % rhs_val;
                    mem.pop_back();
                }
            });
        }
        else if (op == "<") {
            ctx.program.emplace_back(op_builtin_mem_op{
                .name = std::format("{0} {1} {0}", type, op),
                .ptr = +[](std::vector<block>& mem) {
                    const auto& rhs_val = std::get<block_int>(back(mem));
                    auto& lhs = penult(mem);
                    auto& lhs_val = std::get<block_int>(lhs);
                    lhs = block{lhs_val < rhs_val};
                    mem.pop_back();
                }
            });
        }
        else if (op == "<=") {
            ctx.program.emplace_back(op_builtin_mem_op{
                .name = std::format("{0} {1} {0}", type, op),
                .ptr = +[](std::vector<block>& mem) {
                    const auto& rhs_val = std::get<block_int>(back(mem));
                    auto& lhs = penult(mem);
                    auto& lhs_val = std::get<block_int>(lhs);
                    lhs = block{lhs_val <= rhs_val};
                    mem.pop_back();
                }
            });
        }
        else if (op == ">") {
            ctx.program.emplace_back(op_builtin_mem_op{
                .name = std::format("{0} {1} {0}", type, op),
                .ptr = +[](std::vector<block>& mem) {
                    const auto& rhs_val = std::get<block_int>(back(mem));
                    auto& lhs = penult(mem);
                    auto& lhs_val = std::get<block_int>(lhs);
                    lhs = block{lhs_val > rhs_val};
                    mem.pop_back();
                }
            });
        }
        else if (op == ">=") {
            ctx.program.emplace_back(op_builtin_mem_op{
                .name = std::format("{0} {1} {0}", type, op),
                .ptr = +[](std::vector<block>& mem) {
                    const auto& rhs_val = std::get<block_int>(back(mem));
                    auto& lhs = penult(mem);
                    auto& lhs_val = std::get<block_int>(lhs);
                    lhs = block{lhs_val >= rhs_val};
                    mem.pop_back();
                }
            });
        }
        else if (op == "==") {
            ctx.program.emplace_back(op_builtin_mem_op{
                .name = std::format("{0} {1} {0}", type, op),
                .ptr = +[](std::vector<block>& mem) {
                    const auto& rhs_val = std::get<block_int>(back(mem));
                    auto& lhs = penult(mem);
                    auto& lhs_val = std::get<block_int>(lhs);
                    lhs = block{lhs_val == rhs_val};
                    mem.pop_back();
                }
            });
        }
        else if (op == "!=") {
            ctx.program.emplace_back(op_builtin_mem_op{
                .name = std::format("{0} {1} {0}", type, op),
                .ptr = +[](std::vector<block>& mem) {
                    const auto& rhs_val = std::get<block_int>(back(mem));
                    auto& lhs = penult(mem);
                    auto& lhs_val = std::get<block_int>(lhs);
                    lhs = block{lhs_val != rhs_val};
                    mem.pop_back();
                }
            });
        }
    }
    else if (type == bool_type()) {
        if (op == "==") {
            ctx.program.emplace_back(op_builtin_mem_op{
                .name = std::format("{0} {1} {0}", type, op),
                .ptr = +[](std::vector<block>& mem) {
                    const auto& rhs_val = std::get<block_bool>(back(mem));
                    auto& lhs_val = std::get<block_bool>(penult(mem));
                    lhs_val = lhs_val == rhs_val;
                    mem.pop_back();
                }
            });
        }
        else if (op == "!=") {
            ctx.program.emplace_back(op_builtin_mem_op{
                .name = std::format("{0} {1} {0}", type, op),
                .ptr = +[](std::vector<block>& mem) {
                    const auto& rhs_val = std::get<block_bool>(back(mem));
                    auto& lhs_val = std::get<block_bool>(penult(mem));
                    lhs_val = lhs_val != rhs_val;
                    mem.pop_back();
                }
            });
        }
        else if (op == "&&") {
            ctx.program.emplace_back(op_builtin_mem_op{
                .name = std::format("{0} {1} {0}", type, op),
                .ptr = +[](std::vector<block>& mem) {
                    const auto& rhs_val = std::get<block_bool>(back(mem));
                    auto& lhs_val = std::get<block_bool>(penult(mem));
                    lhs_val = lhs_val && rhs_val;
                    mem.pop_back();
                }
            });
        }
        else if (op == "||") {
            ctx.program.emplace_back(op_builtin_mem_op{
                .name = std::format("{0} {1} {0}", type, op),
                .ptr = +[](std::vector<block>& mem) {
                    const auto& rhs_val = std::get<block_bool>(back(mem));
                    auto& lhs_val = std::get<block_bool>(penult(mem));
                    lhs_val = lhs_val || rhs_val;
                    mem.pop_back();
                }
            });
        }
    }
    else if (type == str_type()) {
        if (op == "+") {
            ctx.program.emplace_back(op_builtin_mem_op{
                .name = std::format("{0} {1} {0}", type, op),
                .ptr = +[](std::vector<block>& mem) {
                    const auto& rhs_val = std::get<block_str>(back(mem));
                    auto& lhs_val = std::get<block_str>(penult(mem));
                    lhs_val = lhs_val + rhs_val;
                    mem.pop_back();
                }
            });
        }
        else if (op == "==") {
            ctx.program.emplace_back(op_builtin_mem_op{
                .name = std::format("{0} {1} {0}", type, op),
                .ptr = +[](std::vector<block>& mem) {
                    const auto& rhs_val = std::get<block_str>(back(mem));
                    auto& lhs = penult(mem);
                    auto& lhs_val = std::get<block_str>(lhs);
                    lhs = block{lhs_val == rhs_val};
                    mem.pop_back();
                }
            });
        }
        else if (op == "!=") {
            ctx.program.emplace_back(op_builtin_mem_op{
                .name = std::format("{0} {1} {0}", type, op),
                .ptr = +[](std::vector<block>& mem) {
                    const auto& rhs_val = std::get<block_str>(back(mem));
                    auto& lhs = penult(mem);
                    auto& lhs_val = std::get<block_str>(lhs);
                    lhs = block{lhs_val != rhs_val};
                    mem.pop_back();
                }
            });
        }
    }
    else {
        invalid_expr();
    }
}

void compile_node(const node_function_call_expr& node, compiler_context& ctx)
{
    compile_function_call(node.function_name, node.args, ctx);
}

void compile_node(const node_list_expr& node, compiler_context& ctx)
{
    for (const auto& element : node.elements | std::views::reverse) {
        compile_node(*element, ctx);
    }
    ctx.program.emplace_back(op_build_list{ .size = node.elements.size() });
}

void compile_node(const node_sequence_stmt& node, compiler_context& ctx)
{
    for (const auto& seq_node : node.sequence) {
        compile_node(*seq_node, ctx);
    }
}

void compile_node(const node_while_stmt& node, compiler_context& ctx)
{
    const auto begin_pos = append_op(ctx, op_loop_begin{});
    compile_node(*node.condition, ctx);
    const auto jump_pos = append_op(ctx, op_jump_if_false{});
    compile_node(*node.body, ctx);
    const auto end_pos = append_op(ctx, op_loop_end{ .jump=begin_pos });
    link_up_jumps(ctx, begin_pos, jump_pos, end_pos);
}

void compile_node(const node_if_stmt& node, compiler_context& ctx)
{
    const auto if_pos = append_op(ctx, op_if{});
    compile_node(*node.condition, ctx);
    const auto jump_pos = append_op(ctx, op_jump_if_false{});
    compile_node(*node.body, ctx);

    if (node.else_body) {
        const auto else_pos = append_op(ctx, op_else{});
        compile_node(*node.else_body, ctx);
        ctx.program.emplace_back(anzu::op_if_end{});
        std::get<op_jump_if_false>(ctx.program[jump_pos]).jump = else_pos + 1; // Jump into the else block if false
        std::get<op_else>(ctx.program[else_pos]).jump = std::ssize(ctx.program); // Jump past the end if false
    } else {
        ctx.program.emplace_back(anzu::op_if_end{});
        std::get<op_jump_if_false>(ctx.program[jump_pos]).jump = std::ssize(ctx.program); // Jump past the end if false
    }
}

void compile_node(const node_for_stmt& node, compiler_context& ctx)
{
    const auto container_name = std::string{"_Container"};
    const auto index_name = std::string{"_Index"};

    // Push the container to the stack
    compile_node(*node.container, ctx);
    declare_variable_name(ctx, container_name);
    save_variable(ctx, container_name);

    // Push the counter to the stack
    ctx.program.emplace_back(anzu::op_load_literal{
        .value=make_int(0)
    });
    declare_variable_name(ctx, index_name);
    save_variable(ctx, index_name);

    declare_variable_name(ctx, node.var);

    const auto begin_pos = append_op(ctx, op_loop_begin{});

    load_variable(ctx, index_name);
    load_variable(ctx, container_name);
    call_builtin(ctx, "list_size");

    // OP_NE - TODO: Make this better
    ctx.program.emplace_back(op_builtin_mem_op{
        .name = "int != int",
        .ptr = +[](std::vector<block>& mem) {
            const auto& rhs_val = std::get<block_int>(back(mem));
            auto& lhs = penult(mem);
            auto& lhs_val = std::get<block_int>(lhs);
            lhs = block{lhs_val != rhs_val};
            mem.pop_back();
        }
    });
    
    const auto jump_pos = append_op(ctx, op_jump_if_false{}); // If size == index, jump to end

    load_variable(ctx, container_name);
    load_variable(ctx, index_name);
    call_builtin(ctx, "list_at");
    save_variable(ctx, node.var);

    compile_node(*node.body, ctx);

    // Increment the index
    load_variable(ctx, index_name);
    ctx.program.emplace_back(op_builtin_mem_op{
        .name = "increment",
        .ptr = +[](std::vector<block>& mem) {
            ++std::get<block_int>(back(mem));
        }
    });
    save_variable(ctx, index_name);

    const auto end_pos = append_op(ctx, op_loop_end{ .jump=begin_pos });

    link_up_jumps(ctx, begin_pos, jump_pos, end_pos);
}

void compile_node(const node_break_stmt&, compiler_context& ctx)
{
    ctx.program.emplace_back(anzu::op_break{});
}

void compile_node(const node_continue_stmt&, compiler_context& ctx)
{
    ctx.program.emplace_back(anzu::op_continue{});
}

void compile_node(const node_declaration_stmt& node, compiler_context& ctx)
{
    compile_node(*node.expr, ctx);
    declare_variable_name(ctx, node.name);
    save_variable(ctx, node.name);
}

void compile_node(const node_assignment_stmt& node, compiler_context& ctx)
{
    compile_node(*node.expr, ctx);
    save_variable(ctx, node.name);
}

void compile_node(const node_function_def_stmt& node, compiler_context& ctx)
{
    const auto begin_pos = append_op(ctx, op_function{ .name=node.name, .sig=node.sig });
    ctx.functions[node.name] = { .sig=node.sig ,.ptr=begin_pos };

    ctx.locals.emplace();
    for (const auto& arg : node.sig.args) {
        declare_variable_name(ctx, arg.name);
    }
    compile_node(*node.body, ctx);
    ctx.locals.reset();

    const auto end_pos = append_op(ctx, op_function_end{});

    std::get<anzu::op_function>(ctx.program[begin_pos]).jump = end_pos + 1;
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

auto compile(const node_stmt_ptr& root, const expr_types& types) -> anzu::program
{
    anzu::compiler_context ctx;
    ctx.expr_types = types;
    compile_node(*root, ctx);
    return ctx.program;
}

}