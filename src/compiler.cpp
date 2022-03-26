#include "compiler.hpp"
#include "lexer.hpp"
#include "object.hpp"
#include "parser.hpp"
#include "functions.hpp"
#include "operators.hpp"
#include "utility/print.hpp"
#include "utility/overloaded.hpp"

#include <string_view>
#include <optional>
#include <tuple>
#include <vector>
#include <unordered_map>

namespace anzu {
namespace {

struct addrof
{
    std::size_t position;
    bool        is_local; // If true, the position is an offset
    type_name   type;
};

struct var_info
{
    std::size_t location;
    type_name   type;
};

struct var_locations
{
    std::unordered_map<std::string, var_info> info;
    std::size_t next = 0;
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

    var_locations globals;
    std::optional<var_locations> locals;

    type_info type_info;
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
auto declare_variable_name(compiler_context& ctx, const std::string& name, const type_name& type) -> void
{
    auto& vars = ctx.locals.has_value() ? ctx.locals.value() : ctx.globals;
    const auto [iter, success] = vars.info.emplace(name, var_info{vars.next, type});
    if (success) { // If not successful, then the name already existed, so dont increase
        vars.next += ctx.type_info.types.block_size(type);
    }
}

auto save_variable(compiler_context& ctx, const std::string& name) -> void
{
    if (ctx.locals && ctx.locals->info.contains(name)) {
        const auto& info = ctx.locals->info.at(name);
        ctx.program.emplace_back(anzu::op_save_local{
            .name=name, .offset=info.location, .size=ctx.type_info.types.block_size(info.type)
        });
        return;
    }
    
    if (ctx.globals.info.contains(name)) {
        const auto& info = ctx.globals.info.at(name);
        ctx.program.emplace_back(anzu::op_save_global{
            .name=name, .position=info.location, .size=ctx.type_info.types.block_size(info.type)
        });
        return;
    }

    anzu::print("BAD! Could not assign to variable\n");
    std::exit(1);
}

auto load_variable(compiler_context& ctx, const std::string& name) -> void
{
    if (ctx.locals) {
        if (auto it = ctx.locals->info.find(name); it != ctx.locals->info.end()) {
            ctx.program.emplace_back(anzu::op_load_local{
                .name=name,
                .offset=it->second.location,
                .size=ctx.type_info.types.block_size(it->second.type)
            });
            return;
        }
    }
    if (auto it = ctx.globals.info.find(name); it != ctx.globals.info.end()) {
        ctx.program.emplace_back(anzu::op_load_global{
            .name=name,
            .position=it->second.location,
            .size=ctx.type_info.types.block_size(it->second.type)
        });
    }
}

auto signature_args_size(const compiler_context& ctx, const signature& sig) -> std::size_t
{
    auto args_size = std::size_t{0};
    for (const auto& arg : sig.args) {
        args_size += ctx.type_info.types.block_size(arg.type);
    }
    return args_size;
}

auto call_builtin(compiler_context& ctx, const std::string& function_name) -> void
{
    const auto& func = anzu::fetch_builtin(function_name);
    ctx.program.emplace_back(anzu::op_builtin_call{
        .name=function_name, .ptr=func.ptr, .args_size=signature_args_size(ctx, func.sig)
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

auto address_of(
    const compiler_context& ctx, const std::string& name, std::span<const std::string> fields
)
    -> addrof
{
    if (ctx.locals && ctx.locals->info.contains(name)) {
        const auto& info = ctx.locals->info.at(name);
        auto location = info.location;
        auto type = info.type;
        for (const auto& field_name : fields) { // Field names in the assignemnt
            for (const auto& field : ctx.type_info.types.get_fields(type)) { // Field of curr type
                if (field.name == field_name) {
                    type = field.type;
                    break;
                }
                location += ctx.type_info.types.block_size(field.type);
            }
        }
        return { .position=location, .is_local=true, .type=type};
    }
    
    const auto& info = ctx.globals.info.at(name);
    auto location = info.location;
    auto type = info.type;
    for (const auto& field_name : fields) { // Field names in the assignemnt
        for (const auto& field : ctx.type_info.types.get_fields(type)) { // Field of curr type
            if (field.name == field_name) {
                type = field.type;
                break;
            }
            location += ctx.type_info.types.block_size(field.type);
        }
    }
    return { .position=location, .is_local=false, .type=type};
}

auto address_of_expr(const compiler_context& ctx, const node_expr& node) -> addrof
{
    return std::visit(overloaded{
        [&](const node_variable_expr& n) {
            return address_of(ctx, n.name, {});
        },
        [&](const node_field_expr& n) {
            if (!std::holds_alternative<node_variable_expr>(*n.expression)) {
                print("not currently supporting field access of non-variables\n");
                std::exit(1);
            }
            const auto& var = std::get<node_variable_expr>(*n.expression);
            return address_of(ctx, var.name, std::vector{n.field_name});
        },
        [](const auto&) {
            print("compiler error: cannot take address of a non-lvalue\n");
            std::exit(1);
            return anzu::addrof{.position=0, .is_local=false, .type=int_type()};
        }
    }, node);
}

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

// Returns the size of the return type
auto compile_function_call(
    const std::string& function,
    const std::vector<node_expr_ptr>& args,
    compiler_context& ctx
)
    -> std::size_t
{
    // Push the args to the stack
    for (const auto& arg : args) {
        compile_node(*arg, ctx);
    }

    // If this is the name of a simple type, then this is a constructor call, so
    // there is currently nothing to do since the arguments are already pushed to
    // the stack.
    const auto as_type_name = type_name{type_simple{ .name=function }};
    if (ctx.type_info.types.is_registered_type(as_type_name)) {
        return ctx.type_info.types.block_size(as_type_name);
    }

    // Otherwise, it may be a custom function.
    else if (const auto function_def = find_function(ctx, function)) {
        ctx.program.emplace_back(anzu::op_function_call{
            .name=function,
            .ptr=function_def->ptr + 1, // Jump into the function
            .args_size=signature_args_size(ctx, function_def->sig),
            .return_size=ctx.type_info.types.block_size(function_def->sig.return_type)
        });
        return ctx.type_info.types.block_size(function_def->sig.return_type);
    }

    // Otherwise, it must be a builtin function.
    const auto& builtin = anzu::fetch_builtin(function);

    // TODO: Make this more generic, but we need to fill in the types before
    // calling here, so that we can pass in the correct block count
    auto sig = builtin.sig;
    if (function == "print" || function == "println") {
        sig.args[0].type = ctx.type_info.expr_types[args[0].get()];
    }

    ctx.program.emplace_back(anzu::op_builtin_call{
        .name=function,
        .ptr=builtin.ptr,
        .args_size=signature_args_size(ctx, sig)
    });
    return ctx.type_info.types.block_size(sig.return_type);
}

void compile_node(const node_expr& expr, const node_literal_expr& node, compiler_context& ctx)
{
    if (node.value.data.size() != 1) {
        anzu::print("Objects with block-size != 1 not currently supported\n");
        std::exit(1);
    }
    ctx.program.emplace_back(anzu::op_load_literal{ .value=node.value });
}

void compile_node(const node_expr& expr, const node_variable_expr& node, compiler_context& ctx)
{
    load_variable(ctx, node.name);
}

void compile_node(const node_expr& expr, const node_field_expr& node, compiler_context& ctx)
{
    const auto& var_name = std::get<node_variable_expr>(*node.expression).name;
    const auto& var_type = ctx.type_info.expr_types[node.expression.get()];

    const auto addr_of = address_of_expr(ctx, expr);
    const auto type_size = ctx.type_info.types.block_size(addr_of.type);
    if (addr_of.is_local) {
        ctx.program.emplace_back(op_load_local{
            .name = std::format("{}.{}", var_name, node.field_name),
            .offset = addr_of.position,
            .size = type_size
        });
    } else {
        ctx.program.emplace_back(op_load_global{
            .name = std::format("{}.{}", var_name, node.field_name),
            .position = addr_of.position,
            .size = type_size
        });
    }
}

// This is a copy of the logic from typecheck.cpp now, pretty bad, we should make it more
// generic and combine the logic.
void compile_node(const node_expr& expr, const node_bin_op_expr& node, compiler_context& ctx)
{
    compile_node(*node.lhs, ctx);
    compile_node(*node.rhs, ctx);
    const auto op = node.token.text;
    const auto lhs_type = ctx.type_info.expr_types[node.lhs.get()];
    const auto rhs_type = ctx.type_info.expr_types[node.rhs.get()];
    const auto info = resolve_bin_op({.op = op, .lhs = lhs_type, .rhs = rhs_type});

    if (!info) {
        anzu::print("[{}:{}] could not evaluate '{} {} {}'", node.token.line, node.token.col, lhs_type, op, rhs_type);
        std::exit(1);
    }

    ctx.program.emplace_back(op_builtin_mem_op{
        .name = std::format("{} {} {}", lhs_type, op, rhs_type),
        .ptr = info->operator_func
    });
}

void compile_node(const node_expr& expr, const node_function_call_expr& node, compiler_context& ctx)
{
    compile_function_call(node.function_name, node.args, ctx);
}

void compile_node(const node_expr& expr, const node_list_expr& node, compiler_context& ctx)
{
    for (const auto& element : node.elements | std::views::reverse) {
        compile_node(*element, ctx);
    }
    ctx.program.emplace_back(op_build_list{ .size = node.elements.size() });
}

void compile_node(const node_expr& expr, const node_addrof_expr& node, compiler_context& ctx)
{
    const auto addr_of = address_of_expr(ctx, *node.expr);
    const auto type_of = ctx.type_info.expr_types[node.expr.get()];
    const auto size_of = ctx.type_info.types.block_size(type_of);

    if (addr_of.is_local) {
        ctx.program.emplace_back(op_load_addr_of_local{
            .offset=addr_of.position, .size=size_of
        });
    } else {
        ctx.program.emplace_back(op_load_addr_of_global{
            .position=addr_of.position, .size=size_of
        });
    }
}

void compile_node(const node_expr& expr, const node_deref_expr& node, compiler_context& ctx)
{
    compile_node(*node.expr, ctx);
    ctx.program.emplace_back(op_deref{});
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

void compile_node(const node_struct_stmt& node, compiler_context& ctx)
{

}

// TODO: This only works if the contained type has size 1, because lists are broken
void compile_node(const node_for_stmt& node, compiler_context& ctx)
{
    const auto container_name = std::string{"_Container"};
    const auto container_type = ctx.type_info.expr_types[node.container.get()];
    const auto contained_type = match(container_type, generic_list_type())->at(0);
    const auto index_name = std::string{"_Index"};

    // Push the container to the stack
    compile_node(*node.container, ctx);
    declare_variable_name(ctx, container_name, container_type);
    save_variable(ctx, container_name); // Currently only lists are allowed in for stmts

    // Push the counter to the stack
    ctx.program.emplace_back(anzu::op_load_literal{
        .value=make_int(0)
    });
    declare_variable_name(ctx, index_name, int_type());
    save_variable(ctx, index_name);

    declare_variable_name(ctx, node.var, contained_type);

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
    declare_variable_name(ctx, node.name, ctx.type_info.expr_types[node.expr.get()]);
    save_variable(ctx, node.name);
}

void compile_node(const node_assignment_stmt& node, compiler_context& ctx)
{
    compile_node(*node.expr, ctx);
    if (!std::holds_alternative<node_variable_expr>(*node.position)) {
        print("currently cannot assign to a non-variable\n");
        std::exit(1);
    }
    const auto& name = std::get<node_variable_expr>(*node.position).name;
    save_variable(ctx, name);
}

void compile_node(const node_field_assignment_stmt& node, compiler_context& ctx)
{
    compile_node(*node.expr, ctx);
    const auto addr = address_of(ctx, node.name, node.fields);
    if (addr.is_local) {
        ctx.program.emplace_back(op_save_local{
            .name="temp", .offset=addr.position, .size=ctx.type_info.types.block_size(addr.type)
        });
    } else {
        ctx.program.emplace_back(op_save_local{
            .name="temp", .offset=addr.position, .size=ctx.type_info.types.block_size(addr.type)
        });
    }
}

void compile_node(const node_function_def_stmt& node, compiler_context& ctx)
{
    const auto begin_pos = append_op(ctx, op_function{ .name=node.name, .sig=node.sig });
    ctx.functions[node.name] = { .sig=node.sig ,.ptr=begin_pos };

    ctx.locals.emplace();
    for (const auto& arg : node.sig.args) {
        declare_variable_name(ctx, arg.name, arg.type);
    }
    compile_node(*node.body, ctx);
    ctx.locals.reset();

    const auto end_pos = append_op(ctx, op_function_end{});

    std::get<anzu::op_function>(ctx.program[begin_pos]).jump = end_pos + 1;
}

void compile_node(const node_function_call_stmt& node, compiler_context& ctx)
{
    const auto return_size = compile_function_call(node.function_name, node.args, ctx);
    ctx.program.emplace_back(anzu::op_pop{ .size=return_size });
}

void compile_node(const node_return_stmt& node, compiler_context& ctx)
{
    compile_node(*node.return_value, ctx);
    ctx.program.emplace_back(anzu::op_return{});
}

auto compile_node(const node_expr& expr, compiler_context& ctx) -> void
{
    std::visit([&](const auto& node) { compile_node(expr, node, ctx); }, expr);
}

auto compile_node(const node_stmt& root, compiler_context& ctx) -> void
{
    std::visit([&](const auto& node) { compile_node(node, ctx); }, root);
}

}

auto compile(const node_stmt_ptr& root, const type_info& types) -> anzu::program
{
    anzu::compiler_context ctx;
    ctx.type_info = types;
    compile_node(*root, ctx);
    return ctx.program;
}

}