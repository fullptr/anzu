#include "compiler.hpp"
#include "lexer.hpp"
#include "object.hpp"
#include "parser.hpp"
#include "functions.hpp"
#include "operators.hpp"
#include "utility/print.hpp"
#include "utility/overloaded.hpp"
#include "utility/views.hpp"

#include <string_view>
#include <optional>
#include <tuple>
#include <vector>
#include <unordered_map>

namespace anzu {
namespace {

template <typename... Args>
[[noreturn]] void compiler_error(const token& tok, std::string_view msg, Args&&... args)
{
    const auto formatted_msg = std::format(msg, std::forward<Args>(args)...);
    anzu::print("[ERROR] ({}:{}) {}\n", tok.line, tok.col, formatted_msg);
    std::exit(1);
}

template <typename... Args>
[[noreturn]] void compiler_assert(bool cond, const token& tok, std::string_view msg, Args&&... args)
{
    if (!cond) {
        compiler_error(tok, msg, std::forward<Args>(args)...);
    }
}

struct var_info
{
    std::size_t location;
    type_name   type;
    std::size_t type_size;
};

class var_locations
{
    std::vector<std::unordered_map<std::string, var_info>> d_scopes;
    std::size_t d_next = 0;

public:
    var_locations()
    {
        d_scopes.emplace_back();
    }

    auto declare(const std::string& name, const type_name& type, std::size_t type_size) -> bool
    {
        const auto [_, success] = d_scopes.back().emplace(name, var_info{d_next, type, type_size});
        if (success) {
            d_next += type_size;
        }
        return success;
    }

    auto find(const std::string& name) const -> std::optional<var_info>
    {
        for (const auto& scope : d_scopes | std::views::reverse) {
            if (const auto it = scope.find(name); it != scope.end()) {
                return it->second;
            }
        }
        return std::nullopt;
    }

    auto push_scope() -> void
    {
        d_scopes.emplace_back();
    }

    auto pop_scope() -> std::size_t // Returns the size of the scope just popped
    {
        auto scope_size = std::size_t{0};
        for (const auto& [name, info] : d_scopes.back()) {
            scope_size += info.type_size;
        }
        d_scopes.pop_back();
        d_next -= scope_size;
        return scope_size;
    }
};

struct function_def
{
    signature   sig;
    std::size_t ptr;
};

struct current_function
{
    var_locations vars;
    type_name     return_type;
};

// Struct used to store information while compiling an AST. Contains the output program
// as well as information such as function definitions.
struct compiler
{
    anzu::program program;

    std::unordered_map<std::string, function_def> functions;

    var_locations globals;
    std::optional<current_function> current_func;

    type_store types;
};

auto current_vars(compiler& com) -> var_locations&
{
    return com.current_func ? com.current_func->vars : com.globals;
}

auto verify_real_type(const compiler& com, const token& tok, const type_name& t) -> void
{
    if (!com.types.contains(t)) {
        compiler_error(tok, "{} is not a recognised type", t);
    }
}

template <typename T>
auto append_op(compiler& com, T&& op) -> std::size_t
{
    com.program.emplace_back(std::forward<T>(op));
    return com.program.size() - 1;
}

// Registers the given name in the current scope
auto declare_variable_name(
    compiler& com, const token& tok, const std::string& name, const type_name& type
)
    -> void
{
    auto& vars = current_vars(com);
    const auto type_size = com.types.size_of(type);
    vars.declare(name, type, com.types.size_of(type));
}

auto find_variable_type(const compiler& com, const token& tok, const std::string& name) -> type_name
{
    if (com.current_func) {
        auto& locals = com.current_func->vars;
        if (const auto info = locals.find(name); info.has_value()) {
            return info->type;
        }
    }
    
    auto& globals = com.globals;
    if (const auto info = globals.find(name); info.has_value()) {
        return info->type;
    }

    compiler_error(tok, "could not find variable '{}'\n", name);
}

auto find_variable(compiler& com, const token& tok, const std::string& name) -> type_name
{
    if (com.current_func) {
        auto& locals = com.current_func->vars;
        if (const auto info = locals.find(name); info.has_value()) {
            com.program.emplace_back(op_push_local_addr{
                .offset=info->location, .size=info->type_size
            });
            return info->type;
        }
    }

    auto& globals = com.globals;
    if (const auto info = globals.find(name); info.has_value()) {
        com.program.emplace_back(op_push_global_addr{
            .position=info->location, .size=info->type_size
        });
        return info->type;
    }

    compiler_error(tok, "could not find variable '{}'\n", name);
}

auto save_variable(compiler& com, const token& tok, const std::string& name) -> void
{
    find_variable(com, tok, name);
    com.program.emplace_back(anzu::op_save{});
}

auto load_variable(compiler& com, const token& tok, const std::string& name) -> void
{
    find_variable(com, tok, name);
    com.program.emplace_back(anzu::op_load{});
}

auto signature_args_size(const compiler& com, const signature& sig) -> std::size_t
{
    auto args_size = std::size_t{0};
    for (const auto& arg : sig.args) {
        args_size += com.types.size_of(arg.type);
    }
    return args_size;
}

auto find_function(const compiler& com, const std::string& function) -> const function_def*
{
    if (const auto it = com.functions.find(function); it != com.functions.end()) {
        return &it->second;
    }
    return nullptr;
}

auto modify_ptr(compiler& com, std::size_t offset, std::size_t size) -> void
{
    com.program.emplace_back(op_load_literal{ .blk=block_uint{offset} });
    com.program.emplace_back(op_load_literal{ .blk=block_uint{size} });
    com.program.emplace_back(op_modify_ptr{});
}

// Given a type and field name, and assuming that the top of the stack at runtime is a pointer
// to an object of the given type, this function adds an op code to modify that pointer to
// instead point to the given field. Returns the type of the field.
auto compile_ptr_to_field(
    compiler& com, const token& tok, const type_name& type, const std::string& field_name
)
    -> type_name
{
    auto offset     = std::size_t{0};
    auto size       = std::size_t{0};
    auto field_type = type_name{};
    for (const auto& field : com.types.fields_of(type)) {
        const auto field_size = com.types.size_of(field.type);
        if (field.name == field_name) {
            size = field_size;
            field_type = field.type;
            break;
        }
        offset += field_size;
    }
    
    compiler_assert(size != 0, tok, "type {} has no field '{}'\n", type, field_name);
    modify_ptr(com, offset, size);
    return field_type;
}

// Both for and while loops have the form [<begin> <condition> <do> <body> <end>].
// This function links the do to jump to one past the end if false, makes breaks
// jump past the end, and makes continues jump back to the beginning.
void link_up_jumps(compiler& com, std::size_t begin, std::size_t jump, std::size_t end)
{
    // Jump past the end if false
    std::get<op_jump_if_false>(com.program[jump]).jump = end + 1;
        
    // Only set unset jumps, there may be other already set from nested loops
    for (std::size_t idx = jump + 1; idx != end; ++idx) {
        std::visit(overloaded{
            [&](op_break& op) {
                if (op.jump == 0) { op.jump = end + 1; }
            },
            [&](op_continue& op) {
                if (op.jump == 0) { op.jump = begin; }
            },
            [](auto&&) {}
        }, com.program[idx]);
    }
}

void verify_sig(const token& tok, const signature& sig, const std::vector<type_name>& args)
{
    if (sig.args.size() != args.size()) {
        compiler_error(tok, "function expected {} args, got {}", sig.args.size(), args.size());
    }

    for (const auto& [actual, expected] : zip(args, sig.args)) {
        if (actual != expected.type) {
            compiler_error(tok, "'{}' does not match '{}'", actual, expected.type);
        }
    }
}

auto make_constructor_sig(const compiler& com, const type_name& type) -> signature
{
    auto sig = signature{};
    for (const auto& field : com.types.fields_of(type)) {
        sig.args.emplace_back(field.name, field.type);
    }
    sig.return_type = type;
    return sig;
}

auto function_ends_with_return(const node_function_def_stmt& node) -> bool
{
    if (std::holds_alternative<node_sequence_stmt>(*node.body)) {
        const auto& seq = std::get<node_sequence_stmt>(*node.body).sequence;
        if (seq.empty() || !std::holds_alternative<node_return_stmt>(*seq.back())) {
            return false;
        }
        return true;
    }
    return std::holds_alternative<node_return_stmt>(*node.body);
}

auto type_of_expr(const compiler& com, const node_expr& node) -> type_name
{
    return std::visit(overloaded{
        [&](const node_literal_expr& expr) { return expr.value.type; },
        [&](const node_unary_op_expr& expr) {
            const auto r = resolve_unary_op({
                .op = expr.token.text,
                .type=type_of_expr(com, *expr.expr)
            });
            return r->result_type;
        },
        [&](const node_binary_op_expr& expr) {
            const auto r = resolve_binary_op(
                com.types,
                {
                    .op = expr.token.text,
                    .lhs=type_of_expr(com, *expr.lhs),
                    .rhs=type_of_expr(com, *expr.rhs)
                }
            );
            return r->result_type;
        },
        [&](const node_function_call_expr& expr) {
            return com.functions.at(expr.function_name).sig.return_type;
        },
        [&](const node_list_expr& expr) {
            return type_name{type_list{
                .inner_type = type_of_expr(com, *expr.elements.front()),
                .count = expr.elements.size()
            }};
        },
        [&](const node_addrof_expr& expr) {
            return concrete_ptr_type(type_of_expr(com, *expr.expr));
        },
        [&](const node_sizeof_expr& expr) {
            return uint_type();
        },
        [&](const node_variable_expr& expr) {
            return find_variable_type(com, expr.token, expr.name);
        },
        [&](const node_field_expr& expr) {
            const auto type = type_of_expr(com, *expr.expr);
            for (const auto& field : com.types.fields_of(type)) {
                if (field.name == expr.field_name) {
                    return field.type;
                }
            }
            return int_type();
        },
        [&](const node_arrow_expr& expr) {
            const auto ptype = type_of_expr(com, *expr.expr);
            compiler_assert(is_ptr_type(ptype), expr.token, "cannot use arrow operator on non-ptr type '{}'", ptype);
            const auto type = inner_type(ptype);
            for (const auto& field : com.types.fields_of(type)) {
                if (field.name == expr.field_name) {
                    return field.type;
                }
            }
            return int_type();
        },
        [&](const node_deref_expr& expr) {
            const auto ptype = type_of_expr(com, *expr.expr);
            compiler_assert(is_ptr_type(ptype), expr.token, "cannot use deref operator on non-ptr type '{}'", ptype);
            return inner_type(ptype);
        },
        [&](const node_subscript_expr& expr) {
            const auto ltype = type_of_expr(com, *expr.expr);
            if (!std::holds_alternative<type_list>(ltype)) {
                compiler_error(expr.token, "cannot use subscript operator on non-list type '{}'", ltype);
            }
            return *std::get<type_list>(ltype).inner_type;
        }
    }, node);
}

auto compile_expr_ptr(compiler& com, const node_expr& node) -> type_name;
auto compile_expr_val(compiler& com, const node_expr& expr) -> type_name;
auto compile_stmt(compiler& com, const node_stmt& root) -> void;

auto compile_expr_ptr(compiler& com, const node_variable_expr& node) -> type_name
{
    return find_variable(com, node.token, node.name);
}

auto compile_expr_ptr(compiler& com, const node_field_expr& node) -> type_name
{
    const auto type = compile_expr_ptr(com, *node.expr);
    return compile_ptr_to_field(com, node.token, type, node.field_name);
}

auto compile_expr_ptr(compiler& com, const node_arrow_expr& node) -> type_name
{
    const auto type = compile_expr_val(com, *node.expr); // Push the address
    compiler_assert(is_ptr_type(type), node.token, "cannot use arrow operator on non-ptr type '{}'", type);
    return compile_ptr_to_field(com, node.token, type, node.field_name);
}

auto compile_expr_ptr(compiler& com, const node_deref_expr& node) -> type_name
{
    const auto type = compile_expr_val(com, *node.expr); // Push the address
    compiler_assert(is_ptr_type(type), node.token, "cannot use deref operator on non-ptr type '{}'", type);
    return inner_type(type);
}

auto compile_expr_ptr(compiler& com, const node_subscript_expr& expr) -> type_name
{
    const auto ltype = compile_expr_ptr(com, *expr.expr);
    if (!std::holds_alternative<type_list>(ltype)) {
        compiler_error(expr.token, "cannot use subscript operator on non-list type '{}'", ltype);
    }
    const auto etype = *std::get<type_list>(ltype).inner_type;
    const auto etype_size = com.types.size_of(etype);

    // Push the offset (index * size)
    const auto itype = compile_expr_val(com, *expr.index);
    compiler_assert(itype == uint_type(), expr.token, "subscript argument must be a 'uint', got '{}'", itype);

    com.program.emplace_back(op_load_literal{ .blk={block_uint{etype_size}} });
    const auto info = resolve_binary_op(com.types, { .op="*", .lhs=itype, .rhs=itype });
    com.program.emplace_back(op_builtin_mem_op{
        .name = "uint * uint",
        .ptr = info->operator_func
    });

    // Push the size
    com.program.emplace_back(op_load_literal{ .blk={block_uint{etype_size}} });

    com.program.emplace_back(op_modify_ptr{});
    return etype;
}

[[noreturn]] auto compile_expr_ptr(compiler& com, const auto& node) -> type_name
{
    compiler_error(node.token, "cannot take address of a non-lvalue\n");
}

auto compile_expr_ptr(compiler& com, const node_expr& node) -> type_name
{
    return std::visit([&](const auto& expr) { return compile_expr_ptr(com, expr); }, node);
}



auto compile_expr_val(compiler& com, const node_literal_expr& node) -> type_name
{
    for (const auto& blk : node.value.data) {
        com.program.emplace_back(anzu::op_load_literal{ .blk=blk });
    }
    return node.value.type;
}

auto compile_expr_val(compiler& com, const node_binary_op_expr& node) -> type_name
{
    const auto lhs = compile_expr_val(com, *node.lhs);
    const auto rhs = compile_expr_val(com, *node.rhs);
    const auto op = node.token.text;

    const auto info = resolve_binary_op(com.types, { .op=op, .lhs=lhs, .rhs=rhs });
    compiler_assert(info.has_value(), node.token, "could not evaluate '{} {} {}'", lhs, op, rhs);

    com.program.emplace_back(op_builtin_mem_op{
        .name = std::format("{} {} {}", lhs, op, rhs),
        .ptr = info->operator_func
    });
    return info->result_type;
}

auto compile_expr_val(compiler& com, const node_unary_op_expr& node) -> type_name
{
    const auto type = compile_expr_val(com, *node.expr);
    const auto op = node.token.text;
    const auto info = resolve_unary_op({.op = op, .type = type});
    compiler_assert(info.has_value(), node.token, "could not evaluate '{}{}'", op, type);

    com.program.emplace_back(op_builtin_mem_op{
        .name = std::format("{}{}", op, type),
        .ptr = info->operator_func
    });
    return info->result_type;
} 

auto compile_expr_val(compiler& com, const node_function_call_expr& node) -> type_name
{
    // If this is the name of a simple type, then this is a constructor call, so
    // there is currently nothing to do since the arguments are already pushed to
    // the stack.
    if (const auto type = make_type(node.function_name); com.types.contains(type)) {
        const auto sig = make_constructor_sig(com, type);
        std::vector<type_name> param_types;
        for (const auto& arg : node.args) {
            param_types.emplace_back(compile_expr_val(com, *arg));
        }
        verify_sig(node.token, sig, param_types);
        return type;
    }
    // Otherwise, it may be a custom function.
    else if (const auto function_def = find_function(com, node.function_name)) {
        static constexpr auto payload_size = std::size_t{3};
        const auto return_size = com.types.size_of(function_def->sig.return_type);
        com.program.emplace_back(op_load_literal{ .blk=block_uint{0} }); // base ptr
        com.program.emplace_back(op_load_literal{ .blk=block_uint{0} }); // prog ptr
        com.program.emplace_back(op_load_literal{ .blk=block_uint{return_size} });
        
        // Push the args to the stack
        std::vector<type_name> param_types;
        for (const auto& arg : node.args) {
            param_types.emplace_back(compile_expr_val(com, *arg));
        }
        verify_sig(node.token, function_def->sig, param_types);
        com.program.emplace_back(anzu::op_function_call{
            .name=node.function_name,
            .ptr=function_def->ptr + 1, // Jump into the function
            .args_size=signature_args_size(com, function_def->sig) + payload_size
        });
        return function_def->sig.return_type;
    }

    // Otherwise, it must be a builtin function.

    // Push the args to the stack
    std::vector<type_name> param_types;
    for (const auto& arg : node.args) {
        param_types.emplace_back(compile_expr_val(com, *arg));
    }

    const auto& builtin = anzu::fetch_builtin(node.function_name);

    // TODO: Make this more generic, but we need to fill in the types before
    // calling here, so that we can pass in the correct block count
    auto sig = builtin.sig;
    if (node.function_name == "print" || node.function_name == "println") {
        sig.args[0].type = param_types[0];
    }

    com.program.emplace_back(anzu::op_builtin_call{
        .name=node.function_name,
        .ptr=builtin.ptr,
        .args_size=signature_args_size(com, sig)
    });
    return sig.return_type;
}

auto compile_expr_val(compiler& com, const node_list_expr& node) -> type_name
{
    compiler_assert(!node.elements.empty(), node.token, "currently do not support empty list literals");

    const auto inner_type = compile_expr_val(com, *node.elements.front());
    for (const auto& element : node.elements | std::views::drop(1)) {
        const auto element_type = compile_expr_val(com, *element);
        compiler_assert(element_type == inner_type, node.token, "list has mismatching element types");
    }
    return concrete_list_type(inner_type, node.elements.size());
}

auto compile_expr_val(compiler& com, const node_addrof_expr& node) -> type_name
{
    const auto type = compile_expr_ptr(com, *node.expr);
    return concrete_ptr_type(type);
}

auto compile_expr_val(compiler& com, const node_sizeof_expr& node) -> type_name
{
    const auto type = type_of_expr(com, *node.expr);
    const auto size = com.types.size_of(type);
    com.program.emplace_back(op_load_literal{ .blk=block_uint{size} });
    return uint_type();
}

// If not implemented explicitly, assume that the given node_expr is an lvalue, in which case
// we can load it by pushing the address to the stack and loading.
auto compile_expr_val(compiler& com, const auto& node) -> type_name
{
    const auto type = compile_expr_ptr(com, node);
    com.program.emplace_back(op_load{});
    return type;
}

void compile_stmt(compiler& com, const node_sequence_stmt& node)
{
    current_vars(com).push_scope();
    for (const auto& seq_node : node.sequence) {
        compile_stmt(com, *seq_node);
    }
    const auto scope_size = current_vars(com).pop_scope();
    com.program.emplace_back(op_pop{scope_size});
}

void compile_stmt(compiler& com, const node_while_stmt& node)
{
    const auto begin_pos = append_op(com, op_loop_begin{});
    const auto cond_type = compile_expr_val(com, *node.condition);
    compiler_assert(cond_type == bool_type(), node.token, "while-stmt expected bool, got {}", cond_type);

    const auto jump_pos = append_op(com, op_jump_if_false{});
    compile_stmt(com, *node.body);
    const auto end_pos = append_op(com, op_loop_end{ .jump=begin_pos });
    link_up_jumps(com, begin_pos, jump_pos, end_pos);
}

void compile_stmt(compiler& com, const node_if_stmt& node)
{
    const auto if_pos = append_op(com, op_if{});
    const auto cond_type = compile_expr_val(com, *node.condition);
    compiler_assert(cond_type == bool_type(), node.token, "if-stmt expected bool, got {}", cond_type);

    const auto jump_pos = append_op(com, op_jump_if_false{});
    compile_stmt(com, *node.body);

    if (node.else_body) {
        const auto else_pos = append_op(com, op_else{});
        compile_stmt(com, *node.else_body);
        com.program.emplace_back(anzu::op_if_end{});
        std::get<op_jump_if_false>(com.program[jump_pos]).jump = else_pos + 1; // Jump into the else block if false
        std::get<op_else>(com.program[else_pos]).jump = com.program.size(); // Jump past the end if false
    } else {
        com.program.emplace_back(anzu::op_if_end{});
        std::get<op_jump_if_false>(com.program[jump_pos]).jump = com.program.size(); // Jump past the end if false
    }
}

void compile_stmt(compiler& com, const node_struct_stmt& node)
{
    compiler_assert(!com.types.contains(node.name), node.token, "type {} already defined", node.name);

    for (const auto& field : node.fields) {
        compiler_assert(
            com.types.contains(field.type),
            node.token, 
            "unknown type {} of field {} for struct {}\n",
            field.type, field.name, node.name
        );
    }

    com.types.add(node.name, node.fields);
}

void compile_stmt(compiler& com, const node_break_stmt&)
{
    com.program.emplace_back(anzu::op_break{});
}

void compile_stmt(compiler& com, const node_continue_stmt&)
{
    com.program.emplace_back(anzu::op_continue{});
}

void compile_stmt(compiler& com, const node_declaration_stmt& node)
{
    const auto type = compile_expr_val(com, *node.expr);
    declare_variable_name(com, node.token, node.name, type);
    save_variable(com, node.token, node.name);
}

void compile_stmt(compiler& com, const node_assignment_stmt& node)
{
    const auto rhs = compile_expr_val(com, *node.expr);
    const auto lhs = compile_expr_ptr(com, *node.position);
    compiler_assert(lhs == rhs, node.token, "cannot assign a {} to a {}\n", rhs, lhs);
    com.program.emplace_back(op_save{});
}

void compile_stmt(compiler& com, const node_function_def_stmt& node)
{
    for (const auto& arg : node.sig.args) {
        verify_real_type(com, node.token, arg.type);
    }
    verify_real_type(com, node.token, node.sig.return_type);

    const auto begin_pos = append_op(com, op_function{ .name=node.name });
    com.functions[node.name] = { .sig=node.sig ,.ptr=begin_pos };

    com.current_func.emplace(current_function{ .vars={}, .return_type=node.sig.return_type });
    declare_variable_name(com, node.token, "# old_base_ptr", uint_type()); // Store the old base ptr
    declare_variable_name(com, node.token, "# old_prog_ptr", uint_type()); // Store the old program ptr
    declare_variable_name(com, node.token, "# return_size", uint_type());  // Store the return size
    for (const auto& arg : node.sig.args) {
        declare_variable_name(com, node.token, arg.name, arg.type);
    }
    compile_stmt(com, *node.body);
    com.current_func.reset();

    if (!function_ends_with_return(node)) {
        // A function returning null does not need a final return statement, and in this case
        // we manually add a return value of null here.
        if (node.sig.return_type == null_type()) {
            com.program.emplace_back(op_load_literal{block_null{}});
            com.program.emplace_back(op_return{});
        } else {
            compiler_error(node.token, "function '{}' does not end in a return statement", node.name);
        }
    }
    
    std::get<anzu::op_function>(com.program[begin_pos]).jump = com.program.size();
}

void compile_stmt(compiler& com, const node_return_stmt& node)
{
    if (!com.current_func) {
        compiler_error(node.token, "return statements can only be within functions");
    }
    const auto return_type = compile_expr_val(com, *node.return_value);
    if (return_type != com.current_func->return_type) {
        compiler_error(
            node.token,
            "mismatched return type, expected {}, got {}",
            com.current_func->return_type, return_type
        );
    }
    com.program.emplace_back(anzu::op_return{});
}

void compile_stmt(compiler& com, const node_expression_stmt& node)
{
    const auto type = compile_expr_val(com, *node.expr);
    com.program.emplace_back(anzu::op_pop{ .size=com.types.size_of(type) });
}

auto compile_expr_val(compiler& com, const node_expr& expr) -> type_name
{
    return std::visit([&](const auto& node) { return compile_expr_val(com, node); }, expr);
}

auto compile_stmt(compiler& com, const node_stmt& root) -> void
{
    std::visit([&](const auto& node) { compile_stmt(com, node); }, root);
}

}

auto compile(const node_stmt_ptr& root) -> anzu::program
{
    anzu::compiler com;
    compile_stmt(com, *root);
    return com.program;
}

}