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
#include <stack>
#include <unordered_map>
#include <unordered_set>

namespace anzu {
namespace {

template <typename... Args>
[[noreturn]] void compiler_error(const token& tok, std::string_view msg, Args&&... args)
{
    const auto formatted_msg = std::format(msg, std::forward<Args>(args)...);
    print("[ERROR] ({}:{}) {}\n", tok.line, tok.col, formatted_msg);
    std::exit(1);
}

template <typename... Args>
void compiler_assert(bool cond, const token& tok, std::string_view msg, Args&&... args)
{
    if (!cond) {
        compiler_error(tok, msg, std::forward<Args>(args)...);
    }
}

template <typename... Args>
auto compiler_assert_eq(
    const auto& left, const auto& right, const token& tok, std::string_view msg, Args&&... args
)
    -> void
{
    if (left != right) {
        const auto user_msg = std::format(msg, std::forward<Args>(args)...);
        compiler_error(tok, "{}: expected {}, got {}", user_msg, right, left);
    }
}

struct var_info
{
    std::size_t location;
    type_name   type;
    std::size_t type_size;
};

struct var_scope
{
    enum class scope_type
    {
        seq_stmt,
        while_stmt
    };

    scope_type type;
    std::unordered_map<std::string, var_info> vars;
};

class var_locations
{
    std::vector<var_scope> d_scopes;
    std::size_t d_next = 0;

public:
    var_locations()
    {
        d_scopes.emplace_back();
    }

    auto declare(const std::string& name, const type_name& type, std::size_t type_size) -> bool
    {
        const auto [_, success] = d_scopes.back().vars.try_emplace(name, d_next, type, type_size);
        if (success) {
            d_next += type_size;
        }
        return success;
    }

    auto find(const std::string& name) const -> std::optional<var_info>
    {
        for (const auto& scope : d_scopes | std::views::reverse) {
            if (const auto it = scope.vars.find(name); it != scope.vars.end()) {
                return it->second;
            }
        }
        return std::nullopt;
    }

    auto push_scope(var_scope::scope_type type) -> void
    {
        d_scopes.emplace_back(type);
    }

    auto pop_scope() -> std::size_t // Returns the size of the scope just popped
    {
        auto scope_size = std::size_t{0};
        for (const auto& [name, info] : d_scopes.back().vars) {
            scope_size += info.type_size;
        }
        d_scopes.pop_back();
        d_next -= scope_size;
        return scope_size;
    }

    auto current_scope() const -> const var_scope&
    {
        return d_scopes.back();
    }

    auto scopes() const -> const std::vector<var_scope>&
    {
        return d_scopes;
    }
};

struct function_key
{
    std::string            name;
    std::vector<type_name> args;
    auto operator==(const function_key&) const -> bool = default;
};

auto hash(const function_key& f) -> std::size_t
{
    auto hash_value = std::hash<std::string>{}(f.name);
    for (const auto& arg : f.args) {
        hash_value ^= hash(arg);
    }
    return hash_value;
}

struct function_val
{
    signature   sig;
    std::size_t ptr;
    token       tok;
};

struct current_function
{
    var_locations vars;
    type_name     return_type;
};

struct control_flow_frame
{
    std::unordered_set<std::size_t> continue_stmts;
    std::unordered_set<std::size_t> break_stmts;
};

// Struct used to store information while compiling an AST. Contains the output program
// as well as information such as function definitions.
struct compiler
{
    program program;

    using function_hash = decltype([](const function_key& f) { return hash(f); });
    std::unordered_map<function_key, function_val, function_hash> functions;
    std::unordered_set<std::string> function_names;

    var_locations globals;
    std::optional<current_function> current_func;

    std::stack<control_flow_frame> control_flow;

    type_store types;
};

void verify_unused_name(compiler& com, const token& tok, const std::string& name)
{
    const auto message = std::format("type '{}' already defined", name);
    compiler_assert(!com.types.contains(make_type(name)), tok, message);
    compiler_assert(!com.function_names.contains(name), tok, message);
}


template <typename T>
auto push_literal(compiler& com, const T& value) -> void
{
    const auto bytes = as_bytes(value);
    com.program.emplace_back(op_load_bytes{{bytes.begin(), bytes.end()}});
}

auto current_vars(compiler& com) -> var_locations&
{
    return com.current_func ? com.current_func->vars : com.globals;
}

auto verify_real_type(const compiler& com, const token& tok, const type_name& t) -> void
{
    compiler_assert(com.types.contains(t), tok, "{} is not a recognised type", t);
}

template <typename T>
auto append_op(compiler& com, T&& op) -> std::size_t
{
    com.program.emplace_back(std::forward<T>(op));
    return com.program.size() - 1;
}

// Registers the given name in the current scope
void declare_var(compiler& com, const token& tok, const std::string& name, const type_name& type)
{
    if (!current_vars(com).declare(name, type, com.types.size_of(type))) {
        compiler_error(tok, "name already in use: '{}'", name);
    }
}

auto get_var_type(const compiler& com, const token& tok, const std::string& name) -> type_name
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

auto push_var_addr(compiler& com, const token& tok, const std::string& name) -> type_name
{
    if (com.current_func) {
        auto& locals = com.current_func->vars;
        if (const auto info = locals.find(name); info.has_value()) {
            com.program.emplace_back(op_push_local_addr{ .offset=info->location });
            return info->type;
        }
    }

    auto& globals = com.globals;
    if (const auto info = globals.find(name); info.has_value()) {
        com.program.emplace_back(op_push_global_addr{ .position=info->location });
        return info->type;
    }

    compiler_error(tok, "could not find variable '{}'\n", name);
}

auto save_variable(compiler& com, const token& tok, const std::string& name) -> void
{
    const auto type = push_var_addr(com, tok, name);
    const auto size = com.types.size_of(type);
    com.program.emplace_back(op_save{ .size=size });
}

auto load_variable(compiler& com, const token& tok, const std::string& name) -> void
{
    const auto type = push_var_addr(com, tok, name);
    const auto size = com.types.size_of(type);
    com.program.emplace_back(op_load{ .size=size });
}

// Returns the size of the parameter list in bytes + the function payload
auto signature_args_size(const compiler& com, const signature& sig) -> std::size_t
{
    // Function payload == old_base_ptr and old_prog_ptr
    auto args_size = 2 * sizeof(std::uint64_t);
    for (const auto& arg : sig.params) {
        args_size += com.types.size_of(arg.type);
    }
    return args_size;
}

// Given a type and a field name, push the offset of the fields position relative to its
// owner onto the stack
auto compile_field_offset(
    compiler& com, const token& tok, const type_name& type, const std::string& field_name
)
    -> type_name
{
    auto offset = std::size_t{0};
    for (const auto& field : com.types.fields_of(type)) {
        if (field.name == field_name) {
            push_literal(com, offset);
            return field.type;
        }
        offset += com.types.size_of(field.type);
    }
    
    compiler_error(tok, "could not find field '{}' for type '{}'\n", field_name, type);
}

// Given a type and field name, and assuming that the top of the stack at runtime is a pointer
// to an object of the given type, this function adds an op code to modify that pointer to
// instead point to the given field. Returns the type of the field.
auto compile_ptr_to_field(
    compiler& com, const token& tok, const type_name& type, const std::string& field_name
)
    -> type_name
{
    const auto field_type = compile_field_offset(com, tok, type, field_name);
    com.program.emplace_back(op_modify_ptr{});
    return field_type;
}

void verify_sig(const token& tok, const signature& sig, const std::vector<type_name>& args)
{
    if (sig.params.size() != args.size()) {
        compiler_error(tok, "function expected {} args, got {}", sig.params.size(), args.size());
    }

    for (const auto& [actual, expected] : zip(args, sig.params)) {
        if (actual != expected.type) {
            compiler_error(tok, "'{}' does not match '{}'", actual, expected.type);
        }
    }
}

auto make_constructor_sig(const compiler& com, const type_name& type) -> signature
{
    auto sig = signature{};
    for (const auto& field : com.types.fields_of(type)) {
        sig.params.emplace_back(field.name, field.type);
    }
    sig.return_type = type;
    return sig;
}

auto function_ends_with_return(const node_stmt& node) -> bool
{
    if (std::holds_alternative<node_sequence_stmt>(node)) {
        const auto& seq = std::get<node_sequence_stmt>(node).sequence;
        if (seq.empty() || !std::holds_alternative<node_return_stmt>(*seq.back())) {
            return false;
        }
        return true;
    }
    return std::holds_alternative<node_return_stmt>(node);
}

// Assumes that the given "compile_obj_ptr" is a function that compiles code to produce
// a pointer to an object of the given type. This function compiles
// code to destruct that object.
using compile_obj_ptr_cb = std::function<void(const token&)>;
auto call_destructor(compiler& com, const type_name& type, compile_obj_ptr_cb compile_obj_ptr) -> void
{
    const auto destructor_name = std::format("{}::drop", type);
    auto func_key = function_key{};
    func_key.name = destructor_name;
    func_key.args = { concrete_ptr_type(type) };
    if (auto it = com.functions.find(func_key); it != com.functions.end()) {
        const auto& [sig, ptr, tok] = it->second;

        // Push the args to the stack
        push_literal(com, std::uint64_t{0}); // base ptr
        push_literal(com, std::uint64_t{0}); // prog ptr
        compile_obj_ptr(tok);

        com.program.emplace_back(op_function_call{
            .name=destructor_name,
            .ptr=ptr + 1, // Jump into the function
            .args_size=com.types.size_of(concrete_ptr_type(type)) + 2 * sizeof(std::uint64_t)
        });
        com.program.emplace_back(op_pop{ .size = com.types.size_of(sig.return_type) });
    }

    // Loop through the fields and call their destructors.
    const auto fields = com.types.fields_of(type);
    for (const auto& field : fields | std::views::reverse) {
        call_destructor(com, field.type, [&](const token& tok) {
            compile_obj_ptr(tok);
            compile_ptr_to_field(com, tok, field.type, field.name);
        });
    }
}

auto call_destructor_named_var(compiler& com, const std::string& var, const type_name& type) -> void
{
    if (var.starts_with('#')) { return; } // Compiler intrinsic vars can be skipped

    return std::visit(overloaded{
        [&](const type_simple&) {
            // Call destructor of the object type.
            call_destructor(com, type, [&](const token& tok) {
                push_var_addr(com, tok, var);
            });
        },
        [&](const type_list& list) {
            const auto inner_type = *list.inner_type;
            const auto inner_size = com.types.size_of(inner_type);
            for (std::size_t index = list.count; index != 0;) {
                --index;
                call_destructor(com, inner_type, [&](const token& tok) {
                    push_var_addr(com, tok, var);
                    push_literal(com, index * inner_size);
                    com.program.emplace_back(op_modify_ptr{});
                });
            }
        },
        [&](const type_ptr&) {}
    }, type);
}

auto destruct_on_end_of_scope(compiler& com) -> void
{
    auto& scope = current_vars(com).current_scope();
    for (const auto& [name, info] : scope.vars | std::views::reverse) {
        call_destructor_named_var(com, name, info.type);
    }
}

auto destruct_on_break_or_continue(compiler& com) -> void
{
    for (const auto& scope : current_vars(com).scopes() | std::views::reverse) {
        for (const auto& [name, info] : scope.vars | std::views::reverse) {
            call_destructor_named_var(com, name, info.type);
        }
        if (scope.type == var_scope::scope_type::while_stmt) {
            return;
        }
    }
}

auto destruct_on_return(compiler& com, const node_return_stmt* node = nullptr) -> void
{
    auto return_variable = std::string{"#"};
    auto skip_return_destructor = true;
    if (node && std::holds_alternative<node_variable_expr>(*node->return_value)) {
        return_variable = std::get<node_variable_expr>(*node->return_value).name;
    }
    for (const auto& scope : current_vars(com).scopes() | std::views::reverse) {
        for (const auto& [name, info] : scope.vars | std::views::reverse) {
            // If the return expr is just a variable name, do not destruct that object.
            // Further, if there is a variable in an outer scope with the same name, make
            // sure to only destruct the inner name.
            if (name != return_variable || !skip_return_destructor) {
                call_destructor_named_var(com, name, info.type);
            }
            else {
                skip_return_destructor = false;
            }
        }
    }
}

auto type_of_expr(const compiler& com, const node_expr& node) -> type_name
{
    return std::visit(overloaded{
        [&](const node_literal_expr& expr) { return expr.value.type; },
        [&](const node_unary_op_expr& expr) {
            const auto desc = unary_op_description{
                .op = expr.token.text,
                .type=type_of_expr(com, *expr.expr)
            };
            const auto r = resolve_unary_op(desc);
            if (!r.has_value()) {
                compiler_error(expr.token, "could not find op '{}{}'", desc.op, desc.type);
            }
            return r->result_type;
        },
        [&](const node_binary_op_expr& expr) {
            const auto desc = binary_op_description{
                .op = expr.token.text,
                .lhs=type_of_expr(com, *expr.lhs),
                .rhs=type_of_expr(com, *expr.rhs)
            };
            const auto r = resolve_binary_op(com.types, desc);
            if (!r.has_value()) {
                compiler_error(
                    expr.token, "could not find op '{} {} {}'",
                    desc.lhs, desc.op, desc.rhs
                );
            }
            return r->result_type;
        },
        [&](const node_function_call_expr& expr) {
            if (com.types.contains(make_type(expr.function_name))) { // constructor
                return make_type(expr.function_name);
            }
            auto key = function_key{};
            key.name = expr.function_name;
            key.args.reserve(expr.args.size());
            for (const auto& arg : expr.args) {
                key.args.push_back(type_of_expr(com, *arg));
            }
            auto it = com.functions.find(key);
            if (it == com.functions.end()) {
                compiler_error(expr.token, "(1) could not find function '{}({})'", key.name, format_comma_separated(key.args));
            }
            return it->second.sig.return_type;
        },
        [&](const node_member_function_call_expr& expr) {
            const auto obj_type = type_of_expr(com, *expr.expr);
            auto key = function_key{};
            key.name = std::format("{}::{}", obj_type, expr.function_name);
            key.args.reserve(expr.args.size() + 1);
            key.args.push_back(concrete_ptr_type(obj_type));
            for (const auto& arg : expr.args) {
                key.args.push_back(type_of_expr(com, *arg));
            }
            const auto it = com.functions.find(key);
            if (it == com.functions.end()) {
                const auto function_str = std::format("{}({})", key.name, format_comma_separated(key.args));
                compiler_error(expr.token, "(2) could not find function '{}'", function_str);
            }
            return it->second.sig.return_type;
        },
        [&](const node_list_expr& expr) {
            return type_name{type_list{
                .inner_type = type_of_expr(com, *expr.elements.front()),
                .count = expr.elements.size()
            }};
        },
        [&](const node_repeat_list_expr& expr) {
            return type_name{type_list{
                .inner_type = type_of_expr(com, *expr.value),
                .count = expr.size
            }};
        },
        [&](const node_addrof_expr& expr) {
            return concrete_ptr_type(type_of_expr(com, *expr.expr));
        },
        [&](const node_sizeof_expr& expr) {
            return u64_type();
        },
        [&](const node_variable_expr& expr) {
            return get_var_type(com, expr.token, expr.name);
        },
        [&](const node_field_expr& expr) {
            const auto type = type_of_expr(com, *expr.expr);
            for (const auto& field : com.types.fields_of(type)) {
                if (field.name == expr.field_name) {
                    return field.type;
                }
            }
            return i64_type();
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
        },
        [&](const node_new_expr& expr) {
            return concrete_ptr_type(expr.type);
        }
    }, node);
}

auto compile_expr_ptr(compiler& com, const node_expr& node) -> type_name;
auto compile_expr_val(compiler& com, const node_expr& expr) -> type_name;
auto compile_stmt(compiler& com, const node_stmt& root) -> void;

auto compile_expr_ptr(compiler& com, const node_variable_expr& node) -> type_name
{
    return push_var_addr(com, node.token, node.name);
}

auto compile_expr_ptr(compiler& com, const node_field_expr& node) -> type_name
{
    const auto type = compile_expr_ptr(com, *node.expr);
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
    compiler_assert_eq(itype, u64_type(), expr.token, "subscript argument wrog type");

    push_literal(com, etype_size);
    const auto info = resolve_binary_op(com.types, { .op="*", .lhs=itype, .rhs=itype });
    com.program.emplace_back(op_builtin_call{
        .name = "uint * uint",
        .ptr = info->operator_func
    });

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
    com.program.emplace_back(op_load_bytes{node.value.data});
    return node.value.type;
}

auto compile_expr_val(compiler& com, const node_binary_op_expr& node) -> type_name
{
    const auto lhs = compile_expr_val(com, *node.lhs);
    const auto rhs = compile_expr_val(com, *node.rhs);
    const auto op = node.token.text;

    const auto info = resolve_binary_op(com.types, { .op=op, .lhs=lhs, .rhs=rhs });
    compiler_assert(info.has_value(), node.token, "could not evaluate '{} {} {}'", lhs, op, rhs);

    com.program.emplace_back(op_builtin_call{
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

    com.program.emplace_back(op_builtin_call{
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
    auto key = function_key{};
    key.name = node.function_name;
    key.args.reserve(node.args.size());
    for (const auto& arg : node.args) {
        key.args.push_back(type_of_expr(com, *arg));
    }
    
    if (auto it = com.functions.find(key); it != com.functions.end()) {
        const auto& [sig, ptr, tok] = it->second;
        push_literal(com, std::uint64_t{0}); // base ptr
        push_literal(com, std::uint64_t{0}); // prog ptr
        
        // Push the args to the stack
        std::vector<type_name> param_types;
        for (const auto& arg : node.args) {
            param_types.emplace_back(compile_expr_val(com, *arg));
        }
        verify_sig(node.token, sig, param_types);
        com.program.emplace_back(op_function_call{
            .name=node.function_name,
            .ptr=ptr + 1, // Jump into the function
            .args_size=signature_args_size(com, sig)
        });
        return sig.return_type;
    }

    // Otherwise, it must be a builtin function.
    // Push the args to the stack
    auto param_types = std::vector<type_name>{};
    auto args_size = std::size_t{0};
    for (const auto& arg : node.args) {
        param_types.emplace_back(compile_expr_val(com, *arg));
        args_size += com.types.size_of(param_types.back());
    }

    if (is_builtin(node.function_name, param_types)) {
        const auto& builtin = fetch_builtin(node.function_name, param_types);

        com.program.emplace_back(op_builtin_call{
            .name=node.function_name,
            .ptr=builtin.ptr,
            .args_size=args_size
        });
        return builtin.return_type;
    }

    const auto function_str = std::format("{}({})", node.function_name, format_comma_separated(key.args));
    compiler_error(node.token, "(3) could not find function '{}'", function_str);
}

auto compile_expr_val(compiler& com, const node_member_function_call_expr& node) -> type_name
{
    const auto obj_type = type_of_expr(com, *node.expr);
    const auto qualified_function_name = std::format("{}::{}", obj_type, node.function_name);

    auto key = function_key{};
    key.name = qualified_function_name;
    key.args.reserve(node.args.size() + 1);
    key.args.push_back(concrete_ptr_type(obj_type));
    for (const auto& arg : node.args) {
        key.args.push_back(type_of_expr(com, *arg));
    }
    
    const auto it = com.functions.find(key);
    if (it == com.functions.end()) {
        compiler_error(node.token, "(4) could not find function '{}'", qualified_function_name);
    }
    
    const auto& [sig, ptr, tok] = it->second;
    push_literal(com, std::uint64_t{0}); // base ptr
    push_literal(com, std::uint64_t{0}); // prog ptr
    
    // Push the args to the stack
    std::vector<type_name> param_types;
    compile_expr_ptr(com, *node.expr);
    param_types.emplace_back(concrete_ptr_type(obj_type));
    for (const auto& arg : node.args) {
        param_types.emplace_back(compile_expr_val(com, *arg));
    }
    verify_sig(node.token, sig, param_types);
    com.program.emplace_back(op_function_call{
        .name=node.function_name,
        .ptr=ptr + 1, // Jump into the function
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
        compiler_assert_eq(element_type, inner_type, node.token, "list has mismatching element types");
    }
    return concrete_list_type(inner_type, node.elements.size());
}

auto compile_expr_val(compiler& com, const node_repeat_list_expr& node) -> type_name
{
    compiler_assert(node.size != 0, node.token, "currently do not support empty list literals");

    const auto inner_type = type_of_expr(com, *node.value);
    for (std::size_t i = 0; i != node.size; ++i) {
        compile_expr_val(com, *node.value);
    }
    return concrete_list_type(inner_type, node.size);
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
    push_literal(com, size);
    return u64_type();
}

auto compile_expr_val(compiler& com, const node_new_expr& node) -> type_name
{
    const auto count = compile_expr_val(com, *node.size);
    compiler_assert_eq(count, u64_type(), node.token, "invalid array size type");
    com.program.emplace_back(op_allocate{ .type_size=com.types.size_of(node.type) });
    return concrete_ptr_type(node.type);
}

// If not implemented explicitly, assume that the given node_expr is an lvalue, in which case
// we can load it by pushing the address to the stack and loading.
auto compile_expr_val(compiler& com, const auto& node) -> type_name
{
    const auto type = compile_expr_ptr(com, node);
    const auto size = com.types.size_of(type);
    com.program.emplace_back(op_load{ .size=size });
    return type;
}

void compile_stmt(compiler& com, const node_sequence_stmt& node)
{
    current_vars(com).push_scope(var_scope::scope_type::seq_stmt);
    for (const auto& seq_node : node.sequence) {
        compile_stmt(com, *seq_node);
    }

    destruct_on_end_of_scope(com);
    const auto scope_size = current_vars(com).pop_scope();
    if (scope_size > 0) {
        com.program.emplace_back(op_pop{scope_size});
    }
}

void compile_stmt(compiler& com, const node_while_stmt& node)
{
    current_vars(com).push_scope(var_scope::scope_type::while_stmt);

    const auto begin_pos = std::ssize(com.program);
    const auto cond_type = compile_expr_val(com, *node.condition);
    compiler_assert_eq(cond_type, bool_type(), node.token, "while-stmt invalid condition");

    const auto jump_pos = append_op(com, op_jump_if_false{});

    com.control_flow.emplace();
    compile_stmt(com, *node.body);
    const auto end_pos = std::ssize(com.program);
    com.program.emplace_back(op_jump{ .jump=(begin_pos - end_pos) });

    std::get<op_jump_if_false>(com.program[jump_pos]).jump = end_pos + 1 - jump_pos;

    const auto& control_flow = com.control_flow.top();
    for (const auto idx : control_flow.break_stmts) {
        std::get<op_jump>(com.program[idx]).jump = end_pos + 1 - idx; // Jump past end
    }
    for (const auto idx : control_flow.continue_stmts) {
        std::get<op_jump>(com.program[idx]).jump = begin_pos - idx; // Jump to start
    }
    com.control_flow.pop();

    const auto scope_size = current_vars(com).pop_scope();
    if (scope_size > 0) {
        com.program.emplace_back(op_pop{scope_size});
    }
}

void compile_stmt(compiler& com, const node_if_stmt& node)
{
    const auto cond_type = compile_expr_val(com, *node.condition);
    compiler_assert_eq(cond_type, bool_type(), node.token, "if-stmt invalid condition");

    const auto jump_pos = append_op(com, op_jump_if_false{});
    compile_stmt(com, *node.body);

    if (node.else_body) {
        const auto else_pos = append_op(com, op_jump{});
        compile_stmt(com, *node.else_body);
        std::get<op_jump_if_false>(com.program[jump_pos]).jump = else_pos + 1 - jump_pos; // Jump into the else block if false
        std::get<op_jump>(com.program[else_pos]).jump = com.program.size() - else_pos; // Jump past the end if false
    } else {
        std::get<op_jump_if_false>(com.program[jump_pos]).jump = com.program.size() - jump_pos; // Jump past the end if false
    }
}

void compile_stmt(compiler& com, const node_struct_stmt& node)
{
    verify_unused_name(com, node.token, node.name);
    for (const auto& field : node.fields) {
        verify_real_type(com, node.token, field.type);
    }

    com.types.add(make_type(node.name), node.fields);
    for (const auto& function : node.functions) {
        compile_stmt(com, *function);
    }
}

void compile_stmt(compiler& com, const node_break_stmt&)
{
    destruct_on_break_or_continue(com);
    const auto pos = append_op(com, op_jump{});
    com.control_flow.top().break_stmts.insert(pos);
}

void compile_stmt(compiler& com, const node_continue_stmt&)
{
    destruct_on_break_or_continue(com);
    const auto pos = append_op(com, op_jump{});
    com.control_flow.top().continue_stmts.insert(pos);
}

void compile_stmt(compiler& com, const node_declaration_stmt& node)
{
    const auto type = compile_expr_val(com, *node.expr);
    if (is_lvalue_expr(*node.expr) && !is_type_fundamental(type)) {
        compiler_error(node.token, "{} cannot be copy-constructed", type);
    } else {
        declare_var(com, node.token, node.name, type);
        save_variable(com, node.token, node.name);
    }
}

void compile_stmt(compiler& com, const node_assignment_stmt& node)
{
    const auto rhs = compile_expr_val(com, *node.expr);
    if (is_lvalue_expr(*node.expr) && !is_type_fundamental(rhs)) {
        compiler_error(node.token, "{} cannot be copy-assigned", rhs);
    } else {
        const auto lhs = compile_expr_ptr(com, *node.position);
        compiler_assert_eq(lhs, rhs, node.token, "invalid assignment");
        com.program.emplace_back(op_save{ .size=com.types.size_of(lhs) });
    }
}

auto make_key(compiler& com, const token& tok, const std::string& name, const signature& sig)
    -> function_key
{
    auto key = function_key{ .name=name };
    for (const auto& arg : sig.params) {
        verify_real_type(com, tok, arg.type);
        key.args.push_back(arg.type);
    }
    verify_real_type(com, tok, sig.return_type);
    return key;
}

void compile_function_body(
    compiler& com,
    const token& tok,
    const std::string& name,
    const signature& sig,
    const node_stmt_ptr& body)
{
    const auto key = make_key(com, tok, name, sig);

    const auto begin_pos = append_op(com, op_function{ .name=key.name });
    com.functions[key] = { .sig=sig, .ptr=begin_pos, .tok=tok };

    com.current_func.emplace(current_function{ .vars={}, .return_type=sig.return_type });
    declare_var(com, tok, "# old_base_ptr", u64_type()); // Store the old base ptr
    declare_var(com, tok, "# old_prog_ptr", u64_type()); // Store the old program ptr
    for (const auto& arg : sig.params) {
        declare_var(com, tok, arg.name, arg.type);
    }
    compile_stmt(com, *body);
    com.current_func.reset();

    if (!function_ends_with_return(*body)) {
        // A function returning null does not need a final return statement, and in this case
        // we manually add a return value of null here.
        if (sig.return_type == null_type()) {
            destruct_on_return(com);
            com.program.emplace_back(op_load_bytes{{std::byte{0}}});
            com.program.emplace_back(op_return{ .size=1 });
        } else {
            compiler_error(tok, "function '{}' does not end in a return statement", key.name);
        }
    }

    std::get<op_function>(com.program[begin_pos]).jump = com.program.size();
}

void compile_stmt(compiler& com, const node_function_def_stmt& node)
{
    if (com.types.contains(make_type(node.name))) {
        compiler_error(node.token, "'{}' cannot be a function name, it is a type def", node.name);
    }
    com.function_names.insert(node.name);
    compile_function_body(com, node.token, node.name, node.sig, node.body);
}

void compile_stmt(compiler& com, const node_member_function_def_stmt& node)
{
    compiler_assert(node.sig.params.size() >= 1, node.token, "member functions must have at least one arg");

    const auto expected = concrete_ptr_type(make_type(node.struct_name));
    const auto actual = node.sig.params.front().type;
    compiler_assert_eq(actual, expected, node.token, "'{}' bad 1st arg", node.function_name);

    // Special function extra checks
    if (node.function_name == "drop") {
        compiler_assert_eq(node.sig.return_type, null_type(), node.token, "'drop' bad return type");
        compiler_assert_eq(node.sig.params.size(), 1, node.token, "'drop' bad number of args");
    }
    else if (node.function_name == "copy") {
        compiler_assert_eq(node.sig.return_type, null_type(), node.token, "'copy' bad return type");
        compiler_assert_eq(node.sig.params.size(), 2, node.token, "'copy' bad number of args");
        compiler_assert_eq(node.sig.params.back().type, expected, node.token, "'copy' bad 2nd arg");
    }

    const auto name = std::format("{}::{}", node.struct_name, node.function_name);
    compile_function_body(com, node.token, name, node.sig, node.body);
}

void compile_stmt(compiler& com, const node_return_stmt& node)
{
    compiler_assert(com.current_func.has_value(), node.token, "can only return within functions");
    destruct_on_return(com, &node);
    const auto return_type = compile_expr_val(com, *node.return_value);
    compiler_assert_eq(return_type, com.current_func->return_type, node.token, "wrong return type");
    com.program.emplace_back(op_return{ .size=com.types.size_of(return_type) });
}

void compile_stmt(compiler& com, const node_expression_stmt& node)
{
    const auto type = compile_expr_val(com, *node.expr);
    com.program.emplace_back(op_pop{ .size=com.types.size_of(type) });
}

void compile_stmt(compiler& com, const node_delete_stmt& node)
{
    const auto type = compile_expr_val(com, *node.expr);
    compiler_assert(is_ptr_type(type), node.token, "delete requires a ptr, got {}\n", type);
    com.program.emplace_back(op_deallocate{});
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

auto compile(const node_stmt_ptr& root) -> program
{
    auto com = compiler{};
    compile_stmt(com, *root);
    return com.program;
}

}