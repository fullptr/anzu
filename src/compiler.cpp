#include "compiler.hpp"
#include "lexer.hpp"
#include "object.hpp"
#include "parser.hpp"
#include "functions.hpp"
#include "resolver.hpp"
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
        block,
        loop,
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

    auto top() const -> std::size_t
    {
        return d_next;
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

    auto push_scope(var_scope::scope_type type = var_scope::scope_type::block) -> void
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

    type_store types; // TODO: store a flag in here to say if a type is default/deleted/implemented copyable/assignable
};

auto push_expr_ptr(compiler& com, const node_expr& node) -> type_name;
auto push_expr_val(compiler& com, const node_expr& expr) -> type_name;
auto compile_stmt(compiler& com, const node_stmt& root) -> void;
auto current_vars(compiler& com) -> var_locations&;
auto type_of_expr(const compiler& com, const node_expr& node) -> type_name;
auto call_destructor_named_var(compiler& com, const std::string& var, const type_name& type) -> void;

auto resolve_type(const compiler& com, const node_type& type) -> type_name
{
    return std::visit(overloaded {
        [&](const node_named_type& node) {
            return node.type;
        },
        [&](const node_expr_type& node) {
            return type_of_expr(com, *node.expr);
        }
    }, type);
}

auto resolve_sig(const compiler& com, const node_signature& sig) -> signature
{
    auto new_sig = signature{};
    new_sig.return_type = resolve_type(com, *sig.return_type);
    for (const auto& p : sig.params) {
        new_sig.params.emplace_back(
            signature::parameter{ .name=p.name, .type=resolve_type(com, *p.type) }
        );
    }
    return new_sig;
}

auto resolve_type_fields(const compiler& com, const node_type_fields& fields) -> type_fields
{
    auto new_fields = type_fields{};
    for (const auto& f : fields) {
        new_fields.emplace_back(field{ .name=f.name, .type=resolve_type(com, *f.type) });
    }
    return new_fields;
}

class scope_guard
{
    compiler* d_com;
    var_scope::scope_type d_type;

public:
    scope_guard(compiler& com, var_scope::scope_type type = var_scope::scope_type::block)
        : d_com(&com)
        , d_type(type)
    {
        current_vars(com).push_scope(type);
        if (type == var_scope::scope_type::loop) {
            com.control_flow.emplace();
        }
    }

    ~scope_guard()
    {
        if (d_type == var_scope::scope_type::loop) {
            d_com->control_flow.pop();
        }

        // destruct all variables in the current scope
        auto& scope = current_vars(*d_com).current_scope();
        for (const auto& [name, info] : scope.vars | std::views::reverse) {
            call_destructor_named_var(*d_com, name, info.type);
        }

        // deallocate all space in used by the space
        const auto scope_size = current_vars(*d_com).pop_scope();
        if (scope_size > 0) {
            d_com->program.emplace_back(op_pop{scope_size});
        }
    }

    scope_guard(const scope_guard&) = delete;
    scope_guard& operator=(const scope_guard&) = delete;
};

void verify_unused_name(compiler& com, const token& tok, const std::string& name)
{
    const auto message = std::format("type '{}' already defined", name);
    tok.assert(!com.types.contains(make_type(name)), message);
    tok.assert(!com.function_names.contains(name), message);
}

template <typename T>
auto push_literal(compiler& com, const T& value) -> void
{
    const auto bytes = as_bytes(value);
    com.program.emplace_back(op_load_bytes{{bytes.begin(), bytes.end()}});
}

auto push_ptr_adjust(compiler& com, std::size_t offset) -> void
{
    push_literal(com, offset);
    com.program.emplace_back(op_u64_add{}); // modify ptr
}

auto current_vars(compiler& com) -> var_locations&
{
    return com.current_func ? com.current_func->vars : com.globals;
}

auto verify_real_type(const compiler& com, const token& tok, const type_name& t) -> void
{
    tok.assert(com.types.contains(t), "{} is not a recognised type", t);
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

struct function_info
{
    std::string name;
    std::size_t ptr;
    signature   sig;
    token       tok;
};

auto get_function(const compiler& com, const function_key& key)
    -> std::optional<function_info>
{
    if (const auto it = com.functions.find(key); it != com.functions.end()) {
        const auto& [name, args] = it->first;
        const auto& [sig, ptr, tok] = it->second;
        return function_info{name, ptr, sig, tok};
    }
    return std::nullopt;
}

auto push_function_call_begin(compiler& com) -> void
{
    push_literal(com, std::uint64_t{0}); // base ptr
    push_literal(com, std::uint64_t{0}); // prog ptr
}

auto push_function_call(
    compiler& com,
    const std::string& name,
    std::size_t ptr,
    const signature& sig
)
    -> void
{
    com.program.emplace_back(op_function_call{
        .name=name,
        .ptr=ptr + 1, // Jump into the function
        .args_size=signature_args_size(com, sig)
    });
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
        tok.error("name already in use: '{}'", name);
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

    tok.error("could not find variable '{}'\n", name);
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

    tok.error("could not find variable '{}'\n", name);
}

auto load_variable(compiler& com, const token& tok, const std::string& name) -> void
{
    const auto type = push_var_addr(com, tok, name);
    const auto size = com.types.size_of(type);
    com.program.emplace_back(op_load{ .size=size });
}

auto save_variable(compiler& com, const token& tok, const std::string& name) -> void
{
    const auto type = push_var_addr(com, tok, name);
    const auto size = com.types.size_of(type);
    com.program.emplace_back(op_save{ .size=size });
}

// Given a type and a field name, push the offset of the fields position relative to its
// owner onto the stack
auto push_field_offset(
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
    
    tok.error("could not find field '{}' for type '{}'\n", field_name, type);
}

// Given a type and field name, and assuming that the top of the stack at runtime is a pointer
// to an object of the given type, this function adds op codes to modify that pointer to
// instead point to the given field. Returns the type of the field.
auto push_adjust_ptr_to_field(
    compiler& com, const token& tok, const type_name& type, const std::string& field_name
)
    -> type_name
{
    const auto field_type = push_field_offset(com, tok, type, field_name);
    com.program.emplace_back(op_u64_add{}); // modify ptr
    return field_type;
}

void verify_sig(const token& tok, const signature& sig, const std::vector<type_name>& args)
{
    if (sig.params.size() != args.size()) {
        tok.error("function expected {} args, got {}", sig.params.size(), args.size());
    }

    for (const auto& [actual, expected] : zip(args, sig.params)) {
        if (actual != expected.type) {
            tok.error("'{}' does not match '{}'", actual, expected.type);
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

auto assign_fn(const type_name& type) -> function_key
{
    return {
        .name = std::format("{}::assign", type),
        .args = { concrete_ptr_type(type), concrete_ptr_type(type) }
    };
}

auto copy_fn(const type_name& type) -> function_key
{
    return {
        .name = std::format("{}::copy", type),
        .args = { concrete_ptr_type(type) }
    };
}

auto drop_fn(const type_name& type) -> function_key
{
    return {
        .name = std::format("{}::drop", type),
        .args = { concrete_ptr_type(type) }
    };
}

// Assumes that the given "push_object_ptr" is a function that compiles code to produce
// a pointer to an object of the given type. This function compiles
// code to destruct that object.
using compile_obj_ptr_cb = std::function<void(const token&)>;
auto call_destructor(compiler& com, const type_name& type, compile_obj_ptr_cb push_object_ptr) -> void
{
    if (is_list_type(type)) {
        const auto etype = inner_type(type);
        const auto inner_size = com.types.size_of(etype);

        if (const auto drop = get_function(com, drop_fn(etype))) {
            for (std::size_t i = array_length(type); i != 0;) {
                --i;
                push_function_call_begin(com);
                push_object_ptr(drop->tok);
                push_ptr_adjust(com, i * inner_size);
                push_function_call(com, drop->name, drop->ptr, drop->sig);
            }
        }
        return;
    }

    if (const auto func = get_function(com, drop_fn(type)); func) {
        // Push the args to the stack
        push_function_call_begin(com);
        push_object_ptr(func->tok);
        push_function_call(com, func->name, func->ptr, func->sig);
        com.program.emplace_back(op_pop{ .size = com.types.size_of(func->sig.return_type) });
    }

    // Loop through the fields and call their destructors.
    const auto fields = com.types.fields_of(type);
    for (const auto& field : fields | std::views::reverse) {
        call_destructor(com, field.type, [&](const token& tok) {
            push_object_ptr(tok);
            push_adjust_ptr_to_field(com, tok, field.type, field.name);
        });
    }
}

auto call_destructor_named_var(compiler& com, const std::string& var, const type_name& type) -> void
{
    if (var.starts_with('#')) { return; } // Compiler intrinsic vars can be skipped
    call_destructor(com, type, [&](const token& tok) {
        push_var_addr(com, tok, var);
    });
}

auto destruct_on_break_or_continue(compiler& com) -> void
{
    for (const auto& scope : current_vars(com).scopes() | std::views::reverse) {
        for (const auto& [name, info] : scope.vars | std::views::reverse) {
            call_destructor_named_var(com, name, info.type);
            com.program.emplace_back(op_debug{std::format("destructing {}", name)});
        }
        if (scope.type == var_scope::scope_type::loop) {
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

// Assumes that the top of the stack is an object of the given type. This
// function calls the destructor and pops the data. It MUST NOT already be a defined
// variable
auto pop_object(compiler& com, const type_name& type, const token& tok) -> void
{
    const auto object_ptr = current_vars(com).top();
    call_destructor(com, type, [&](const token&) {
        // Because the variable has not been delcared, the stack pointer has not
        // incremented, so it is currently pointing to our temp value.
        com.program.emplace_back(op_push_local_addr{ .offset = object_ptr});
    });
    com.program.emplace_back(op_pop{ .size = com.types.size_of(type) });
}

// Given an expression, evaluate it and push to the top of the stack. If the expression
// is an lvalue, copy constructors are invoked.
auto push_object_copy(compiler& com, const node_expr& expr, const token& tok) -> type_name
{
    const auto type = type_of_expr(com, expr);

    if (is_rvalue_expr(expr) || is_type_trivially_copyable(type)) {
        push_expr_val(com, expr);
    }
    
    else if (is_list_type(type)) {
        const auto etype = inner_type(type);
        const auto inner_size = com.types.size_of(etype);

        const auto copy = get_function(com, copy_fn(etype));
        tok.assert(copy.has_value(), "{} cannot be copied", etype);

        for (std::size_t i = 0; i != array_length(type); ++i) {
            push_function_call_begin(com);
            push_expr_ptr(com, expr);
            push_ptr_adjust(com, i * inner_size);
            push_function_call(com, copy->name, copy->ptr, copy->sig);
        }
    }

    else {
        const auto copy = get_function(com, copy_fn(type));
        tok.assert(copy.has_value(), "{} cannot be copied", type);

        push_function_call_begin(com);
        push_expr_ptr(com, expr);
        push_function_call(com, copy->name, copy->ptr, copy->sig);
    }

    return type;
}

auto type_of_expr(const compiler& com, const node_expr& node) -> type_name
{
    return std::visit(overloaded{
        [&](const node_literal_expr& expr) { return expr.value.type; },
        [&](const node_unary_op_expr& expr) {
            const auto type = type_of_expr(com, *expr.expr);
            const auto op = expr.token.text;

            const auto info = resolve_operation(type, op);
            expr.token.assert(info.has_value(), "could not find op '{}{}'", op, type);
            return info->return_type;
        },
        [&](const node_binary_op_expr& expr) {
            const auto lhs = type_of_expr(com, *expr.lhs);
            const auto rhs = type_of_expr(com, *expr.rhs);
            const auto op = expr.token.text;

            // Pointer arithmetic
            if (is_ptr_type(lhs) && rhs == u64_type() && op == tk_add) {
                return lhs;
            }

            const auto info = resolve_operation(lhs, rhs, op);
            expr.token.assert(info.has_value(), "could not find op '{} {} {}'", lhs, op, rhs);
            return info->return_type;
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
                expr.token.error("(1) could not find function '{}({})'", key.name, format_comma_separated(key.args));
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
                expr.token.error("(2) could not find function '{}'", function_str);
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
            expr.token.assert(is_ptr_type(ptype), "cannot use deref operator on non-ptr type '{}'", ptype);
            return inner_type(ptype);
        },
        [&](const node_subscript_expr& expr) {
            const auto ltype = type_of_expr(com, *expr.expr);
            if (!std::holds_alternative<type_list>(ltype)) {
                expr.token.error("cannot use subscript operator on non-list type '{}'", ltype);
            }
            return *std::get<type_list>(ltype).inner_type;
        },
        [&](const node_new_expr& expr) {
            return concrete_ptr_type(resolve_type(com, *expr.type));
        }
    }, node);
}

auto push_expr_ptr(compiler& com, const node_variable_expr& node) -> type_name
{
    return push_var_addr(com, node.token, node.name);
}

auto push_expr_ptr(compiler& com, const node_field_expr& node) -> type_name
{
    const auto type = push_expr_ptr(com, *node.expr);
    return push_adjust_ptr_to_field(com, node.token, type, node.field_name);
}

auto push_expr_ptr(compiler& com, const node_deref_expr& node) -> type_name
{
    const auto type = push_expr_val(com, *node.expr); // Push the address
    node.token.assert(is_ptr_type(type), "cannot use deref operator on non-ptr type '{}'", type);
    return inner_type(type);
}

auto push_expr_ptr(compiler& com, const node_subscript_expr& expr) -> type_name
{
    const auto ltype = push_expr_ptr(com, *expr.expr);
    expr.token.assert(is_list_type(ltype), "cannot use subscript operator on non-list type '{}'", ltype);
    const auto etype = inner_type(ltype);
    const auto etype_size = com.types.size_of(etype);

    // Push the offset (index * size) and add to the ptr
    const auto itype = push_expr_val(com, *expr.index);
    expr.token.assert_eq(itype, u64_type(), "subscript argument wrong type");
    push_literal(com, etype_size);
    com.program.emplace_back(op_u64_mul{});
    com.program.emplace_back(op_u64_add{}); // modify ptr
    return etype;
}

[[noreturn]] auto push_expr_ptr(compiler& com, const auto& node) -> type_name
{
    node.token.error("cannot take address of a non-lvalue\n");
}

auto push_expr_ptr(compiler& com, const node_expr& node) -> type_name
{
    return std::visit([&](const auto& expr) { return push_expr_ptr(com, expr); }, node);
}

auto push_expr_val(compiler& com, const node_literal_expr& node) -> type_name
{
    com.program.emplace_back(op_load_bytes{node.value.data});
    return node.value.type;
}

auto push_expr_val(compiler& com, const node_binary_op_expr& node) -> type_name
{
    const auto lhs = push_expr_val(com, *node.lhs);
    const auto rhs = push_expr_val(com, *node.rhs);
    const auto op = node.token.text;

    // Pointer arithmetic, multiply the offset by the size of the type before adding
    if (is_ptr_type(lhs) && rhs == u64_type() && op == tk_add) {
        push_literal(com, com.types.size_of(inner_type(lhs)));
        com.program.emplace_back(op_u64_mul{});
        com.program.emplace_back(op_u64_add{});
        return lhs;
    }
    
    const auto info = resolve_operation(lhs, rhs, op);
    node.token.assert(info.has_value(), "could not find op '{} {} {}'", lhs, op, rhs);
    com.program.emplace_back(info->op_code);
    return info->return_type;
}

auto push_expr_val(compiler& com, const node_unary_op_expr& node) -> type_name
{
    const auto type = push_expr_val(com, *node.expr);
    const auto op = node.token.text;

    const auto info = resolve_operation(type, op);
    node.token.assert(info.has_value(), "could not find op '{}{}'", op, type);
    com.program.emplace_back(info->op_code);
    return info->return_type;
} 

auto push_expr_val(compiler& com, const node_function_call_expr& node) -> type_name
{
    // If this is the name of a simple type, then this is a constructor call, so
    // there is currently nothing to do since the arguments are already pushed to
    // the stack.
    if (const auto type = make_type(node.function_name); com.types.contains(type)) {
        const auto sig = make_constructor_sig(com, type);
        std::vector<type_name> param_types;
        for (const auto& arg : node.args) {
            param_types.emplace_back(push_object_copy(com, *arg, node.token));
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
    
    if (const auto func = get_function(com, key); func.has_value()) {
        push_function_call_begin(com);
        for (const auto& arg : node.args) {
            push_object_copy(com, *arg, node.token);
        }
        push_function_call(com, func->name, func->ptr, func->sig);
        return func->sig.return_type;
    }

    // Otherwise, it must be a builtin function.
    // Push the args to the stack
    auto param_types = std::vector<type_name>{};
    auto args_size = std::size_t{0};
    for (const auto& arg : node.args) {
        param_types.emplace_back(push_object_copy(com, *arg, node.token));
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
    node.token.error("(3) could not find function '{}'", function_str);
}

auto push_expr_val(compiler& com, const node_member_function_call_expr& node) -> type_name
{
    const auto obj_type = type_of_expr(com, *node.expr);
    const auto qualified_function_name = std::format("{}::{}", obj_type, node.function_name);

    auto key = function_key{};
    key.name = qualified_function_name;
    key.args.push_back(concrete_ptr_type(obj_type));
    for (const auto& arg : node.args) {
        key.args.push_back(type_of_expr(com, *arg));
    }
    
    const auto func = get_function(com, key);
    node.token.assert(func.has_value(), "(4) could not find function '{}'", qualified_function_name);
    push_function_call_begin(com);
    push_expr_ptr(com, *node.expr);
    for (const auto& arg : node.args) {
        push_object_copy(com, *arg, node.token);
    }
    push_function_call(com, func->name, func->ptr, func->sig);
    return func->sig.return_type;
}

auto push_expr_val(compiler& com, const node_list_expr& node) -> type_name
{
    node.token.assert(!node.elements.empty(), "currently do not support empty list literals");

    const auto inner_type = push_expr_val(com, *node.elements.front());
    for (const auto& element : node.elements | std::views::drop(1)) {
        const auto element_type = push_expr_val(com, *element);
        node.token.assert_eq(element_type, inner_type, "list has mismatching element types");
    }
    return concrete_list_type(inner_type, node.elements.size());
}

auto push_expr_val(compiler& com, const node_repeat_list_expr& node) -> type_name
{
    node.token.assert(node.size != 0, "currently do not support empty list literals");

    const auto inner_type = type_of_expr(com, *node.value);
    for (std::size_t i = 0; i != node.size; ++i) {
        push_expr_val(com, *node.value);
    }
    return concrete_list_type(inner_type, node.size);
}

auto push_expr_val(compiler& com, const node_addrof_expr& node) -> type_name
{
    const auto type = push_expr_ptr(com, *node.expr);
    return concrete_ptr_type(type);
}

auto push_expr_val(compiler& com, const node_sizeof_expr& node) -> type_name
{
    const auto type = type_of_expr(com, *node.expr);
    const auto size = com.types.size_of(type);
    push_literal(com, size);
    return u64_type();
}

auto push_expr_val(compiler& com, const node_new_expr& node) -> type_name
{
    const auto count = push_expr_val(com, *node.size);
    node.token.assert_eq(count, u64_type(), "invalid array size type");
    const auto type = resolve_type(com, *node.type);
    com.program.emplace_back(op_allocate{ .type_size=com.types.size_of(type) });
    return concrete_ptr_type(type);
}

// If not implemented explicitly, assume that the given node_expr is an lvalue, in which case
// we can load it by pushing the address to the stack and loading.
auto push_expr_val(compiler& com, const auto& node) -> type_name
{
    const auto type = push_expr_ptr(com, node);
    const auto size = com.types.size_of(type);
    com.program.emplace_back(op_load{ .size=size });
    return type;
}

void compile_stmt(compiler& com, const node_sequence_stmt& node)
{
    const auto scope = scope_guard{com};
    for (const auto& seq_node : node.sequence) {
        compile_stmt(com, *seq_node);
    }
}

auto compile_loop(compiler& com, std::function<void()> body) -> void
{
    const auto scope = scope_guard{com, var_scope::scope_type::loop};
    
    const auto begin_pos = com.program.size();
    {
        const auto body_scope = scope_guard{com};
        body();
    }
    com.program.emplace_back(op_jump{ .jump=begin_pos });

    // Fix up the breaks and continues
    const auto& control_flow = com.control_flow.top();
    for (const auto idx : control_flow.break_stmts) {
        std::get<op_jump>(com.program[idx]).jump = com.program.size(); // Jump past end
    }
    for (const auto idx : control_flow.continue_stmts) {
        std::get<op_jump>(com.program[idx]).jump = begin_pos; // Jump to start
    }
}

void compile_stmt(compiler& com, const node_loop_stmt& node)
{
    compile_loop(com, [&] {
        compile_stmt(com, *node.body);
    });
}

void push_break(compiler& com);

/*
while <condition> {
    <body>
}

becomes

loop {
    if !<condition> break;
    <body>
}
*/
void compile_stmt(compiler& com, const node_while_stmt& node)
{
    compile_loop(com, [&] {
        // if !<condition> break;
        const auto cond_type = push_expr_val(com, *node.condition);
        node.token.assert_eq(cond_type, bool_type(), "while-stmt invalid condition");
        com.program.emplace_back(op_bool_not{});
        const auto jump_pos = append_op(com, op_jump_if_false{});
        push_break(com);
        std::get<op_jump_if_false>(com.program[jump_pos]).jump = com.program.size(); // Jump past the end if false
        
        // <body>
        compile_stmt(com, *node.body);
    });
}

/*
for <name> in <iter> {
    <body>
}

becomes

{
    <<create temporary var if iter is an rvalue>>
    idx = 0u;
    size := <<length of iter>>;
    loop {
        if idx == size break;
        name := &iter[idx];
        idx = idx + 1u;
        <body>
    }
}
*/
void compile_stmt(compiler& com, const node_for_stmt& node)
{
    const auto scope = scope_guard{com};

    const auto iter_type = type_of_expr(com, *node.iter);
    node.token.assert(is_list_type(iter_type), "for-loops only supported for arrays");

    // Need to create a temporary if we're using an rvalue
    if (is_rvalue_expr(*node.iter)) {
        push_expr_val(com, *node.iter);
        declare_var(com, node.token, "#:iter", iter_type);
    }

    // idx := 0u;
    push_literal(com, std::uint64_t{0});
    declare_var(com, node.token, "#:idx", u64_type());

    // size := length of iter;
    push_literal(com, array_length(iter_type));
    declare_var(com, node.token, "#:size", u64_type());

    compile_loop(com, [&] {
        // if idx == size break;
        load_variable(com, node.token, "#:idx");
        load_variable(com, node.token, "#:size");
        com.program.emplace_back(op_u64_eq{});
        const auto jump_pos = append_op(com, op_jump_if_false{});
        push_break(com);
        std::get<op_jump_if_false>(com.program[jump_pos]).jump = com.program.size(); // Jump past the end if false

        // name := &iter[idx];
        if (is_rvalue_expr(*node.iter)) {
            push_var_addr(com, node.token, "#:iter");
        } else {
            push_expr_ptr(com, *node.iter);
        }
        load_variable(com, node.token, "#:idx");
        push_literal(com, com.types.size_of(inner_type(iter_type)));
        com.program.emplace_back(op_u64_mul{});
        com.program.emplace_back(op_u64_add{});
        declare_var(com, node.token, node.name, concrete_ptr_type(inner_type(iter_type)));

        // idx = idx + 1;
        load_variable(com, node.token, "#:idx");
        push_literal(com, std::uint64_t{1});
        com.program.emplace_back(op_u64_add{});
        save_variable(com, node.token, "#:idx");

        // main body
        compile_stmt(com, *node.body);
    });
}

void compile_stmt(compiler& com, const node_if_stmt& node)
{
    const auto cond_type = push_expr_val(com, *node.condition);
    node.token.assert_eq(cond_type, bool_type(), "if-stmt invalid condition");

    const auto jump_pos = append_op(com, op_jump_if_false{});
    compile_stmt(com, *node.body);

    if (node.else_body) {
        const auto else_pos = append_op(com, op_jump{});
        compile_stmt(com, *node.else_body);
        std::get<op_jump_if_false>(com.program[jump_pos]).jump = else_pos + 1; // Jump into the else block if false
        std::get<op_jump>(com.program[else_pos]).jump = com.program.size(); // Jump past the end if false
    } else {
        std::get<op_jump_if_false>(com.program[jump_pos]).jump = com.program.size(); // Jump past the end if false
    }
}

void compile_stmt(compiler& com, const node_struct_stmt& node)
{
    verify_unused_name(com, node.token, node.name);
    const auto fields = resolve_type_fields(com, node.fields);
    for (const auto& field : fields) {
        verify_real_type(com, node.token, field.type);
    }

    com.types.add(make_type(node.name), fields);
    for (const auto& function : node.functions) {
        compile_stmt(com, *function);
    }
}

void push_break(compiler& com)
{
    destruct_on_break_or_continue(com);
    const auto pos = append_op(com, op_jump{});
    com.control_flow.top().break_stmts.insert(pos);
}

void compile_stmt(compiler& com, const node_break_stmt&)
{
    push_break(com);
}

void compile_stmt(compiler& com, const node_continue_stmt&)
{
    destruct_on_break_or_continue(com);
    const auto pos = append_op(com, op_jump{});
    com.control_flow.top().continue_stmts.insert(pos);
}

auto compile_stmt(compiler& com, const node_declaration_stmt& node) -> void
{
    const auto type = push_object_copy(com, *node.expr, node.token);
    declare_var(com, node.token, node.name, type);
}

void compile_stmt(compiler& com, const node_assignment_stmt& node)
{
    const auto rhs = type_of_expr(com, *node.expr);
    const auto lhs = type_of_expr(com, *node.position);
    node.token.assert_eq(lhs, rhs, "invalid assignment");

    if (is_rvalue_expr(*node.expr) || is_type_trivially_copyable(rhs)) {
        const auto rhs = push_expr_val(com, *node.expr);
        const auto lhs = push_expr_ptr(com, *node.position);
        node.token.assert_eq(lhs, rhs, "invalid assignment");
        com.program.emplace_back(op_save{ .size=com.types.size_of(lhs) });
        return;
    }
    
    if (is_list_type(rhs)) {
        const auto etype = inner_type(rhs);
        const auto inner_size = com.types.size_of(etype);

        const auto assign = get_function(com, assign_fn(etype));
        node.token.assert(assign.has_value(), "{} cannot be assigned", etype);

        for (std::size_t i = 0; i != array_length(rhs); ++i) {
            push_function_call_begin(com);

            push_expr_ptr(com, *node.position); // i-th element of dst
            push_ptr_adjust(com, i * inner_size);
            push_expr_ptr(com, *node.expr); // i-th element of src
            push_ptr_adjust(com, i * inner_size);

            push_function_call(com, assign->name, assign->ptr, assign->sig);
            pop_object(com, assign->sig.return_type, node.token);
        }

        return;
    }

    const auto assign = get_function(com, assign_fn(rhs));
    node.token.assert(assign.has_value(), "{} cannot be assigned", rhs);

    push_function_call_begin(com);
    push_expr_ptr(com, *node.position);
    push_expr_ptr(com, *node.expr);
    push_function_call(com, assign->name, assign->ptr, assign->sig);
    pop_object(com, assign->sig.return_type, node.token);
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

auto compile_function_body(
    compiler& com,
    const token& tok,
    const std::string& name,
    const node_signature& node_sig,
    const node_stmt_ptr& body
)
    -> signature
{
    const auto sig = resolve_sig(com, node_sig);
    const auto key = make_key(com, tok, name, sig);

    const auto begin_pos = append_op(com, op_jump{});
    com.functions[key] = { .sig=sig, .ptr=begin_pos, .tok=tok };

    com.current_func.emplace(current_function{ .vars={}, .return_type=sig.return_type });

    {
        const auto scope = scope_guard{com}; // Ensures destructors for params called

        declare_var(com, tok, "# old_base_ptr", u64_type()); // Store the old base ptr
        declare_var(com, tok, "# old_prog_ptr", u64_type()); // Store the old program ptr
        for (const auto& arg : sig.params) {
            declare_var(com, tok, arg.name, arg.type);
        }
        compile_stmt(com, *body);

        if (!function_ends_with_return(*body)) {
            // A function returning null does not need a final return statement, and in this case
            // we manually add a return value of null here.
            if (sig.return_type == null_type()) {
                destruct_on_return(com);
                com.program.emplace_back(op_load_bytes{{std::byte{0}}});
                com.program.emplace_back(op_return{ .size=1 });
            } else {
                tok.error("function '{}' does not end in a return statement", key.name);
            }
        }
    }

    com.current_func.reset();

    std::get<op_jump>(com.program[begin_pos]).jump = com.program.size();
    return sig;
}

void compile_stmt(compiler& com, const node_function_def_stmt& node)
{
    if (com.types.contains(make_type(node.name))) {
        node.token.error("'{}' cannot be a function name, it is a type def", node.name);
    }
    com.function_names.insert(node.name);
    compile_function_body(com, node.token, node.name, node.sig, node.body);
}

void compile_stmt(compiler& com, const node_member_function_def_stmt& node)
{
    const auto expected = concrete_ptr_type(make_type(node.struct_name));
    const auto name = std::format("{}::{}", node.struct_name, node.function_name);
    const auto struct_type = make_type(node.struct_name);
    const auto sig = compile_function_body(com, node.token, name, node.sig, node.body);

    node.token.assert(sig.params.size() >= 1, "member functions must have at least one arg");

    const auto actual = sig.params.front().type;
    node.token.assert_eq(actual, expected, "'{}' bad 1st arg", node.function_name);

    // Special function extra checks
    if (node.function_name == "drop") {
        node.token.assert_eq(sig.return_type, null_type(), "'drop' bad return type");
        node.token.assert_eq(sig.params.size(), 1, "'drop' bad number of args");
    }
    else if (node.function_name == "copy") {
        node.token.assert_eq(sig.return_type, make_type(node.struct_name), "'copy' bad return type");
        node.token.assert_eq(sig.params.size(), 1, "'copy' bad number of args");
    }
    else if (node.function_name == "assign") {
        node.token.assert_eq(sig.return_type, null_type(), "'assign' bad return type");
        node.token.assert_eq(sig.params.size(), 2, "'assign' bad number of args");
        node.token.assert_eq(sig.params.back().type, expected, "'assign' bad 2nd arg");
    }

}

void compile_stmt(compiler& com, const node_return_stmt& node)
{
    node.token.assert(com.current_func.has_value(), "can only return within functions");
    destruct_on_return(com, &node);
    const auto return_type = push_expr_val(com, *node.return_value);
    node.token.assert_eq(return_type, com.current_func->return_type, "wrong return type");
    com.program.emplace_back(op_return{ .size=com.types.size_of(return_type) });
}

void compile_stmt(compiler& com, const node_expression_stmt& node)
{
    const auto type = push_expr_val(com, *node.expr);
    pop_object(com, type, node.token);
}

void compile_stmt(compiler& com, const node_delete_stmt& node)
{
    const auto type = push_expr_val(com, *node.expr);
    node.token.assert(is_ptr_type(type), "delete requires a ptr, got {}\n", type);
    com.program.emplace_back(op_deallocate{});
}

auto push_expr_val(compiler& com, const node_expr& expr) -> type_name
{
    return std::visit([&](const auto& node) { return push_expr_val(com, node); }, expr);
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