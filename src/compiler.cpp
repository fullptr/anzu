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
#include <set>
#include <unordered_map>
#include <unordered_set>

namespace anzu {
namespace {

static const auto global_namespace = make_type("<global>");

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

struct function_info
{
    type_name   return_type;
    std::size_t ptr;
    token       tok;
};

auto hash(const type_names& params) -> std::size_t
{
    auto hash_value = size_t{0};
    for (const auto& param : params) {
        hash_value ^= hash(param);
    }
    return hash_value;
}

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

using function_hash = decltype([](const type_names& f) { return hash(f); });
using type_name_hash = decltype([](const type_name& f) { return hash(f); });
using function_param_map = std::unordered_map<type_names, function_info, function_hash>;
using function_map = std::unordered_map<std::string, function_param_map>;
using function_iter = typename function_param_map::const_iterator;

// Struct used to store information while compiling an AST. Contains the output program
// as well as information such as function definitions.
struct compiler
{
    std::vector<op> program;
    std::string     read_only_data;

    bool debug = false;

    // namespace (type_name) -> function_name -> signatures -> function_info
    std::unordered_map<type_name, function_map, type_name_hash> functions;

    var_locations globals;
    std::optional<current_function> current_func;

    std::stack<control_flow_frame> control_flow;

    type_store types; // TODO: store a flag in here to say if a type is default/deleted/implemented copyable/assignable
};

auto push_expr_ptr(compiler& com, const node_expr& node) -> type_name;
auto push_expr_val(compiler& com, const node_expr& expr) -> type_name;
auto push_stmt(compiler& com, const node_stmt& root) -> void;
auto current_vars(compiler& com) -> var_locations&;
auto type_of_expr(compiler& com, const node_expr& node) -> type_name;
auto call_destructor_named_var(compiler& com, const std::string& var, const type_name& type) -> void;

auto resolve_type(compiler& com, const token& tok, const node_type_ptr& type) -> type_name
{
    if (!type) {
        return global_namespace;
    }

    const auto resolved_type = std::visit(overloaded {
        [&](const node_named_type& node) {
            return node.type;
        },
        [&](const node_expr_type& node) {
            return type_of_expr(com, *node.expr);
        }
    }, *type);

    tok.assert(com.types.contains(resolved_type), "{} is not a recognised type", resolved_type);
    return resolved_type;
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

auto push_ptr_adjust(compiler& com, std::size_t offset) -> void
{
    com.program.emplace_back(op_push_literal_u64{offset});
    com.program.emplace_back(op_u64_add{}); // modify ptr
}

auto current_vars(compiler& com) -> var_locations&
{
    return com.current_func ? com.current_func->vars : com.globals;
}

auto get_function(
    const compiler& com,
    const type_name& struct_name,
    const std::string& function_name,
    const type_names& params
)
    -> std::optional<function_info>
{
    if (const auto it = com.functions.find(struct_name); it != com.functions.end()) {
        if (const auto it2 = it->second.find(function_name); it2 != it->second.end()) {
            if (const auto it3 = it2->second.find(params); it3 != it2->second.end()) {
                return it3->second;
            }
        }
    }
    return std::nullopt;
}

auto push_function_call_begin(compiler& com) -> void
{
    com.program.emplace_back(op_push_literal_u64{0}); // base ptr
    com.program.emplace_back(op_push_literal_u64{0}); // prog ptr
}

auto push_function_call(compiler& com, std::size_t ptr, const std::vector<type_name>& params) -> void
{
    auto args_size = 2 * sizeof(std::uint64_t);
    for (const auto& param : params) {
        args_size += com.types.size_of(param);
    }

    com.program.emplace_back(op_function_call{ .ptr=ptr, .args_size=args_size });
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
            com.program.emplace_back(op_push_literal_u64{offset});
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

struct signature
{
    std::vector<type_name> params;
    type_name              return_type;
};


void verify_sig(
    const token& tok,
    const std::vector<type_name>& expected,
    const std::vector<type_name>& actual)
{
    if (expected.size() != actual.size()) {
        tok.error("function expected {} args, got {}", expected.size(), actual.size());
    }

    auto arg_index = std::size_t{0};
    for (const auto& [expected_param, actual_param] : zip(expected, actual)) {
        if (expected_param != actual_param) {
            tok.error("arg {} type '{}' does not match '{}'", arg_index, actual_param, expected_param);
            ++arg_index;
        }
    }
}

auto get_constructor_params(const compiler& com, const type_name& type) -> std::vector<type_name>
{
    auto params = std::vector<type_name>{};
    for (const auto& field : com.types.fields_of(type)) {
        params.emplace_back(field.type);
    }
    return params;
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

auto assign_fn_params(const type_name& type) -> type_names
{
    return { concrete_ptr_type(type), concrete_ptr_type(type) };
}

auto copy_fn_params(const type_name& type) -> type_names
{
    return { concrete_ptr_type(type) };
}

auto drop_fn_params(const type_name& type) -> type_names
{
    return { concrete_ptr_type(type) };
}

// Assumes that the given "push_object_ptr" is a function that compiles code to produce
// a pointer to an object of the given type. This function compiles
// code to destruct that object.
using compile_obj_ptr_cb = std::function<void(const token&)>;
auto call_destructor(compiler& com, const type_name& type, compile_obj_ptr_cb push_object_ptr) -> void
{
    std::visit(overloaded{
        [&](const type_simple&) {
            const auto params = drop_fn_params(type);
            if (const auto func = get_function(com, type, "drop", params); func) {
                // Push the args to the stack
                push_function_call_begin(com);
                push_object_ptr(func->tok);
                push_function_call(com, func->ptr, params);
                com.program.emplace_back(op_pop{ .size = com.types.size_of(func->return_type) });
            }

            // Loop through the fields and call their destructors.
            const auto fields = com.types.fields_of(type);
            for (const auto& field : fields | std::views::reverse) {
                call_destructor(com, field.type, [&](const token& tok) {
                    push_object_ptr(tok);
                    push_adjust_ptr_to_field(com, tok, field.type, field.name);
                });
            }
        },
        [&](const type_list& list_type) {
            const auto inner_size = com.types.size_of(*list_type.inner_type);
            const auto params = drop_fn_params(*list_type.inner_type);

            if (const auto drop = get_function(com, *list_type.inner_type, "drop", params)) {
                for (std::size_t i = array_length(type); i != 0;) {
                    --i;
                    push_function_call_begin(com);
                    push_object_ptr(drop->tok);
                    push_ptr_adjust(com, i * inner_size);
                    push_function_call(com, drop->ptr, params);
                }
            }
        },
        [&](const type_ptr&) {
            // pointers have no destructors
        },
        [&](const type_span&) {
            // spans have no destructors
        },
        [&](const type_function_ptr&) {
            // functions pointers have no destructors
        }
    }, type);
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
    if (node && std::holds_alternative<node_name_expr>(*node->return_value)) {
        return_variable = std::get<node_name_expr>(*node->return_value).name;
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
        const auto params = copy_fn_params(etype);

        const auto copy = get_function(com, etype, "copy", params);
        tok.assert(copy.has_value(), "{} cannot be copied", etype);

        for (std::size_t i = 0; i != array_length(type); ++i) {
            push_function_call_begin(com);
            push_expr_ptr(com, expr);
            push_ptr_adjust(com, i * inner_size);
            push_function_call(com, copy->ptr, params);
        }
    }

    else {
        const auto params = copy_fn_params(type);
        const auto copy = get_function(com, type, "copy", params);
        tok.assert(copy.has_value(), "{} cannot be copied", type);

        push_function_call_begin(com);
        push_expr_ptr(com, expr);
        push_function_call(com, copy->ptr, params);
    }

    return type;
}

// Gets the type of the expression by compiling it, then removes the added
// op codes to leave the program unchanged before returning the type.
auto type_of_expr(compiler& com, const node_expr& node) -> type_name
{
    const auto program_size = com.program.size();
    const auto type = push_expr_val(com, node);
    com.program.resize(program_size);
    return type;
}

auto push_expr_ptr(compiler& com, const node_name_expr& node) -> type_name
{
    auto& global_fns = com.functions[global_namespace];
    if (auto it = global_fns.find(node.name); it != global_fns.end()) {
        node.token.error("cannot take address of a function pointer");
    }

    return push_var_addr(com, node.token, node.name);
}

// I think this is a bit of a hack; when pushing the value of a function pointer, we need
// to do it in a special way. TODO: I think this messes with the idea that variable nodes
// are lvalues, so that may cause trouble; we should find out how.
auto push_expr_val(compiler& com, const node_name_expr& node) -> type_name
{
    auto& global_fns = com.functions[global_namespace];
    if (auto it = global_fns.find(node.name); it != global_fns.end()) {
        // We are dealing with a function, so return a function pointer provided
        // this isn't an overloaded function
        if (it->second.size() > 1) {
            node.token.error("cannot get function pointer to an overloaded function\n");
        }
        const auto& [key, value] = *it->second.begin();
        com.program.emplace_back(op_push_literal_u64{value.ptr});

        // next, construct the return type.
        const auto ptr_type = type_function_ptr{
            .param_types = key,
            .return_type = make_value<type_name>(value.return_type)
        };
        return ptr_type;
    }

    // This is the default logic for pushing an lvalue.
    const auto type = push_expr_ptr(com, node);
    const auto size = com.types.size_of(type);
    com.program.emplace_back(op_load{ .size=size });
    return type;
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
    const auto expr_type = type_of_expr(com, *expr.expr);

    const auto is_array = is_list_type(expr_type);
    const auto is_span = is_span_type(expr_type);
    expr.token.assert(is_array || is_span, "subscript only supported for arrays and spans");

    push_expr_ptr(com, *expr.expr);

    // If we are a span, we want the address that it holds rather than its own address,
    // so switch the pointer by loading what it's pointing at.
    if (is_span_type(expr_type)) {
        com.program.emplace_back(op_load{ .size=size_of_ptr()});
    }

    // Bounds checking on the subscript, it's unsigned so only need to check upper bound
    if (com.debug) {
        const auto index = push_expr_val(com, *expr.index);
        expr.token.assert_eq(index, u64_type(), "subscript argument must be u64, got {}", index);
        if (is_array) {
            com.program.emplace_back(op_push_literal_u64{array_length(expr_type)});
        } else {
            push_expr_ptr(com, *expr.expr);
            com.program.emplace_back(op_push_literal_u64{size_of_ptr()});
            com.program.emplace_back(op_u64_add{}); // offset to the size value
            com.program.emplace_back(op_load{ .size = com.types.size_of(u64_type()) }); // load the size
        }
        com.program.emplace_back(op_u64_lt{});
        com.program.emplace_back(op_assert{"index out of range"});
    }

    // Offset pointer by (index * size)
    const auto inner = inner_type(expr_type);
    const auto index = push_expr_val(com, *expr.index);
    expr.token.assert_eq(index, u64_type(), "subscript argument must be u64, got {}", index);
    com.program.emplace_back(op_push_literal_u64{com.types.size_of(inner)});
    com.program.emplace_back(op_u64_mul{});
    com.program.emplace_back(op_u64_add{}); // modify ptr
    return inner;
}

[[noreturn]] auto push_expr_ptr(compiler& com, const auto& node) -> type_name
{
    node.token.error("cannot take address of a non-lvalue\n");
}

auto push_expr_ptr(compiler& com, const node_expr& node) -> type_name
{
    return std::visit([&](const auto& expr) { return push_expr_ptr(com, expr); }, node);
}

// Fetches the given literal from read only memory, or adds it if it is not there, and
// returns the pointer.
auto insert_into_rom(compiler& com, const std::string& data) -> std::size_t
{
    const auto index = com.read_only_data.find(data);
    if (index != std::string::npos) {
        return set_rom_bit(index);
    }
    const auto ptr = com.read_only_data.size();
    com.read_only_data.append(data);
    return set_rom_bit(ptr);
}

auto push_expr_val(compiler& com, const node_literal_i32_expr& node) -> type_name
{
    com.program.emplace_back(op_push_literal_i32{node.value});
    return i32_type();
}

auto push_expr_val(compiler& com, const node_literal_i64_expr& node) -> type_name
{
    com.program.emplace_back(op_push_literal_i64{node.value});
    return i64_type();
}

auto push_expr_val(compiler& com, const node_literal_u64_expr& node) -> type_name
{
    com.program.emplace_back(op_push_literal_u64{node.value});
    return u64_type();
}

auto push_expr_val(compiler& com, const node_literal_f64_expr& node) -> type_name
{
    com.program.emplace_back(op_push_literal_f64{node.value});
    return f64_type();
}

auto push_expr_val(compiler& com, const node_literal_char_expr& node) -> type_name
{
    com.program.emplace_back(op_push_literal_char{node.value});
    return char_type();
}

auto push_expr_val(compiler& com, const node_literal_string_expr& node) -> type_name
{
    // Push the span onto the stack
    com.program.emplace_back(op_push_literal_u64{insert_into_rom(com, node.value)});
    com.program.emplace_back(op_push_literal_u64{node.value.size()});
    return concrete_span_type(char_type());
}

auto push_expr_val(compiler& com, const node_literal_bool_expr& node) -> type_name
{
    com.program.emplace_back(op_push_literal_bool(node.value));
    return bool_type();
}

auto push_expr_val(compiler& com, const node_literal_null_expr& node) -> type_name
{
    com.program.emplace_back(op_push_literal_null{});
    return null_type();
}

auto push_expr_val(compiler& com, const node_binary_op_expr& node) -> type_name
{
    const auto lhs = push_expr_val(com, *node.lhs);
    const auto rhs = push_expr_val(com, *node.rhs);
    const auto op = node.token.type;
    
    const auto info = resolve_operation(lhs, rhs, op);
    node.token.assert(info.has_value(), "could not find op '{} {} {}'", lhs, op, rhs);
    com.program.emplace_back(info->op_code);
    return info->return_type;
}

auto push_expr_val(compiler& com, const node_unary_op_expr& node) -> type_name
{
    const auto type = push_expr_val(com, *node.expr);
    const auto op = node.token.type;

    const auto info = resolve_operation(type, op);
    node.token.assert(info.has_value(), "could not find op '{}{}'", op, type);
    com.program.emplace_back(info->op_code);
    return info->return_type;
}

auto push_expr_val(compiler& com, const node_call_expr& node) -> type_name
{
    // First, handle the cases where the thing we are trying to call is a name.
    if (std::holds_alternative<node_name_expr>(*node.expr)) {
        auto& inner = std::get<node_name_expr>(*node.expr);

        // First, it might be a constructor call
        const auto type = make_type(inner.name);
        if (inner.struct_name == nullptr && com.types.contains(type)) {
            const auto expected_params = get_constructor_params(com, type);
            std::vector<type_name> actual_params;
            for (const auto& arg : node.args) {
                actual_params.emplace_back(push_object_copy(com, *arg, node.token));
            }
            verify_sig(node.token, expected_params, actual_params);
            return type;
        }

        // Second, it might be a function call
        auto params = type_names{};
        params.reserve(node.args.size());
        for (const auto& arg : node.args) {
            params.push_back(type_of_expr(com, *arg));
        }
        
        const auto struct_type = resolve_type(com, node.token, inner.struct_name);
        if (const auto func = get_function(com, struct_type, inner.name, params); func) {
            push_function_call_begin(com);
            for (const auto& arg : node.args) {
                push_object_copy(com, *arg, node.token);
            }
            push_function_call(com, func->ptr, params);
            return func->return_type;
        }

        // Third, it might be a .size member function on a span, TODO: make it easier to add
        // builtin member functions first arg is a pointer to a span, so need to deref with
        // inner_type before checking if its a span. BUG: This will match ANY call a size
        // function, and if the type of the argument is not a list, ptr or span, inner_type is
        // not defined and we fail compilation.
        if (inner.name == "size" && node.args.size() == 1 &&
            is_span_type(inner_type(type_of_expr(com, *node.args[0]))))
        {
            push_expr_val(com, *node.args[0]); // push pointer to span
            com.program.emplace_back(op_push_literal_u64{size_of_ptr()});
            com.program.emplace_back(op_u64_add{}); // offset to the size value
            com.program.emplace_back(op_load{ .size = com.types.size_of(u64_type()) }); // load the size
            return u64_type();
        }

        // Fourth, it might be a builtin function
        auto param_types = std::vector<type_name>{};
        auto args_size = std::size_t{0};
        for (const auto& arg : node.args) {
            param_types.emplace_back(push_object_copy(com, *arg, node.token));
            args_size += com.types.size_of(param_types.back());
        }

        if (const auto b = get_builtin_id(inner.name, param_types); b.has_value()) {
            com.program.emplace_back(op_builtin_call{
                .id=b.value(),
                .args_size=args_size
            });
            return get_builtin(*b).return_type;
        }
    }

    // Otherwise, the expression must be a function pointer.
    const auto type = type_of_expr(com, *node.expr);
    node.token.assert(
        is_function_ptr_type(type),
        "unable to call non-callable type {}", type
    );

    const auto& sig = std::get<type_function_ptr>(type);

    push_function_call_begin(com);
    auto actual_types = std::vector<type_name>{};
    for (const auto& arg : node.args) {
        const auto type = push_object_copy(com, *arg, node.token);
        actual_types.push_back(type);
    }
    verify_sig(node.token, sig.param_types, actual_types);

    auto args_size = 2 * sizeof(std::uint64_t);
    for (const auto& param_type : actual_types) {
        args_size += com.types.size_of(param_type);
    }

    // push the function pointer and call it
    push_expr_val(com, *node.expr);
    com.program.emplace_back(op_call{ .args_size = args_size });
    return *sig.return_type;
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
    com.program.emplace_back(op_push_literal_u64{com.types.size_of(type)});
    return u64_type();
}

auto push_expr_val(compiler& com, const node_span_expr& node) -> type_name
{
    if ((node.lower_bound && !node.upper_bound) || (!node.lower_bound && node.upper_bound)) {
        node.token.error("a span must either have both bounds set, or neither");
    }

    const auto expr_type = type_of_expr(com, *node.expr);
    node.token.assert(is_list_type(expr_type), "can only span arrays, not {}", expr_type);

    // Bounds checking
    if (com.debug && node.lower_bound && node.upper_bound) {
        const auto lower_bound_type = push_expr_val(com, *node.lower_bound);
        const auto upper_bound_type = push_expr_val(com, *node.upper_bound);
        node.token.assert_eq(lower_bound_type, u64_type(), "subspan indices must be u64");
        node.token.assert_eq(upper_bound_type, u64_type(), "subspan indices must be u64");
        com.program.emplace_back(op_u64_lt{});
        com.program.emplace_back(op_assert{"lower bound must be stricly less than the upper bound"});

        push_expr_val(com, *node.upper_bound);
        com.program.emplace_back(op_push_literal_u64{array_length(expr_type)});
        com.program.emplace_back(op_u64_lt{});
        com.program.emplace_back(op_assert{"upper bound must be strictly less than the array size"});
    }

    push_expr_ptr(com, *node.expr);
    if (node.lower_bound) {// move first index of span up
        com.program.emplace_back(op_push_literal_u64{com.types.size_of(inner_type(expr_type))});
        const auto lower_bound_type = push_expr_val(com, *node.lower_bound);
        node.token.assert_eq(lower_bound_type, u64_type(), "subspan indices must be u64");
        com.program.emplace_back(op_u64_mul{});
        com.program.emplace_back(op_u64_add{});
    }

    // next push the size to make up the second half of the span
    if (node.lower_bound && node.upper_bound) {
        push_expr_val(com, *node.upper_bound);
        push_expr_val(com, *node.lower_bound);
        com.program.emplace_back(op_u64_sub{});
    } else {
        com.program.emplace_back(op_push_literal_u64{array_length(expr_type)});
    }

    return concrete_span_type(inner_type(expr_type));
}

auto push_expr_val(compiler& com, const node_new_expr& node) -> type_name
{
    if (node.size) {
        const auto count = push_expr_val(com, *node.size);
        node.token.assert_eq(count, u64_type(), "invalid array size type");
        const auto type = resolve_type(com, node.token, node.type);
        com.program.emplace_back(op_alloc_span{ .type_size=com.types.size_of(type) });
        push_expr_val(com, *node.size); // push the size again to make the second half of the span
        return concrete_span_type(type);
    }

    const auto type = resolve_type(com, node.token, node.type);
    com.program.emplace_back(op_alloc_ptr{ .type_size=com.types.size_of(type) });
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

void push_stmt(compiler& com, const node_sequence_stmt& node)
{
    const auto scope = scope_guard{com};
    for (const auto& seq_node : node.sequence) {
        push_stmt(com, *seq_node);
    }
}

auto push_loop(compiler& com, std::function<void()> body) -> void
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

void push_stmt(compiler& com, const node_loop_stmt& node)
{
    push_loop(com, [&] {
        push_stmt(com, *node.body);
    });
}

void push_break(compiler& com, const token& tok);

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
void push_stmt(compiler& com, const node_while_stmt& node)
{
    push_loop(com, [&] {
        // if !<condition> break;
        const auto cond_type = push_expr_val(com, *node.condition);
        node.token.assert_eq(cond_type, bool_type(), "while-stmt invalid condition");
        com.program.emplace_back(op_bool_not{});
        const auto jump_pos = append_op(com, op_jump_if_false{});
        push_break(com, node.token);
        std::get<op_jump_if_false>(com.program[jump_pos]).jump = com.program.size(); // Jump past the end if false
        
        // <body>
        push_stmt(com, *node.body);
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
void push_stmt(compiler& com, const node_for_stmt& node)
{
    const auto scope = scope_guard{com};

    const auto iter_type = type_of_expr(com, *node.iter);

    const auto is_array = is_list_type(iter_type);
    const auto is_lvalue_span = is_span_type(iter_type) && is_lvalue_expr(*node.iter);
    node.token.assert(is_array || is_lvalue_span, "for-loops only supported for arrays and lvalue spans");

    // Need to create a temporary if we're using an rvalue
    if (is_rvalue_expr(*node.iter)) {
        push_expr_val(com, *node.iter);
        declare_var(com, node.token, "#:iter", iter_type);
    }

    // idx := 0u;
    com.program.emplace_back(op_push_literal_u64{0});
    declare_var(com, node.token, "#:idx", u64_type());

    // size := length of iter;
    if (is_list_type(iter_type)) {
        com.program.emplace_back(op_push_literal_u64{array_length(iter_type)});
        declare_var(com, node.token, "#:size", u64_type());
    } else {
        node.token.assert(is_lvalue_expr(*node.iter), "for-loops only supported for lvalue spans");
        push_expr_ptr(com, *node.iter); // push pointer to span
        com.program.emplace_back(op_push_literal_u64{size_of_ptr()});
        com.program.emplace_back(op_u64_add{}); // offset to the size value
        com.program.emplace_back(op_load{ .size = com.types.size_of(u64_type()) }); // load the size
        declare_var(com, node.token, "#:size", u64_type());
    }

    push_loop(com, [&] {
        // if idx == size break;
        load_variable(com, node.token, "#:idx");
        load_variable(com, node.token, "#:size");
        com.program.emplace_back(op_u64_eq{});
        const auto jump_pos = append_op(com, op_jump_if_false{});
        push_break(com, node.token);
        std::get<op_jump_if_false>(com.program[jump_pos]).jump = com.program.size(); // Jump past the end if false

        // name := &iter[idx];
        const auto iter_type = type_of_expr(com, *node.iter);
        const auto inner = inner_type(iter_type);
        if (is_rvalue_expr(*node.iter)) {
            push_var_addr(com, node.token, "#:iter");
        } else {
            push_expr_ptr(com, *node.iter);
            if (is_span_type(iter_type)) {
                com.program.emplace_back(op_load{ .size=com.types.size_of(concrete_ptr_type(inner))});
            }
        }
        load_variable(com, node.token, "#:idx");
        com.program.emplace_back(op_push_literal_u64{com.types.size_of(inner_type(iter_type))});
        com.program.emplace_back(op_u64_mul{});
        com.program.emplace_back(op_u64_add{});
        declare_var(com, node.token, node.name, concrete_ptr_type(inner_type(iter_type)));

        // idx = idx + 1;
        load_variable(com, node.token, "#:idx");
        com.program.emplace_back(op_push_literal_u64{1});
        com.program.emplace_back(op_u64_add{});
        save_variable(com, node.token, "#:idx");

        // main body
        push_stmt(com, *node.body);
    });
}

void push_stmt(compiler& com, const node_if_stmt& node)
{
    const auto cond_type = push_expr_val(com, *node.condition);
    node.token.assert_eq(cond_type, bool_type(), "if-stmt invalid condition");

    const auto jump_pos = append_op(com, op_jump_if_false{});
    push_stmt(com, *node.body);

    if (node.else_body) {
        const auto else_pos = append_op( com, op_jump{});
        push_stmt(com, *node.else_body);
        std::get<op_jump_if_false>(com.program[jump_pos]).jump = else_pos + 1; // Jump into the else block if false
        std::get<op_jump>(com.program[else_pos]).jump = com.program.size(); // Jump past the end if false
    } else {
        std::get<op_jump_if_false>(com.program[jump_pos]).jump = com.program.size(); // Jump past the end if false
    }
}

void push_stmt(compiler& com, const node_struct_stmt& node)
{
    const auto message = std::format("type '{}' already defined", node.name);
    node.token.assert(!com.types.contains(make_type(node.name)), message);
    node.token.assert(!com.functions[global_namespace].contains(node.name), message);

    auto fields = type_fields{};
    for (const auto& p : node.fields) {
        fields.emplace_back(field{ .name=p.name, .type=resolve_type(com, node.token, p.type) });
    }

    com.types.add(make_type(node.name), fields);
    for (const auto& function : node.functions) {
        push_stmt(com, *function);
    }
}

void push_break(compiler& com, const token& tok)
{
    tok.assert(!com.control_flow.empty(), "cannot use 'break' outside of a loop");
    destruct_on_break_or_continue(com);
    const auto pos = append_op(com, op_jump{});
    com.control_flow.top().break_stmts.insert(pos);
}

void push_stmt(compiler& com, const node_break_stmt& node)
{
    push_break(com, node.token);
}

void push_stmt(compiler& com, const node_continue_stmt& node)
{
    node.token.assert(!com.control_flow.empty(), "cannot use 'continue' outside of a loop");
    destruct_on_break_or_continue(com);
    const auto pos = append_op(com, op_jump{});
    com.control_flow.top().continue_stmts.insert(pos);
}

auto push_stmt(compiler& com, const node_declaration_stmt& node) -> void
{
    const auto type = push_object_copy(com, *node.expr, node.token);
    declare_var(com, node.token, node.name, type);
}

void push_stmt(compiler& com, const node_assignment_stmt& node)
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
        const auto params = assign_fn_params(etype);

        const auto assign = get_function(com, etype, "assign", params);
        node.token.assert(assign.has_value(), "{} cannot be assigned", etype);

        for (std::size_t i = 0; i != array_length(rhs); ++i) {
            push_function_call_begin(com);

            push_expr_ptr(com, *node.position); // i-th element of dst
            push_ptr_adjust(com, i * inner_size);
            push_expr_ptr(com, *node.expr); // i-th element of src
            push_ptr_adjust(com, i * inner_size);

            push_function_call(com, assign->ptr, params);
            pop_object(com, assign->return_type, node.token);
        }

        return;
    }

    const auto params = assign_fn_params(rhs);
    const auto assign = get_function(com, rhs, "assign", params);
    node.token.assert(assign.has_value(), "{} cannot be assigned", rhs);

    push_function_call_begin(com);
    push_expr_ptr(com, *node.position);
    push_expr_ptr(com, *node.expr);
    push_function_call(com, assign->ptr, params);
    pop_object(com, assign->return_type, node.token);
}

auto compile_function_body(
    compiler& com,
    const token& tok,
    const type_name& struct_type,
    const std::string& name,
    const node_signature& node_sig,
    const node_stmt_ptr& body
)
    -> signature
{
    auto sig = signature{};
    const auto begin_pos = append_op(com, op_jump{});

    com.current_func.emplace(current_function{ .vars={}, .return_type=null_type() });

    {
        const auto scope = scope_guard{com}; // Ensures destructors for params called

        declare_var(com, tok, "# old_base_ptr", u64_type()); // Store the old base ptr
        declare_var(com, tok, "# old_prog_ptr", u64_type()); // Store the old program ptr
        for (const auto& arg : node_sig.params) {
            const auto type = resolve_type(com, tok, arg.type);
            sig.params.push_back({type});
            declare_var(com, tok, arg.name, type);
        }

        sig.return_type = resolve_type(com, tok, node_sig.return_type);
        com.current_func->return_type = sig.return_type;
        if (com.functions[struct_type][name].contains(sig.params)) {
            tok.error("multiple definitions of {}({})", name, format_comma_separated(sig.params));
        }
        com.functions[struct_type][name][sig.params] = { .return_type=sig.return_type, .ptr=begin_pos + 1, .tok=tok };

        push_stmt(com, *body);

        if (!function_ends_with_return(*body)) {
            // A function returning null does not need a final return statement, and in this case
            // we manually add a return value of null here.
            if (sig.return_type == null_type()) {
                destruct_on_return(com);
                com.program.emplace_back(op_push_literal_null{});
                com.program.emplace_back(op_return{ .size=1 });
            } else {
                tok.error("function '{}::{}' does not end in a return statement", struct_type, name);
            }
        }
    }

    com.current_func.reset();

    std::get<op_jump>(com.program[begin_pos]).jump = com.program.size();
    
    return sig;
}

void push_stmt(compiler& com, const node_function_def_stmt& node)
{
    if (com.types.contains(make_type(node.name))) {
        node.token.error("'{}' cannot be a function name, it is a type def", node.name);
    }
    compile_function_body(com, node.token, global_namespace, node.name, node.sig, node.body);
}

void push_stmt(compiler& com, const node_member_function_def_stmt& node)
{
    const auto struct_type = make_type(node.struct_name);
    const auto expected = concrete_ptr_type(struct_type);
    const auto sig = compile_function_body(com, node.token, struct_type, node.function_name, node.sig, node.body);

    // Verification code
    node.token.assert(sig.params.size() >= 1, "member functions must have at least one arg");

    const auto actual = sig.params.front();
    node.token.assert_eq(actual, expected, "'{}' bad 1st arg", node.function_name);

    // Special function extra checks
    if (node.function_name == "drop") {
        node.token.assert_eq(sig.return_type, null_type(), "'drop' bad return type");
        node.token.assert_eq(sig.params.size(), 1, "'drop' bad number of args");
    }
    else if (node.function_name == "copy") {
        node.token.assert_eq(sig.return_type, struct_type, "'copy' bad return type");
        node.token.assert_eq(sig.params.size(), 1, "'copy' bad number of args");
    }
    else if (node.function_name == "assign") {
        node.token.assert_eq(sig.return_type, null_type(), "'assign' bad return type");
        node.token.assert_eq(sig.params.size(), 2, "'assign' bad number of args");
        node.token.assert_eq(sig.params.back(), expected, "'assign' bad 2nd arg");
    }

}

void push_stmt(compiler& com, const node_return_stmt& node)
{
    node.token.assert(com.current_func.has_value(), "can only return within functions");
    destruct_on_return(com, &node);
    const auto return_type = push_expr_val(com, *node.return_value);
    node.token.assert_eq(return_type, com.current_func->return_type, "wrong return type");
    com.program.emplace_back(op_return{ .size=com.types.size_of(return_type) });
}

void push_stmt(compiler& com, const node_expression_stmt& node)
{
    const auto type = push_expr_val(com, *node.expr);
    pop_object(com, type, node.token);
}

void push_stmt(compiler& com, const node_delete_stmt& node)
{
    const auto type = type_of_expr(com, *node.expr);
    if (is_span_type(type)) {
        push_expr_val(com, *node.expr);
        com.program.emplace_back(op_dealloc_span{ .type_size=com.types.size_of(inner_type(type)) });
    } else if (is_ptr_type(type)) {
        push_expr_val(com, *node.expr);
        com.program.emplace_back(op_dealloc_ptr{ .type_size=com.types.size_of(inner_type(type)) });
    } else {
        node.token.error("can only call delete spans and pointers, not {}", type);
    }
}

void push_stmt(compiler& com, const node_assert_stmt& node)
{
    const auto expr = type_of_expr(com, *node.expr);
    node.token.assert_eq(expr, bool_type(), "bad assertion expression");

    if (com.debug) {
        push_expr_val(com, *node.expr);
        const auto message = std::format("line {}", node.token.line);
        com.program.emplace_back(op_assert{message});
    }
}

auto push_expr_val(compiler& com, const node_expr& expr) -> type_name
{
    return std::visit([&](const auto& node) { return push_expr_val(com, node); }, expr);
}

auto push_stmt(compiler& com, const node_stmt& root) -> void
{
    std::visit([&](const auto& node) { push_stmt(com, node); }, root);
}

}

auto compiled_all_requirements(const parse_result& module, const std::set<std::filesystem::path>& compiled) -> bool
{
    for (const auto& requirement : module.required_modules) {
        if (!compiled.contains(requirement)) {
            return false;
        }
    }
    return true;
}

auto compile(
    const std::filesystem::path& main_dir,
    const std::map<std::filesystem::path, parse_result>& modules,
    const bool debug
)
    -> program
{
    auto com = compiler{};
    com.debug = debug;
    auto done = std::set<std::filesystem::path>{};
    auto remaining = std::set<std::filesystem::path>{}; 
    for (const auto& [file, mod] : modules) {
        remaining.emplace(file);
    }
    while (!remaining.empty()) {
        const auto before = remaining.size();
        std::erase_if(remaining, [&](const std::filesystem::path& curr) {
            const auto& mod = modules.at(curr);
            if (compiled_all_requirements(mod, done)) {
                print("    {}\n", curr.lexically_relative(main_dir).string());
                push_stmt(com, *mod.root);
                done.emplace(curr);
                return true;
            }
            return false;
        });
        const auto after = remaining.size();
        if (before == after) {
            print("Cyclic dependency detected among the following files:");
            for (const auto& mod : remaining) {
                print(" {}", mod.lexically_relative(main_dir).string());
            }
            print("\n");
            exit(1);
        }
    }

    auto read_only = std::vector<std::byte>{};
    read_only.reserve(com.read_only_data.size());
    for (char c : com.read_only_data) read_only.push_back(static_cast<std::byte>(c));
    return { com.program, read_only };
}

}