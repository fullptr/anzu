#include "compiler.hpp"
#include "lexer.hpp"
#include "object.hpp"
#include "parser.hpp"
#include "functions.hpp"
#include "utility/print.hpp"
#include "utility/overloaded.hpp"
#include "utility/views.hpp"
#include "utility/memory.hpp"

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
    std::vector<std::byte> program;
    std::string            read_only_data;

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
            push_value(d_com->program, op::pop, scope_size);
        }
    }

    scope_guard(const scope_guard&) = delete;
    scope_guard& operator=(const scope_guard&) = delete;
};

auto push_ptr_adjust(compiler& com, std::size_t offset) -> void
{
    push_value(com.program, op::push_u64, offset);
    push_value(com.program, op::u64_add); // modify ptr
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

auto push_function_call(compiler& com, std::size_t ptr, const std::vector<type_name>& params) -> void
{
    auto args_size = 2 * sizeof(std::uint64_t);
    for (const auto& param : params) {
        args_size += com.types.size_of(param);
    }

    push_value(com.program, op::push_u64, ptr, op::call, args_size);
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
            push_value(com.program, op::push_ptr_rel, info->location);
            return info->type;
        }
    }

    auto& globals = com.globals;
    if (const auto info = globals.find(name); info.has_value()) {
        push_value(com.program, op::push_ptr, info->location);
        return info->type;
    }

    tok.error("could not find variable '{}'\n", name);
}

auto load_variable(compiler& com, const token& tok, const std::string& name) -> void
{
    const auto type = push_var_addr(com, tok, name);
    const auto size = com.types.size_of(type);
    push_value(com.program, op::load, size);
}

auto save_variable(compiler& com, const token& tok, const std::string& name) -> void
{
    const auto type = push_var_addr(com, tok, name);
    const auto size = com.types.size_of(type);
    push_value(com.program, op::save, size);
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
            push_value(com.program, op::push_u64, offset);
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
    tok.assert(!is_reference_type(type), "cannot adjust pointer to field of a reference, "
                                         "as the value should already be dereferenced");
    const auto field_type = push_field_offset(com, tok, type, field_name);
    push_value(com.program, op::u64_add); // modify ptr
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
        if (actual_param != expected_param) {
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
                push_value(com.program, op::push_call_frame);
                push_object_ptr(func->tok);
                push_function_call(com, func->ptr, params);
                push_value(com.program, op::pop, com.types.size_of(func->return_type));
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
                    push_value(com.program, op::push_call_frame);
                    push_object_ptr(drop->tok);
                    push_ptr_adjust(com, i * inner_size);
                    push_function_call(com, drop->ptr, params);
                }
            }
        },
        [](const type_ptr&) {
            // pointers do not own anything to cloean up
        },
        [](const type_span&) {
            // spans do not own anything to cloean up
        },
        [](const type_function_ptr&) {
            // functions pointers do not own anything to cloean up
        },
        [](const type_reference&) {
            // references do not own anything to cloean up
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
        push_value(com.program, op::push_ptr_rel, object_ptr);
    });
    push_value(com.program, op::pop, com.types.size_of(type));
}

// Given an expression that evaluates to a reference, evaluate it and push to the top of the stack. If the expression
// is an lvalue, copy constructors are invoked.
auto push_object_ref_copy(compiler& com, const node_expr& expr, const token& tok) -> type_name
{
    const auto type = type_of_expr(com, expr);
    tok.assert(is_reference_type(type), "tried to push a copy of a non-ref");

    const auto inner = inner_type(type);
    
    if (is_rvalue_expr(expr) || is_type_trivially_copyable(inner)) {
        push_expr_val(com, expr);
        push_value(com.program, op::load, com.types.size_of(inner));
    }

    else if (is_list_type(inner)) {
        tok.assert(false, "copying a ref to an array not implemented yet");
        //const auto etype = inner_type(type);
        //const auto inner_size = com.types.size_of(etype);
        //const auto params = copy_fn_params(etype);

        //const auto copy = get_function(com, etype, "copy", params);
        //tok.assert(copy.has_value(), "{} cannot be copied", etype);

        //for (std::size_t i = 0; i != array_length(type); ++i) {
        //    push_value(com.program, op::push_call_frame);
        //    push_expr_ptr(com, expr);
        //    push_ptr_adjust(com, i * inner_size);
        //    push_function_call(com, copy->ptr, params);
        //}
    }

    else {
        const auto params = copy_fn_params(inner);
        const auto copy = get_function(com, type, "copy", params);
        tok.assert(copy.has_value(), "{} cannot be copied", inner);

        push_value(com.program, op::push_call_frame);
        push_expr_val(com, expr);
        push_function_call(com, copy->ptr, params);
    }

    return inner;
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
            push_value(com.program, op::push_call_frame);
            push_expr_ptr(com, expr);
            push_ptr_adjust(com, i * inner_size);
            push_function_call(com, copy->ptr, params);
        }
    }

    else {
        const auto params = copy_fn_params(type);
        const auto copy = get_function(com, type, "copy", params);
        tok.assert(copy.has_value(), "{} cannot be copied", type);

        push_value(com.program, op::push_call_frame);
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

// Fetches the given literal from read only memory, or adds it if it is not there, and
// returns the pointer.
auto insert_into_rom(compiler& com, std::string_view data) -> std::size_t
{
    const auto index = com.read_only_data.find(data);
    if (index != std::string::npos) {
        return set_rom_bit(index);
    }
    const auto ptr = com.read_only_data.size();
    com.read_only_data.append(data);
    return set_rom_bit(ptr);
}

auto push_assert(compiler& com, std::string_view message) -> void
{
    const auto index = unset_rom_bit(insert_into_rom(com, message));
    push_value(com.program, op::assert, index, message.size());
}

auto push_expr_ptr(compiler& com, const node_name_expr& node) -> type_name
{
    auto& global_fns = com.functions[global_namespace];
    if (auto it = global_fns.find(node.name); it != global_fns.end()) {
        node.token.error("cannot take address of a function pointer");
    }

    const auto type = push_var_addr(com, node.token, node.name);
    return type;
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
        push_value(com.program, op::push_u64, value.ptr);

        // next, construct the return type.
        const auto ptr_type = type_function_ptr{
            .param_types = key,
            .return_type = make_value<type_name>(value.return_type)
        };
        return ptr_type;
    }

    // This is the default logic for pushing an lvalue.
    const auto type = push_expr_ptr(com, node);
    push_value(com.program, op::load, com.types.size_of(type));
    return type;
}

auto push_expr_ptr(compiler& com, const node_field_expr& node) -> type_name
{
    auto type = push_expr_ptr(com, *node.expr);
    if (is_reference_type(type)) {
        push_value(com.program, op::load, size_of_reference());
        type = inner_type(type);
    }
    return push_adjust_ptr_to_field(com, node.token, type, node.field_name);
}

auto push_expr_ptr(compiler& com, const node_deref_expr& node) -> type_name
{
    const auto type = push_expr_val(com, *node.expr); // Push the address
    node.token.assert(is_ptr_type(type), "cannot use deref operator on non-ptr type '{}'", type);
    return inner_type(type);
}

auto push_expr_ptr(compiler& com, const node_subscript_expr& node) -> type_name
{
    const auto expr_type = type_of_expr(com, *node.expr);

    const auto is_array = is_list_type(expr_type);
    const auto is_span = is_span_type(expr_type);
    node.token.assert(is_array || is_span, "subscript only supported for arrays and spans");

    push_expr_ptr(com, *node.expr);

    // If we are a span, we want the address that it holds rather than its own address,
    // so switch the pointer by loading what it's pointing at.
    if (is_span_type(expr_type)) {
        push_value(com.program, op::load, size_of_ptr());
    }

    // Bounds checking on the subscript, it's unsigned so only need to check upper bound
    if (com.debug) {
        const auto index = push_expr_val(com, *node.index);
        node.token.assert_eq(index, u64_type(), "subscript argument must be u64, got {}", index);
        if (is_array) {
            push_value(com.program, op::push_u64, array_length(expr_type));
        } else {
            push_expr_ptr(com, *node.expr);
            push_value(com.program, op::push_u64, size_of_ptr());
            push_value(com.program, op::u64_add); // offset to the size value
            push_value(com.program, op::load, com.types.size_of(u64_type())); // load the size
        }
        push_value(com.program, op::u64_lt);
        push_assert(com, "index out of range");
    }

    // Offset pointer by (index * size)
    const auto inner = inner_type(expr_type);
    const auto index = push_expr_val(com, *node.index);
    node.token.assert_eq(index, u64_type(), "subscript argument must be u64, got {}", index);
    push_value(com.program, op::push_u64, com.types.size_of(inner));
    push_value(com.program, op::u64_mul);
    push_value(com.program, op::u64_add); // modify ptr
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

auto push_expr_val(compiler& com, const node_literal_i32_expr& node) -> type_name
{
    push_value(com.program, op::push_i32, node.value);
    return i32_type();
}

auto push_expr_val(compiler& com, const node_literal_i64_expr& node) -> type_name
{
    push_value(com.program, op::push_i64, node.value);
    return i64_type();
}

auto push_expr_val(compiler& com, const node_literal_u64_expr& node) -> type_name
{
    push_value(com.program, op::push_u64, node.value);
    return u64_type();
}

auto push_expr_val(compiler& com, const node_literal_f64_expr& node) -> type_name
{
    push_value(com.program, op::push_f64, node.value);
    return f64_type();
}

auto push_expr_val(compiler& com, const node_literal_char_expr& node) -> type_name
{
    push_value(com.program, op::push_char, node.value);
    return char_type();
}

auto push_expr_val(compiler& com, const node_literal_string_expr& node) -> type_name
{
    push_value(com.program, op::push_string_literal);
    push_value(com.program, insert_into_rom(com, node.value), node.value.size());
    return concrete_span_type(char_type());
}

auto push_expr_val(compiler& com, const node_literal_bool_expr& node) -> type_name
{
    push_value(com.program, op::push_bool, node.value);
    return bool_type();
}

auto push_expr_val(compiler& com, const node_literal_null_expr& node) -> type_name
{
    push_value(com.program, op::push_null);
    return null_type();
}

auto push_expr_val(compiler& com, const node_binary_op_expr& node) -> type_name
{
    using tt = token_type;
    const auto lhs = push_expr_val(com, *node.lhs);
    const auto rhs = push_expr_val(com, *node.rhs);

    if (lhs != rhs) node.token.error("could not find op '{} {} {}'", lhs, node.token.type, rhs);

    const auto& type = lhs;
    if (type == char_type()) {
        switch (node.token.type) {
            case tt::equal_equal: push_value(com.program, op::char_eq); return bool_type();
            case tt::bang_equal:  push_value(com.program, op::char_ne); return bool_type();
        }
    }
    else if (type == i32_type()) {
        switch (node.token.type) {
            case tt::plus:          push_value(com.program, op::i32_add); return type;
            case tt::minus:         push_value(com.program, op::i32_sub); return type;
            case tt::star:          push_value(com.program, op::i32_mul); return type;
            case tt::slash:         push_value(com.program, op::i32_div); return type;
            case tt::percent:       push_value(com.program, op::i32_mod); return type;
            case tt::equal_equal:   push_value(com.program, op::i32_eq); return bool_type();
            case tt::bang_equal:    push_value(com.program, op::i32_ne); return bool_type();
            case tt::less:          push_value(com.program, op::i32_lt); return bool_type();
            case tt::less_equal:    push_value(com.program, op::i32_le); return bool_type();
            case tt::greater:       push_value(com.program, op::i32_gt); return bool_type();
            case tt::greater_equal: push_value(com.program, op::i32_ge); return bool_type();
        }
    }
    else if (type == i64_type()) {
        switch (node.token.type) {
            case tt::plus:          push_value(com.program, op::i64_add); return type;
            case tt::minus:         push_value(com.program, op::i64_sub); return type;
            case tt::star:          push_value(com.program, op::i64_mul); return type;
            case tt::slash:         push_value(com.program, op::i64_div); return type;
            case tt::percent:       push_value(com.program, op::i64_mod); return type;
            case tt::equal_equal:   push_value(com.program, op::i64_eq); return bool_type();
            case tt::bang_equal:    push_value(com.program, op::i64_ne); return bool_type();
            case tt::less:          push_value(com.program, op::i64_lt); return bool_type();
            case tt::less_equal:    push_value(com.program, op::i64_le); return bool_type();
            case tt::greater:       push_value(com.program, op::i64_gt); return bool_type();
            case tt::greater_equal: push_value(com.program, op::i64_ge); return bool_type();
        }
    }
    else if (type == u64_type()) {
        switch (node.token.type) {
            case tt::plus:          push_value(com.program, op::u64_add); return type;
            case tt::minus:         push_value(com.program, op::u64_sub); return type;
            case tt::star:          push_value(com.program, op::u64_mul); return type;
            case tt::slash:         push_value(com.program, op::u64_div); return type;
            case tt::percent:       push_value(com.program, op::u64_mod); return type;
            case tt::equal_equal:   push_value(com.program, op::u64_eq); return bool_type();
            case tt::bang_equal:    push_value(com.program, op::u64_ne); return bool_type();
            case tt::less:          push_value(com.program, op::u64_lt); return bool_type();
            case tt::less_equal:    push_value(com.program, op::u64_le); return bool_type();
            case tt::greater:       push_value(com.program, op::u64_gt); return bool_type();
            case tt::greater_equal: push_value(com.program, op::u64_ge); return bool_type();
        }
    }
    else if (type == f64_type()) {
        switch (node.token.type) {
            case tt::plus:          push_value(com.program, op::f64_add); return type;
            case tt::minus:         push_value(com.program, op::f64_sub); return type;
            case tt::star:          push_value(com.program, op::f64_mul); return type;
            case tt::slash:         push_value(com.program, op::f64_div); return type;
            case tt::equal_equal:   push_value(com.program, op::f64_eq); return bool_type();
            case tt::bang_equal:    push_value(com.program, op::f64_ne); return bool_type();
            case tt::less:          push_value(com.program, op::f64_lt); return bool_type();
            case tt::less_equal:    push_value(com.program, op::f64_le); return bool_type();
            case tt::greater:       push_value(com.program, op::f64_gt); return bool_type();
            case tt::greater_equal: push_value(com.program, op::f64_ge); return bool_type();
        }
    }
    else if (type == bool_type()) {
        switch (node.token.type) {
            case tt::ampersand_ampersand: push_value(com.program, op::bool_and); return type;
            case tt::bar_bar:             push_value(com.program, op::bool_or);  return type;
            case tt::equal_equal:         push_value(com.program, op::bool_eq);  return type;
            case tt::bang_equal:          push_value(com.program, op::bool_ne);  return type;
        }
    }

    node.token.error("could not find op '{} {} {}'", lhs, node.token.type, rhs);
}

auto push_expr_val(compiler& com, const node_unary_op_expr& node) -> type_name
{
    using tt = token_type;
    const auto type = push_expr_val(com, *node.expr);

    switch (node.token.type) {
        case tt::minus: {
            if (type == i32_type()) push_value(com.program, op::i32_neg); return type;
            if (type == i64_type()) push_value(com.program, op::i64_neg); return type;
            if (type == f64_type()) push_value(com.program, op::f64_neg); return type;
        } break;
        case tt::bang: {
            if (type == bool_type()) push_value(com.program, op::bool_not); return type;
        } break;
    }
    node.token.error("could not find op '{}{}'", node.token.type, type);
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
            node.token.assert_eq(expected_params.size(), node.args.size(),
                                 "incorrect number of arguments to constructor call");
            for (std::size_t i = 0; i != node.args.size(); ++i) {
                const auto& actual = type_of_expr(com, *node.args.at(i));
                const auto& expected = expected_params[i];
                if (actual == expected) {
                    actual_params.emplace_back(push_object_copy(com, *node.args.at(i), node.token));
                }
                else if (is_reference_type(actual) && inner_type(actual) == expected) {
                    actual_params.emplace_back(push_object_ref_copy(com, *node.args.at(i), node.token));
                }
                else {
                    node.token.assert(false, "not yet implemented");
                }
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
            push_value(com.program, op::push_call_frame);
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
            push_value(com.program, op::push_u64, size_of_ptr());
            push_value(com.program, op::u64_add); // offset to the size value
            push_value(com.program, op::load, com.types.size_of(u64_type())); // load the size
            return u64_type();
        }

        // Lastly, it might be a builtin function
        auto param_types = std::vector<type_name>{};
        for (const auto& arg : node.args) {
            param_types.emplace_back(type_of_expr(com, *arg));
        }

        if (const auto b = get_builtin_id(inner.name, param_types); b.has_value()) {
            const auto& builtin = get_builtin(*b);
            for (std::size_t i = 0; i != builtin.args.size(); ++i) {
                if (builtin.args.at(i) == param_types[i]) {
                    push_object_copy(com, *node.args.at(i), node.token);
                }
                // In this case, we have a reference but the function accepts a value,
                // so push the value of the reference, which is an address, then load
                // the value. Note: this skips calling constructors, which is fine for
                // builtin types, but a more general solution is needed for passing references
                // to user functions.
                else if (is_reference_type(param_types[i])) {
                    push_expr_val(com, *node.args.at(i));
                    push_value(com.program, op::load, com.types.size_of(inner_type(param_types[i])));
                }
                // TODO: Handle the case where the function itself takes a reference and we
                // have passed a value. First need a way of expressing that a function takes
                // references, which requires a way to spell reference types. Either do this
                // or go straight to specifying mut/copy (and eventually in when we have const).
                else {
                    node.token.error("TODO: Loading references into builtin functions");
                }
            }
            push_value(com.program, op::builtin_call, *b);
            return get_builtin(*b).return_type;
        }

        node.token.error("Could not find function {}({})\n",
                         inner.name, format_comma_separated(param_types));
    }

    // Otherwise, the expression must be a function pointer.
    const auto type = type_of_expr(com, *node.expr);
    node.token.assert(is_function_ptr_type(type), "unable to call non-callable type {}", type);

    const auto& sig = std::get<type_function_ptr>(type);

    push_value(com.program, op::push_call_frame);
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
    push_value(com.program, op::call, args_size);
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
    push_value(com.program, op::push_u64, com.types.size_of(type));
    return u64_type();
}

auto push_expr_val(compiler& com, const node_span_expr& node) -> type_name
{
    if ((node.lower_bound && !node.upper_bound) || (!node.lower_bound && node.upper_bound)) {
        node.token.error("a span must either have both bounds set, or neither");
    }

    const auto expr_type = type_of_expr(com, *node.expr);
    node.token.assert(
        is_list_type(expr_type) || is_span_type(expr_type),
        "can only span arrays and other spans, not {}", expr_type
    );

    // Bounds checking (TODO: BOUNDS CHECKING ON SPANS TOO)
    if (is_list_type(expr_type) && com.debug && node.lower_bound && node.upper_bound) {
        const auto lower_bound_type = push_expr_val(com, *node.lower_bound);
        const auto upper_bound_type = push_expr_val(com, *node.upper_bound);
        node.token.assert_eq(lower_bound_type, u64_type(), "subspan indices must be u64");
        node.token.assert_eq(upper_bound_type, u64_type(), "subspan indices must be u64");
        push_value(com.program, op::u64_lt);
        push_assert(com, "lower bound must be stricly less than the upper bound");

        push_expr_val(com, *node.upper_bound);
        push_value(com.program, op::push_u64, array_length(expr_type));
        push_value(com.program, op::u64_lt);
        push_assert(com, "upper bound must be strictly less than the array size");
    }

    push_expr_ptr(com, *node.expr);

    // If we are a span, we want the address that it holds rather than its own address,
    // so switch the pointer by loading what it's pointing at.
    if (is_span_type(expr_type)) {
        push_value(com.program, op::load, size_of_ptr());
    }

    if (node.lower_bound) {// move first index of span up
        push_value(com.program, op::push_u64, com.types.size_of(inner_type(expr_type)));
        const auto lower_bound_type = push_expr_val(com, *node.lower_bound);
        node.token.assert_eq(lower_bound_type, u64_type(), "subspan indices must be u64");
        push_value(com.program, op::u64_mul);
        push_value(com.program, op::u64_add);
    }

    // next push the size to make up the second half of the span
    if (node.lower_bound && node.upper_bound) {
        push_expr_val(com, *node.upper_bound);
        push_expr_val(com, *node.lower_bound);
        push_value(com.program, op::u64_sub);
    } else if (is_span_type(expr_type)) {
        // Push the span pointer, offset to the size, and load the size
        push_expr_ptr(com, *node.expr);
        push_value(com.program, op::push_u64, size_of_ptr(), op::u64_add);
        push_value(com.program, op::load, com.types.size_of(u64_type()));
    } else {
        push_value(com.program, op::push_u64, array_length(expr_type));
    }

    return concrete_span_type(inner_type(expr_type));
}

auto push_expr_val(compiler& com, const node_new_expr& node) -> type_name
{
    if (node.size) {
        const auto count = push_expr_val(com, *node.size);
        node.token.assert_eq(count, u64_type(), "invalid array size type");
        const auto type = resolve_type(com, node.token, node.type);
        push_value(com.program, op::alloc_span, com.types.size_of(type));
        push_expr_val(com, *node.size); // push the size again to make the second half of the span
        return concrete_span_type(type);
    }

    const auto type = resolve_type(com, node.token, node.type);
    push_value(com.program, op::alloc_ptr, com.types.size_of(type));
    return concrete_ptr_type(type);
}

auto push_expr_val(compiler& com, const node_reference_expr& node) -> type_name
{
    const auto type = push_expr_ptr(com, *node.expr);
    return concrete_reference_type(type);
}

// If not implemented explicitly, assume that the given node_expr is an lvalue, in which case
// we can load it by pushing the address to the stack and loading.
auto push_expr_val(compiler& com, const auto& node) -> type_name
{
    const auto type = push_expr_ptr(com, node);
    push_value(com.program, op::load, com.types.size_of(type));
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
    push_value(com.program, op::jump, begin_pos);

    // Fix up the breaks and continues
    const auto& control_flow = com.control_flow.top();
    for (const auto idx : control_flow.break_stmts) {
        write_value(com.program, idx, com.program.size()); // Jump past end
    }
    for (const auto idx : control_flow.continue_stmts) {
        write_value(com.program, idx, begin_pos); // Jump to start
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
        push_value(com.program, op::bool_not);
        push_value(com.program, op::jump_if_false);
        const auto jump_pos = push_value(com.program, std::uint64_t{0});
        push_break(com, node.token);
        write_value(com.program, jump_pos, com.program.size()); // Jump past the end if false      
        
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
    push_value(com.program, op::push_u64, std::uint64_t{0});
    declare_var(com, node.token, "#:idx", u64_type());

    // size := length of iter;
    if (is_list_type(iter_type)) {
        push_value(com.program, op::push_u64, array_length(iter_type));
        declare_var(com, node.token, "#:size", u64_type());
    } else {
        node.token.assert(is_lvalue_expr(*node.iter), "for-loops only supported for lvalue spans");
        push_expr_ptr(com, *node.iter); // push pointer to span
        push_value(com.program, op::push_u64, size_of_ptr());
        push_value(com.program, op::u64_add); // offset to the size value
        push_value(com.program, op::load, com.types.size_of(u64_type()));       
        declare_var(com, node.token, "#:size", u64_type());
    }

    push_loop(com, [&] {
        // if idx == size break;
        load_variable(com, node.token, "#:idx");
        load_variable(com, node.token, "#:size");
        push_value(com.program, op::u64_eq);
        push_value(com.program, op::jump_if_false);
        const auto jump_pos = push_value(com.program, std::uint64_t{0});
        push_break(com, node.token);
        write_value(com.program, jump_pos, com.program.size());

        // name := &iter[idx];
        const auto iter_type = type_of_expr(com, *node.iter);
        const auto inner = inner_type(iter_type);
        if (is_rvalue_expr(*node.iter)) {
            push_var_addr(com, node.token, "#:iter");
        } else {
            push_expr_ptr(com, *node.iter);
            if (is_span_type(iter_type)) {
                push_value(com.program, op::load, com.types.size_of(concrete_ptr_type(inner)));
            }
        }
        load_variable(com, node.token, "#:idx");
        push_value(com.program, op::push_u64, com.types.size_of(inner_type(iter_type)));
        push_value(com.program, op::u64_mul);
        push_value(com.program, op::u64_add);
        declare_var(com, node.token, node.name, concrete_ptr_type(inner_type(iter_type)));

        // idx = idx + 1;
        load_variable(com, node.token, "#:idx");
        push_value(com.program, op::push_u64, std::uint64_t{1});
        push_value(com.program, op::u64_add);
        save_variable(com, node.token, "#:idx");

        // main body
        push_stmt(com, *node.body);
    });
}

void push_stmt(compiler& com, const node_if_stmt& node)
{
    const auto cond_type = push_expr_val(com, *node.condition);
    node.token.assert_eq(cond_type, bool_type(), "if-stmt invalid condition");

    push_value(com.program, op::jump_if_false);
    const auto jump_pos = push_value(com.program, std::uint64_t{0});
    push_stmt(com, *node.body);

    if (node.else_body) {
        push_value(com.program, op::jump);
        const auto else_pos = push_value(com.program, std::uint64_t{0});
        const auto in_else_pos = com.program.size();
        push_stmt(com, *node.else_body);
        write_value(com.program, jump_pos, in_else_pos); // Jump into the else block if false
        write_value(com.program, else_pos, com.program.size()); // Jump past the end if false
    } else {
        write_value(com.program, jump_pos, com.program.size()); // Jump past the end if false
    }
}

void push_stmt(compiler& com, const node_struct_stmt& node)
{
    const auto message = std::format("type '{}' already defined", node.name);
    node.token.assert(!com.types.contains(make_type(node.name)), "{}", message);
    node.token.assert(!com.functions[global_namespace].contains(node.name), "{}", message);

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
    push_value(com.program, op::jump);
    const auto pos = push_value(com.program, std::uint64_t{0}); // filled in later
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
    push_value(com.program, op::jump);
    const auto pos = push_value(com.program, std::uint64_t{0}); // filled in later
    com.control_flow.top().continue_stmts.insert(pos);
}

auto push_stmt(compiler& com, const node_declaration_stmt& node) -> void
{
    const auto type = push_object_copy(com, *node.expr, node.token);
    declare_var(com, node.token, node.name, type);
}

auto is_assignable(const type_name& lhs, const type_name& rhs) -> bool
{
    if (lhs != rhs) {
        // Support assigning to references
        if (is_reference_type(lhs) && inner_type(lhs) == rhs) {
            return true;
        }
        return false;
    }

    return true;
}

void push_stmt(compiler& com, const node_assignment_stmt& node)
{
    const auto rhs = type_of_expr(com, *node.expr);
    const auto lhs = type_of_expr(com, *node.position);
    
    node.token.assert(is_assignable(lhs, rhs), "invalid assignment");

    if (is_rvalue_expr(*node.expr) || is_type_trivially_copyable(rhs)) {
        const auto rhs = push_expr_val(com, *node.expr);
        const auto lhs = push_expr_ptr(com, *node.position);
        node.token.assert(is_assignable(lhs, rhs), "invalid assignment");

        // We don't want to assign to the address of the reference, we want to
        // assign the address that it stores
        if (is_reference_type(lhs)) {
            push_value(com.program, op::load, size_of_reference());
        }

        push_value(com.program, op::save, com.types.size_of(lhs));
        return;
    }
    
    if (is_list_type(rhs)) {
        const auto etype = inner_type(rhs);
        const auto inner_size = com.types.size_of(etype);
        const auto params = assign_fn_params(etype);

        const auto assign = get_function(com, etype, "assign", params);
        node.token.assert(assign.has_value(), "{} cannot be assigned", etype);

        for (std::size_t i = 0; i != array_length(rhs); ++i) {
            push_value(com.program, op::push_call_frame);

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

    push_value(com.program, op::push_call_frame);

    push_expr_ptr(com, *node.position);
    if (is_reference_type(lhs)) {
        push_value(com.program, op::load, size_of_reference());
    }

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
    push_value(com.program, op::jump);
    const auto jump_op = push_value(com.program, std::uint64_t{0});
    const auto begin_pos = com.program.size(); // First op code after the jump

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
        com.functions[struct_type][name][sig.params] = {
            .return_type=sig.return_type, .ptr=begin_pos, .tok=tok
        };

        push_stmt(com, *body);

        if (!function_ends_with_return(*body)) {
            // A function returning null does not need a final return statement, and in this case
            // we manually add a return value of null here.
            if (sig.return_type == null_type()) {
                destruct_on_return(com);
                push_value(com.program, op::push_null);
                push_value(com.program, op::ret, std::uint64_t{1});
            } else {
                tok.error("function '{}::{}' does not end in a return statement", struct_type, name);
            }
        }
    }

    com.current_func.reset();
    write_value(com.program, jump_op, com.program.size());
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
    push_value(com.program, op::ret, com.types.size_of(return_type));
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
        push_value(com.program, op::dealloc_span, com.types.size_of(inner_type(type)));
    } else if (is_ptr_type(type)) {
        push_expr_val(com, *node.expr);
        push_value(com.program, op::dealloc_ptr, com.types.size_of(inner_type(type)));
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
        push_assert(com, std::format("line {}", node.token.line));
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

auto compiled_all_requirements(const anzu_module& module, const std::set<std::filesystem::path>& compiled) -> bool
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
    const std::map<std::filesystem::path, anzu_module>& modules,
    const bool debug
)
    -> bytecode_program
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