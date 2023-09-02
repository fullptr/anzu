#include "compiler.hpp"
#include "lexer.hpp"
#include "object.hpp"
#include "parser.hpp"
#include "functions.hpp"
#include "scope_manager.hpp"
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

struct signature
{
    std::vector<type_name> params;
    type_name              return_type;
};

struct function_info
{
    signature   sig;
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

using type_name_hash = decltype([](const type_name& f) { return hash(f); });
using function_map = std::unordered_map<std::string, std::vector<function_info>>;

// Struct used to store information while compiling an AST. Contains the output program
// as well as information such as function definitions.
struct compiler
{
    std::vector<std::byte> program;
    std::string            read_only_data;

    bool debug = false;

    // namespace (type_name) -> function_name -> signatures -> function_info
    std::unordered_map<type_name, function_map, type_name_hash> functions;

    type_store types; // TODO: store a flag in here to say if a type is default/deleted/implemented copyable/assignable

    scope_manager scopes;
};

auto push_expr_ptr(compiler& com, const node_expr& node) -> type_name;
auto push_expr_val(compiler& com, const node_expr& expr) -> type_name;
auto push_stmt(compiler& com, const node_stmt& root) -> void;
auto type_of_expr(compiler& com, const node_expr& node) -> type_name;
auto call_destructor_named_var(compiler& com, const std::string& var, const type_name& type) -> void;

auto push_ptr_underlying(compiler& com, const node_expr& expr) -> type_name
{
    const auto type = type_of_expr(com, expr);
    if (type.is_ref()) {
        push_expr_val(com, expr);
    } else {
        push_expr_ptr(com, expr);
    }
    return type.remove_ref();
}

// TODO: Add an assert; this is only safe to use on fundamental types
auto push_val_underlying(compiler& com, const node_expr& expr) -> type_name
{
    const auto type = push_expr_val(com, expr);
    if (type.is_ref()) {
        push_value(com.program, op::load, com.types.size_of(type.remove_ref()));
    }
    return type.remove_ref();
}

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
            // References act like aliases, so taking the typeof a reference strips the reference away.
            return type_of_expr(com, *node.expr).remove_ref();
        }
    }, *type);

    tok.assert(com.types.contains(resolved_type), "{} is not a recognised type", resolved_type);
    return resolved_type;
}

auto push_ptr_adjust(compiler& com, std::size_t offset) -> void
{
    push_value(com.program, op::push_u64, offset);
    push_value(com.program, op::u64_add); // modify ptr
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
            for (const auto& function_info : it2->second) {
                if (are_types_convertible_to(params, function_info.sig.params)) {
                    return function_info;
                }
            }
        }
    }
    return std::nullopt;
}

auto push_function_call(compiler& com, const function_info& function) -> void
{
    auto args_size = 2 * sizeof(std::uint64_t);
    for (const auto& param : function.sig.params) {
        args_size += com.types.size_of(param);
    }

    push_value(com.program, op::push_u64, function.ptr, op::call, args_size);
}

// Registers the given name in the current scope
void declare_var(compiler& com, const token& tok, const std::string& name, const type_name& type)
{
    if (!com.scopes.declare(name, type, com.types.size_of(type))) {
        tok.error("name already in use: '{}'", name);
    }
}

auto push_var_addr(compiler& com, const token& tok, const std::string& name) -> type_name
{
    const auto var = com.scopes.find(name);
    if (!var) {
        tok.error("could not find variable '{}'\n", name);
    }
    const auto op = var->is_location_relative ? op::push_ptr_rel : op::push_ptr;
    push_value(com.program, op, var->location);
    return var->type;
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
    const auto field_type = push_field_offset(com, tok, type, field_name);
    push_value(com.program, op::u64_add); // modify ptr
    return field_type;
}

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

// TODO: Generalise further
auto function_ends_with_return(const node_stmt& node) -> bool
{
    if (std::holds_alternative<node_sequence_stmt>(node)) {
        const auto& seq = std::get<node_sequence_stmt>(node).sequence;
        if (seq.empty()) {
            return false;
        }
        if (std::holds_alternative<node_return_stmt>(*seq.back())) {
            return true;
        }
        if (std::holds_alternative<node_unsafe_stmt>(*seq.back())) {
            const auto& back = std::get<node_unsafe_stmt>(*seq.back());
            if (back.sequence.empty() || !std::holds_alternative<node_return_stmt>(*back.sequence.back())) {
                return false;
            }
            return true;
        }
        return false;
    }
    return std::holds_alternative<node_return_stmt>(node);
}

auto assign_fn_params(const type_name& type) -> type_names
{
    return { concrete_reference_type(type), concrete_reference_type(type) };
}

auto copy_fn_params(const type_name& type) -> type_names
{
    return { concrete_reference_type(type) };
}

auto drop_fn_params(const type_name& type) -> type_names
{
    return { concrete_reference_type(type) };
}

// Assumes that the given "push_object_ptr" is a function that compiles code to produce
// a pointer to an object of the given type. This function compiles
// code to destruct that object.
using compile_obj_ptr_cb = std::function<void(const token&)>;
auto call_destructor(compiler& com, const type_name& type, compile_obj_ptr_cb push_object_ptr) -> void
{
    std::visit(overloaded{
        [](type_fundamental) {
            // nothing to do
        },
        [&](const type_struct&) {
            const auto params = drop_fn_params(type);
            if (const auto func = get_function(com, type, "drop", params); func) {
                // Push the args to the stack
                push_value(com.program, op::push_call_frame);
                push_object_ptr(func->tok);
                push_function_call(com, *func);
                push_value(com.program, op::pop, com.types.size_of(func->sig.return_type));
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
        [&](const type_array& type) {
            const auto inner_size = com.types.size_of(*type.inner_type);
            const auto params = drop_fn_params(*type.inner_type);

            if (const auto drop = get_function(com, *type.inner_type, "drop", params)) {
                for (std::size_t i = array_length(type); i != 0;) {
                    --i;
                    push_value(com.program, op::push_call_frame);
                    push_object_ptr(drop->tok);
                    push_ptr_adjust(com, i * inner_size);
                    push_function_call(com, *drop);
                    push_value(com.program, op::pop, com.types.size_of(drop->sig.return_type));
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
        },
        [&](const type_const& t) {
            // Strip off the const and call function again
            call_destructor(com, type.remove_const(), push_object_ptr);
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
    for (const auto& scope : com.scopes.all() | std::views::reverse) {
        for (const auto& var : scope->variables() | std::views::reverse) {
            call_destructor_named_var(com, var.name, var.type);
        }
        if (scope->is<loop_scope>()) return;
    }
}

auto destruct_on_return(compiler& com, const node_return_stmt* node = nullptr) -> void
{
    auto return_variable = std::string{"#"};
    auto skip_return_destructor = true;
    if (node && std::holds_alternative<node_name_expr>(*node->return_value)) {
        return_variable = std::get<node_name_expr>(*node->return_value).name;
    }
    for (const auto& scope : com.scopes.all() | std::views::reverse) {
        for (const auto& var : scope->variables() | std::views::reverse) {
            // If the return expr is just a variable name, do not destruct that object.
            // Further, if there is a variable in an outer scope with the same name, make
            // sure to only skip destructing the inner one
            if (var.name != return_variable || !skip_return_destructor) {
                call_destructor_named_var(com, var.name, var.type);
            }
            else {
                skip_return_destructor = false;
            }
        }
        if (scope->is<function_scope>()) return;
    }
}

auto destruct_on_end_of_scope(compiler& com) -> void
{
    auto current = com.scopes.current();

    // destruct all variables in the current scope
    auto scope_size = std::size_t{0};
    for (const auto& variable : current->variables() | std::views::reverse) {
        scope_size += variable.size;
        call_destructor_named_var(com, variable.name, variable.type);
    }

    // deallocate all space used by the scope
    if (scope_size > 0) {
        push_value(com.program, op::pop, scope_size);
    }

    com.scopes.pop_scope();
}

class scope_guard
{
    compiler* d_com;
    scope_guard(compiler& com) : d_com{&com} {}
    scope_guard(const scope_guard&) = delete;
    scope_guard& operator=(const scope_guard&) = delete;

public:
    ~scope_guard() { destruct_on_end_of_scope(*d_com); }

    static auto global(compiler& com) -> scope_guard
    {
        com.scopes.new_global_scope();
        return scope_guard{com};
    }

    static auto block(compiler& com) -> scope_guard
    {
        com.scopes.new_block_scope(false);
        return scope_guard{com};
    }

    static auto unsafe_block(compiler& com) -> scope_guard
    {
        com.scopes.new_block_scope(true);
        return scope_guard{com};
    }

    static auto function(compiler& com, const type_name& return_type) -> scope_guard
    {
        com.scopes.new_function_scope(return_type);
        return scope_guard{com};
    }

    static auto loop(compiler& com) -> scope_guard
    {
        com.scopes.new_loop_scope();
        return scope_guard{com};
    }
};

// Given an expression, evaluate it and push to the top of the stack. If the expression
// is an lvalue, copy constructors are invoked. If the expression is a reference, it is
// automatically dereferenced
auto push_object_copy(compiler& com, const node_expr& expr, const token& tok) -> type_name
{
    const auto type = type_of_expr(com, expr);
    const auto real_type = type.remove_cr();

    if (is_rvalue_expr(expr) || is_type_trivially_copyable(real_type)) {
        push_val_underlying(com, expr);
    }

    else if (is_array_type(type)) {
        const auto etype = inner_type(type).remove_const();
        const auto esize = com.types.size_of(etype);

        const auto params = copy_fn_params(etype);
        const auto copy = get_function(com, etype, "copy", params);
        tok.assert(copy.has_value(), "{} cannot be copied", etype);

        for (std::size_t i = 0; i != array_length(type); ++i) {
            push_value(com.program, op::push_call_frame);
            push_ptr_underlying(com, expr);
            push_ptr_adjust(com, i * esize);
            push_function_call(com, *copy);
        }
    }
    
    else {
        const auto params = copy_fn_params(real_type);
        const auto copy = get_function(com, real_type, "copy", params);
        tok.assert(copy.has_value(), "{} cannot be copied", real_type);

        push_value(com.program, op::push_call_frame);
        push_ptr_underlying(com, expr);
        push_function_call(com, *copy);
    }

    return type.remove_ref(); // Keep constness intact
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
        const auto& info = *it->second.begin();
        push_value(com.program, op::push_u64, info.ptr);

        // next, construct the return type.
        const auto ptr_type = type_function_ptr{
            .param_types = info.sig.params,
            .return_type = make_value<type_name>(info.sig.return_type)
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
    const auto [type, is_const, is_ref] = push_ptr_underlying(com, *node.expr).strip_qualifiers();

    auto ret = push_adjust_ptr_to_field(com, node.token, type, node.field_name);
    if (is_const) ret = ret.add_const(); // Propagate const to members
    return ret;
}

auto push_expr_ptr(compiler& com, const node_deref_expr& node) -> type_name
{
    const auto type = push_val_underlying(com, *node.expr); // Push the address
    node.token.assert(is_ptr_type(type.remove_const()), "cannot use deref operator on non-ptr type '{}'", type);
    return inner_type(type.remove_const());
}

auto push_expr_ptr(compiler& com, const node_subscript_expr& node) -> type_name
{
    const auto expr_type = type_of_expr(com, *node.expr);
    const auto [real_type, is_const, is_ref] = expr_type.strip_qualifiers();

    const auto is_array = is_array_type(real_type);
    const auto is_span = is_span_type(real_type);
    node.token.assert(is_array || is_span, "subscript only supported for arrays and spans");

    push_ptr_underlying(com, *node.expr);

    // If we are a span, we want the address that it holds rather than its own address,
    // so switch the pointer by loading what it's pointing at.
    if (is_span_type(real_type)) {
        push_value(com.program, op::load, size_of_ptr());
    }

    // Bounds checking on the subscript, it's unsigned so only need to check upper bound
    if (com.debug) {
        const auto index = push_expr_val(com, *node.index);
        node.token.assert_eq(index, u64_type(), "subscript argument must be u64, got {}", index);
        if (is_array) {
            push_value(com.program, op::push_u64, array_length(real_type));
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
    const auto inner = inner_type(real_type);
    const auto index = push_expr_val(com, *node.index);
    node.token.assert_eq(index, u64_type(), "subscript argument must be u64, got {}", index);
    push_value(com.program, op::push_u64, com.types.size_of(inner));
    push_value(com.program, op::u64_mul);
    push_value(com.program, op::u64_add); // modify ptr
    if (is_array && is_const) {
        return inner.add_const();
    }
    return inner;
}

auto push_expr_ptr(compiler& com, const node_const_expr& node) -> type_name
{
    return push_expr_ptr(com, *node.expr).add_const();
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
    return concrete_span_type(char_type().add_const());
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
    auto lhs = push_val_underlying(com, *node.lhs);
    auto rhs = push_val_underlying(com, *node.rhs);
    auto lhs_real = lhs.remove_const();
    auto rhs_real = rhs.remove_const();

    if (lhs_real != rhs_real) node.token.error("could not find op '{} {} {}'", lhs, node.token.type, rhs);
    const auto& type = lhs_real;

    if (type == char_type()) {
        switch (node.token.type) {
            case tt::equal_equal: { push_value(com.program, op::char_eq); return bool_type(); }
            case tt::bang_equal:  { push_value(com.program, op::char_ne); return bool_type(); }
        }
    }
    else if (type == i32_type()) {
        switch (node.token.type) {
            case tt::plus:          { push_value(com.program, op::i32_add); return type;       }
            case tt::minus:         { push_value(com.program, op::i32_sub); return type;       }
            case tt::star:          { push_value(com.program, op::i32_mul); return type;       }
            case tt::slash:         { push_value(com.program, op::i32_div); return type;       }
            case tt::percent:       { push_value(com.program, op::i32_mod); return type;       }
            case tt::equal_equal:   { push_value(com.program, op::i32_eq); return bool_type(); }
            case tt::bang_equal:    { push_value(com.program, op::i32_ne); return bool_type(); }
            case tt::less:          { push_value(com.program, op::i32_lt); return bool_type(); }
            case tt::less_equal:    { push_value(com.program, op::i32_le); return bool_type(); }
            case tt::greater:       { push_value(com.program, op::i32_gt); return bool_type(); }
            case tt::greater_equal: { push_value(com.program, op::i32_ge); return bool_type(); }
        }
    }
    else if (type == i64_type()) {
        switch (node.token.type) {
            case tt::plus:          { push_value(com.program, op::i64_add); return type;       }
            case tt::minus:         { push_value(com.program, op::i64_sub); return type;       }
            case tt::star:          { push_value(com.program, op::i64_mul); return type;       }
            case tt::slash:         { push_value(com.program, op::i64_div); return type;       }
            case tt::percent:       { push_value(com.program, op::i64_mod); return type;       }
            case tt::equal_equal:   { push_value(com.program, op::i64_eq); return bool_type(); }
            case tt::bang_equal:    { push_value(com.program, op::i64_ne); return bool_type(); }
            case tt::less:          { push_value(com.program, op::i64_lt); return bool_type(); }
            case tt::less_equal:    { push_value(com.program, op::i64_le); return bool_type(); }
            case tt::greater:       { push_value(com.program, op::i64_gt); return bool_type(); }
            case tt::greater_equal: { push_value(com.program, op::i64_ge); return bool_type(); }
        }
    }
    else if (type == u64_type()) {
        switch (node.token.type) {
            case tt::plus:          { push_value(com.program, op::u64_add); return type;       }
            case tt::minus:         { push_value(com.program, op::u64_sub); return type;       }
            case tt::star:          { push_value(com.program, op::u64_mul); return type;       }
            case tt::slash:         { push_value(com.program, op::u64_div); return type;       }
            case tt::percent:       { push_value(com.program, op::u64_mod); return type;       }
            case tt::equal_equal:   { push_value(com.program, op::u64_eq); return bool_type(); }
            case tt::bang_equal:    { push_value(com.program, op::u64_ne); return bool_type(); }
            case tt::less:          { push_value(com.program, op::u64_lt); return bool_type(); }
            case tt::less_equal:    { push_value(com.program, op::u64_le); return bool_type(); }
            case tt::greater:       { push_value(com.program, op::u64_gt); return bool_type(); }
            case tt::greater_equal: { push_value(com.program, op::u64_ge); return bool_type(); }
        }
    }
    else if (type == f64_type()) {
        switch (node.token.type) {
            case tt::plus:          { push_value(com.program, op::f64_add); return type;       }
            case tt::minus:         { push_value(com.program, op::f64_sub); return type;       }
            case tt::star:          { push_value(com.program, op::f64_mul); return type;       }
            case tt::slash:         { push_value(com.program, op::f64_div); return type;       }
            case tt::equal_equal:   { push_value(com.program, op::f64_eq); return bool_type(); }
            case tt::bang_equal:    { push_value(com.program, op::f64_ne); return bool_type(); }
            case tt::less:          { push_value(com.program, op::f64_lt); return bool_type(); }
            case tt::less_equal:    { push_value(com.program, op::f64_le); return bool_type(); }
            case tt::greater:       { push_value(com.program, op::f64_gt); return bool_type(); }
            case tt::greater_equal: { push_value(com.program, op::f64_ge); return bool_type(); }
        }
    }
    else if (type == bool_type()) {
        switch (node.token.type) {
            case tt::ampersand_ampersand: { push_value(com.program, op::bool_and); return type; }
            case tt::bar_bar:             { push_value(com.program, op::bool_or);  return type; }
            case tt::equal_equal:         { push_value(com.program, op::bool_eq);  return type; }
            case tt::bang_equal:          { push_value(com.program, op::bool_ne);  return type; }
        }
    }

    node.token.error("could not find op '{} {} {}'", lhs, node.token.type, rhs);
}

auto push_expr_val(compiler& com, const node_unary_op_expr& node) -> type_name
{
    using tt = token_type;
    const auto raw_type = push_val_underlying(com, *node.expr);
    const auto type = raw_type.remove_const();

    switch (node.token.type) {
        case tt::minus: {
            if (type == i32_type()) { push_value(com.program, op::i32_neg); return type; }
            if (type == i64_type()) { push_value(com.program, op::i64_neg); return type; }
            if (type == f64_type()) { push_value(com.program, op::f64_neg); return type; }
        } break;
        case tt::bang: {
            if (type == bool_type()) { push_value(com.program, op::bool_not); return type; }
        } break;
    }
    node.token.error("could not find op '{}{}'", node.token.type, type);
}

[[nodiscard]] auto push_function_arg(
    compiler& com, const node_expr& expr, const type_name& expected, const token& tok
) -> bool
{
    const auto actual = type_of_expr(com, expr);
    
    if (is_span_type(expected) && is_array_type(actual)) {
        push_expr_ptr(com, expr);
        push_value(com.program, op::push_u64, array_length(actual));
        return true;
    }
    
    if (expected.remove_cr() == actual.remove_cr() && expected.is_ref() && !actual.is_const()) {
        push_ptr_underlying(com, expr);
        return true;
    }
    
    if (actual == expected) {
        push_object_copy(com, expr, tok);
        return true;
    }

    if (actual.add_const() == expected) {
        push_object_copy(com, expr, tok);
        return true;
    }

    if (expected.add_const() == actual && !actual.is_ref()) {
        push_object_copy(com, expr, tok);
        return true;
    }

    if (actual.remove_ref() == expected) {
        push_object_copy(com, expr, tok);
        return true;
    }

    if (actual.remove_cr() == expected && !expected.is_ref()) {
        push_object_copy(com, expr, tok);
        return true;
    }

    return false;
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
            node.token.assert_eq(expected_params.size(), node.args.size(),
                                 "incorrect number of arguments to constructor call");
            for (std::size_t i = 0; i != node.args.size(); ++i) {
                if (!push_function_arg(com, *node.args.at(i), expected_params[i], node.token)) {
                    node.token.error(
                        "Could not convert arg of type '{}' to '{}'",
                        type_of_expr(com, *node.args.at(i)),
                        expected_params[i]
                    );
                }
            }
            if (node.args.size() == 0) { // if the class has no data, it needs to be size 1
                push_value(com.program, op::push_null);
            }
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
            for (std::size_t i = 0; i != node.args.size(); ++i) {
                if (!push_function_arg(com, *node.args.at(i), func->sig.params[i], node.token)) {
                    node.token.error(
                        "Could not convert arg of type '{}' to '{}'",
                        type_of_expr(com, *node.args.at(i)),
                        func->sig.params[i]
                    );
                }
            }
            push_function_call(com, *func);
            return func->sig.return_type;
        }

        // Third, it might be a .size member function on a span, TODO: make it easier to add
        // builtin member functions first arg is a pointer to a span, so need to deref with
        // inner_type before checking if its a span. BUG: This will match ANY call a size
        // function, and if the type of the argument is not an array, ptr or span, inner_type is
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
        if (const auto b = get_builtin_id(inner.name, params); b.has_value()) {
            const auto& builtin = get_builtin(*b);
            for (std::size_t i = 0; i != builtin.args.size(); ++i) {
                if (!push_function_arg(com, *node.args.at(i), builtin.args[i], node.token)) {
                    node.token.error(
                        "Could not convert arg of type '{}' to '{}'",
                        type_of_expr(com, *node.args.at(i)),
                        builtin.args[i]
                    );
                }
            }
            push_value(com.program, op::builtin_call, *b);
            return get_builtin(*b).return_type;
        }
    }

    // Otherwise, the expression must be a function pointer.
    const auto type = type_of_expr(com, *node.expr);
    node.token.assert(is_function_ptr_type(type), "unable to call non-callable type {}", type);

    const auto& sig = std::get<type_function_ptr>(type);

    push_value(com.program, op::push_call_frame);
    auto args_size = 2 * sizeof(std::uint64_t);
    for (std::size_t i = 0; i != node.args.size(); ++i) {
        if (!push_function_arg(com, *node.args.at(i), sig.param_types[i], node.token)) {
            node.token.error(
                "Could not convert arg of type '{}' to '{}'",
                type_of_expr(com, *node.args.at(i)),
                sig.param_types[i]
            );
        }
        args_size += com.types.size_of(sig.param_types[i]);
    }

    // push the function pointer and call it
    push_expr_val(com, *node.expr);
    push_value(com.program, op::call, args_size);
    return *sig.return_type;
}

auto push_expr_val(compiler& com, const node_array_expr& node) -> type_name
{
    node.token.assert(!node.elements.empty(), "cannot have empty array literals");

    const auto inner_type = push_object_copy(com, *node.elements.front(), node.token);
    for (const auto& element : node.elements | std::views::drop(1)) {
        const auto element_type = push_object_copy(com, *element, node.token);
        node.token.assert_eq(element_type, inner_type, "array has mismatching element types");
    }
    return concrete_array_type(inner_type, node.elements.size());
}

auto push_expr_val(compiler& com, const node_repeat_array_expr& node) -> type_name
{
    node.token.assert(node.size != 0, "cannot have empty array literals");

    const auto inner_type = type_of_expr(com, *node.value);
    for (std::size_t i = 0; i != node.size; ++i) {
        push_object_copy(com, *node.value, node.token);
    }
    return concrete_array_type(inner_type, node.size);
}

auto push_expr_val(compiler& com, const node_addrof_expr& node) -> type_name
{
    const auto type = push_ptr_underlying(com, *node.expr);
    return concrete_ptr_type(type);
}

auto push_expr_val(compiler& com, const node_sizeof_expr& node) -> type_name
{
    const auto type = type_of_expr(com, *node.expr);

    // References act like aliases, so calling sizeof on a reference returns the size
    // of the inner type. References will not be directly spellable eventually.
    push_value(com.program, op::push_u64, com.types.size_of(type.remove_ref()));
    return u64_type();
}

auto push_expr_val(compiler& com, const node_span_expr& node) -> type_name
{
    if ((node.lower_bound && !node.upper_bound) || (!node.lower_bound && node.upper_bound)) {
        node.token.error("a span must either have both bounds set, or neither");
    }

    const auto [type, is_const, is_ref] = type_of_expr(com, *node.expr).strip_qualifiers();
    node.token.assert(
        is_array_type(type) || is_span_type(type),
        "can only span arrays and other spans, not {}", type
    );

    // Bounds checking (TODO: BOUNDS CHECKING ON SPANS TOO)
    if (is_array_type(type) && com.debug && node.lower_bound && node.upper_bound) {
        const auto lower_bound_type = push_expr_val(com, *node.lower_bound);
        const auto upper_bound_type = push_expr_val(com, *node.upper_bound);
        node.token.assert_eq(lower_bound_type, u64_type(), "subspan indices must be u64");
        node.token.assert_eq(upper_bound_type, u64_type(), "subspan indices must be u64");
        push_value(com.program, op::u64_lt);
        push_assert(com, "lower bound must be stricly less than the upper bound");

        push_expr_val(com, *node.upper_bound);
        push_value(com.program, op::push_u64, array_length(type));
        push_value(com.program, op::u64_lt);
        push_assert(com, "upper bound must be strictly less than the array size");
    }

    if (is_ref) {
        push_expr_val(com, *node.expr);
    } else {
        push_expr_ptr(com, *node.expr);
    }

    // If we are a span, we want the address that it holds rather than its own address,
    // so switch the pointer by loading what it's pointing at.
    if (is_span_type(type)) {
        push_value(com.program, op::load, size_of_ptr());
    }

    if (node.lower_bound) {// move first index of span up
        push_value(com.program, op::push_u64, com.types.size_of(inner_type(type)));
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
    } else if (is_span_type(type)) {
        // Push the span pointer, offset to the size, and load the size
        if (is_ref) {
            push_expr_val(com, *node.expr);
        } else {
            push_expr_ptr(com, *node.expr);
        }
        push_value(com.program, op::push_u64, size_of_ptr(), op::u64_add);
        push_value(com.program, op::load, com.types.size_of(u64_type()));
    } else {
        push_value(com.program, op::push_u64, array_length(type));
    }

    if (is_const && is_array_type(type)) {
        return concrete_span_type(inner_type(type).add_const());
    }
    return concrete_span_type(inner_type(type));
}

auto push_expr_val(compiler& com, const node_new_expr& node) -> type_name
{
    if (!com.scopes.in_unsafe()) {
        node.token.error("Cannot have a 'new' statement outside of an unsafe block");
    }

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
    // If we're taking a reference of an existing reference object, we just return the inner
    // object; in order words we create a new reference to the same underlying object, rather
    // than creating a reference to a reference.
    return push_ptr_underlying(com, *node.expr).add_ref();
}

auto push_expr_val(compiler& com, const node_const_expr& node) -> type_name
{
    return push_expr_val(com, *node.expr).add_const();
}

// If not implemented explicitly, assume that the given node_expr is an lvalue, in which case
// we can load it by pushing the address to the stack and loading.
auto push_expr_val(compiler& com, const auto& node) -> type_name
{
    // This has a bug in it, as it obviously doesn't run copy constructors. But this is
    // needed to implement copy constructors, otherwise you wouldn't be able to return anything!
    // This is quite a large bug, and will require a more robust implementation of construction,
    // and likely move semantics
    const auto type = push_expr_ptr(com, node);
    push_value(com.program, op::load, com.types.size_of(type));
    return type;
}

void push_stmt(compiler& com, const node_sequence_stmt& node)
{
    const auto scope = scope_guard::block(com);
    for (const auto& seq_node : node.sequence) {
        push_stmt(com, *seq_node);
    }
}

void push_stmt(compiler& com, const node_unsafe_stmt& node)
{
    const auto scope = scope_guard::unsafe_block(com);
    for (const auto& seq_node : node.sequence) {
        push_stmt(com, *seq_node);
    }
}

auto push_loop(compiler& com, std::function<void()> body) -> void
{
    const auto loop_scope = scope_guard::loop(com);
    
    const auto begin_pos = com.program.size();
    {
        const auto body_scope = scope_guard::block(com);
        body();
    }
    push_value(com.program, op::jump, begin_pos);

    // Fix up the breaks and continues
    const auto& control_flow = com.scopes.get_loop_info();
    for (const auto idx : control_flow.breaks) {
        write_value(com.program, idx, com.program.size()); // Jump past end
    }
    for (const auto idx : control_flow.continues) {
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
        name := iter[idx]~;
        idx = idx + 1u;
        <body>
    }
}
*/
void push_stmt(compiler& com, const node_for_stmt& node)
{
    const auto scope = scope_guard::block(com);

    const auto iter_type = type_of_expr(com, *node.iter);

    const auto is_array = is_array_type(iter_type);
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
    if (is_array_type(iter_type)) {
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

        // name := iter[idx]~;
        const auto iter_type = type_of_expr(com, *node.iter);
        const auto inner = inner_type(iter_type);
        if (is_rvalue_expr(*node.iter)) {
            push_var_addr(com, node.token, "#:iter");
        } else {
            push_ptr_underlying(com, *node.iter);
            if (is_span_type(iter_type)) {
                push_value(com.program, op::load, size_of_ptr());
            }
        }
        load_variable(com, node.token, "#:idx");
        push_value(com.program, op::push_u64, com.types.size_of(inner));
        push_value(com.program, op::u64_mul);
        push_value(com.program, op::u64_add);
        declare_var(com, node.token, node.name, concrete_reference_type(inner));

        // idx = idx + 1;
        load_variable(com, node.token, "#:idx");
        push_value(com.program, op::push_u64, std::uint64_t{1}, op::u64_add);
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
    tok.assert(com.scopes.in_loop(), "cannot use 'break' outside of a loop");
    destruct_on_break_or_continue(com);
    push_value(com.program, op::jump);
    const auto pos = push_value(com.program, std::uint64_t{0}); // filled in later
    com.scopes.get_loop_info().breaks.insert(pos);
}

void push_stmt(compiler& com, const node_break_stmt& node)
{
    push_break(com, node.token);
}

void push_stmt(compiler& com, const node_continue_stmt& node)
{
    node.token.assert(com.scopes.in_loop(), "cannot use 'continue' outside of a loop");
    destruct_on_break_or_continue(com);
    push_value(com.program, op::jump);
    const auto pos = push_value(com.program, std::uint64_t{0}); // filled in later
    com.scopes.get_loop_info().continues.insert(pos);
}

auto push_stmt(compiler& com, const node_declaration_stmt& node) -> void
{
    const auto is_ref = std::holds_alternative<node_reference_expr>(*node.expr);

    // If this is specifically an expression with a ~ at the end, create a reference to it
    auto type = [&] {
        if (is_ref) {
            return push_expr_val(com, *node.expr);
        }
        return push_object_copy(com, *node.expr, node.token);
    }();

    if (!node.is_const && is_ref && type.remove_ref().is_const()) {
        node.token.error("Tried to declare a non-const value as reference to const, "
                         "use 'let' instead of 'var'");
    }

    // Constness does not propagate to the lhs as it is a new object (may need to be careful
    // with references here). The lhs is only const if the declaration is a 'let'.
    type = type.remove_ref();
    type = type.remove_const();
    if (node.is_const) {
        type = type.add_const();
    }
    if (is_ref) {
        type = type.add_ref();
    }

    declare_var(com, node.token, node.name, type);
}

auto is_assignable(const type_name& lhs, const type_name& rhs) -> bool
{
    return lhs.remove_cr() == rhs.remove_cr() && !lhs.remove_ref().is_const();
}

// TODO: Fix assigning from a ref to a ref (currentl)
void push_stmt(compiler& com, const node_assignment_stmt& node)
{
    const auto rhs = type_of_expr(com, *node.expr);
    const auto lhs = type_of_expr(com, *node.position);
    
    node.token.assert(is_assignable(lhs, rhs), "cannot assign a '{}' to a '{}'", rhs.remove_ref(), lhs.remove_ref());

    if (is_rvalue_expr(*node.expr) || is_type_trivially_copyable(rhs.remove_cr())) {
        push_val_underlying(com, *node.expr);
        push_ptr_underlying(com, *node.position);
        push_value(com.program, op::save, com.types.size_of(lhs));
        return;
    }
    
    if (is_array_type(rhs)) {
        const auto etype = inner_type(rhs);
        const auto inner_size = com.types.size_of(etype);
        const auto params = assign_fn_params(etype);

        const auto assign = get_function(com, etype, "assign", params);
        node.token.assert(assign.has_value(), "{} cannot be assigned", etype);

        for (std::size_t i = 0; i != array_length(rhs); ++i) {
            push_value(com.program, op::push_call_frame);

            push_ptr_underlying(com, *node.position); // i-th element of dst
            push_ptr_adjust(com, i * inner_size);
            push_ptr_underlying(com, *node.expr); // i-th element of src
            push_ptr_adjust(com, i * inner_size);

            push_function_call(com, *assign);
            push_value(com.program, op::pop, std::size_t{1});
        }

        return;
    }

    const auto type = rhs.remove_ref();
    const auto params = assign_fn_params(type);
    const auto assign = get_function(com, type, "assign", params);
    node.token.assert(assign.has_value(), "{} cannot be assigned", type);

    push_value(com.program, op::push_call_frame);
    push_ptr_underlying(com, *node.position);
    push_ptr_underlying(com, *node.expr);
    push_function_call(com, *assign);
    push_value(com.program, op::pop, std::size_t{1});
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
    
    {
        const auto scope = scope_guard::function(com, null_type());

        declare_var(com, tok, "# old_base_ptr", u64_type()); // Store the old base ptr
        declare_var(com, tok, "# old_prog_ptr", u64_type()); // Store the old program ptr
        for (const auto& arg : node_sig.params) {
            const auto type = resolve_type(com, tok, arg.type);
            sig.params.push_back({type});
            declare_var(com, tok, arg.name, type);
        }

        sig.return_type = resolve_type(com, tok, node_sig.return_type);
        com.scopes.get_function_info().return_type = sig.return_type;
        for (const auto& function : com.functions[struct_type][name]) {
            // TODO Remove use of this, use push_function_arg instead using the same trick as with
            // type_of_expr where we compile the actual expression then remove the op codes from
            // the program
            if (are_types_convertible_to(sig.params, function.sig.params)) {
                tok.error("multiple definitions of {}({})", name, format_comma_separated(sig.params));
            }
        }
        com.functions[struct_type][name].emplace_back(sig, begin_pos, tok);

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
    const auto expected = concrete_reference_type(struct_type);
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
    node.token.assert(com.scopes.in_function(), "can only return within functions");
    destruct_on_return(com, &node);
    const auto return_type = push_expr_val(com, *node.return_value);
    node.token.assert_eq(return_type, com.scopes.get_function_info().return_type, "wrong return type");
    push_value(com.program, op::ret, com.types.size_of(return_type));
}

void push_stmt(compiler& com, const node_expression_stmt& node)
{
    // Create the temporary in a new scope, and use that to call it's destructor
    const auto scope = scope_guard::block(com);
    const auto type = push_expr_val(com, *node.expr);
    declare_var(com, node.token, "#:temp", type);
}

void push_stmt(compiler& com, const node_delete_stmt& node)
{
    if (!com.scopes.in_unsafe()) {
        node.token.error("Cannot have a 'delete' statement outside of an unsafe block");
    }

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
    {
        const auto global_scope = scope_guard::global(com);
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
                std::exit(1);
            }
        }
    }

    if (!com.scopes.all().empty()) {
        print(
            "Logic Error: There are {} unhandled scopes at the end of compilation\n",
            com.scopes.all().size()
        );
        std::exit(1);
    }

    auto read_only = std::vector<std::byte>{};
    read_only.reserve(com.read_only_data.size());
    for (char c : com.read_only_data) read_only.push_back(static_cast<std::byte>(c));
    return { com.program, read_only };
}

}