#include "compiler.hpp"
#include "lexer.hpp"
#include "object.hpp"
#include "parser.hpp"
#include "functions.hpp"
#include "scope_manager.hpp"
#include "utility/common.hpp"
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

// Struct used to store information while compiling an AST. Contains the output program
// as well as information such as function definitions.
struct compiler
{
    std::vector<std::byte> program;
    std::string            read_only_data;

    bool debug = false;
    std::unordered_map<std::string, function_info> functions;
    type_store types;
    scope_manager scopes;
};

auto push_expr_ptr(compiler& com, const node_expr& node) -> type_name;
auto push_expr_val(compiler& com, const node_expr& expr) -> type_name;
auto push_stmt(compiler& com, const node_stmt& root) -> void;
auto type_of_expr(compiler& com, const node_expr& node) -> type_name;

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

auto are_types_convertible_to(const std::vector<type_name>& args,
                              const std::vector<type_name>& actuals) -> bool;

auto verify_function_call(const function_info& func, const type_names& params, const token& tok) -> void
{
    if (!are_types_convertible_to(params, func.sig.params)) {
        tok.error("tried to call function (TODO - ADD NAME) with wrong signature");
    }
}

auto get_function(
    const compiler& com, const std::string& struct_name, const std::string& function_name
)
    -> std::optional<function_info>
{
    const auto full_name = std::format("{}::{}", struct_name, function_name);
    if (const auto it = com.functions.find(full_name); it != com.functions.end()) {
        return it->second;
    }
    return std::nullopt;
}

auto push_function_call(compiler& com, const function_info& function) -> void
{
    auto args_size = std::size_t{0};
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
    tok.assert(var.has_value(), "could not find variable '{}'\n", name);
    const auto op = var->is_local ? op::push_ptr_local : op::push_ptr_global;
    push_value(com.program, op, var->location);
    return var->type;
}

auto load_variable(compiler& com, const token& tok, const std::string& name) -> void
{
    const auto type = push_var_addr(com, tok, name);
    push_value(com.program, op::load, com.types.size_of(type));
}

auto save_variable(compiler& com, const token& tok, const std::string& name) -> void
{
    const auto type = push_var_addr(com, tok, name);
    push_value(com.program, op::save, com.types.size_of(type));
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

class scope_guard
{
    compiler* d_com;
    scope_guard(compiler& com) : d_com{&com} {}
    scope_guard(const scope_guard&) = delete;
    scope_guard& operator=(const scope_guard&) = delete;

public:
    ~scope_guard() {
        const auto scope_size = d_com->scopes.pop_scope();
        if (scope_size > 0) {
            push_value(d_com->program, op::pop, scope_size);
        }    
    }

    static auto scope(compiler& com) -> scope_guard
    {
        com.scopes.new_scope();
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
        return index;
    }
    const auto ptr = com.read_only_data.size();
    com.read_only_data.append(data);
    return ptr;
}

auto push_assert(compiler& com, std::string_view message) -> void
{
    const auto index = insert_into_rom(com, message);
    push_value(com.program, op::assert, index, message.size());
}

auto push_expr_ptr(compiler& com, const node_name_expr& node) -> type_name
{
    if (auto func = get_function(com, to_string(global_namespace), node.name)) {
        node.token.error("cannot take address of a function pointer");
    }

    return push_var_addr(com, node.token, node.name);
}

// I think this is a bit of a hack; when pushing the value of a function pointer, we need
// to do it in a special way. TODO: I think this messes with the idea that variable nodes
// are lvalues, so that may cause trouble; we should find out how.
auto push_expr_val(compiler& com, const node_name_expr& node) -> type_name
{
    if (auto func = get_function(com, to_string(global_namespace), node.name)) {
        const auto& info = *func;
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
    auto [type, is_const] = push_expr_ptr(com, *node.expr).strip_const();

    // Allow for field access on a pointer
    while (type.is_ptr()) {
        push_value(com.program, op::load, sizeof(std::byte*));
        type = type.remove_ptr();
    }

    auto ret = push_adjust_ptr_to_field(com, node.token, type, node.field_name);
    if (is_const) ret = ret.add_const(); // Propagate const to members
    return ret;
}

auto push_expr_ptr(compiler& com, const node_deref_expr& node) -> type_name
{
    const auto [type, is_const] = push_expr_val(com, *node.expr).strip_const(); // Push the address
    node.token.assert(type.is_ptr(), "cannot use deref operator on non-ptr type '{}'", type);
    return type.remove_ptr();
}

auto push_expr_ptr(compiler& com, const node_subscript_expr& node) -> type_name
{
    const auto expr_type = type_of_expr(com, *node.expr);
    const auto [real_type, is_const] = expr_type.strip_const();

    const auto is_array = real_type.is_array();
    const auto is_span = real_type.is_span();
    node.token.assert(is_array || is_span, "subscript only supported for arrays and spans");

    push_expr_ptr(com, *node.expr);

    // If we are a span, we want the address that it holds rather than its own address,
    // so switch the pointer by loading what it's pointing at.
    if (is_span) {
        push_value(com.program, op::load, sizeof(std::byte*));
    }

    // Bounds checking on the subscript, it's unsigned so only need to check upper bound
    if (com.debug) {
        const auto index = push_expr_val(com, *node.index);
        node.token.assert_eq(index, u64_type(), "subscript argument must be u64, got {}", index);
        if (is_array) {
            push_value(com.program, op::push_u64, array_length(real_type));
        } else {
            push_expr_ptr(com, *node.expr);
            push_value(com.program, op::push_u64, sizeof(std::byte*));
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
    return char_type().add_const().add_span();
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
    auto lhs = push_expr_val(com, *node.lhs);
    auto rhs = push_expr_val(com, *node.rhs);
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
    const auto raw_type = push_expr_val(com, *node.expr);
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

auto get_converter(const type_name& src, const type_name& dst)
    -> void(*)(compiler&, const node_expr&, const token&)
{
    if (src.is_array() && dst.is_span() && src.remove_array() == dst.remove_span()) {
        return [](compiler& com, const node_expr& expr, const token& tok) {
            const auto type = push_expr_ptr(com, expr);
            push_value(com.program, op::push_u64, array_length(type));
        };
    }

    // pointers can convert to pointers-to-const
    if (src.is_ptr() && dst.is_ptr() && src.remove_ptr() == dst.remove_ptr().remove_const()) {
        return [](compiler& com, const node_expr& expr, const token& tok) {
            push_expr_val(com, expr);
        };
    }

    const auto src_raw = src.remove_const();
    const auto dst_raw = dst.remove_const();

    if (src_raw != dst_raw) {
        return nullptr;
    }

    return [](compiler& com, const node_expr& expr, const token& tok) {
        push_expr_val(com, expr);
    };
}

auto push_function_arg(
    compiler& com, const node_expr& expr, const type_name& expected, const token& tok
) -> void
{
    const auto actual = type_of_expr(com, expr);
    const auto converter = get_converter(actual, expected);
    tok.assert(converter != nullptr, "Could not convert arg from '{}' to '{}'", actual, expected);
    (*converter)(com, expr, tok);
}

auto are_types_convertible_to(
    const std::vector<type_name>& args, const std::vector<type_name>& expecteds
)
    -> bool
{
    if (args.size() != expecteds.size()) return false;
    for (const auto& [arg, expected] : zip(args, expecteds)) {
        if (get_converter(arg, expected)) {
            return true;
        }
    }
    return false;
}

auto get_builtin_id(const std::string& name, const std::vector<type_name>& args)
    -> std::optional<std::size_t>
{
    auto index = std::size_t{0};
    for (const auto& b : get_builtins()) {
        if (name == b.name && are_types_convertible_to(args, b.args)) {
            return index;
        }
        ++index;
    }
    return std::nullopt;
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
                push_function_arg(com, *node.args.at(i), expected_params[i], node.token);
            }
            if (node.args.size() == 0) { // if the class has no data, it needs to be size 1
                push_value(com.program, op::push_null);
            }
            return type;
        }

        // Hack to allow for an easy way to dump types of expressions
        if (inner.struct_name == nullptr & inner.name == "__dump_type") {
            std::print("__dump_type(\n");
            for (const auto& arg : node.args) {
                const auto dump = type_of_expr(com, *arg);
                std::print("    {},\n", dump);
            }
            std::print(")\n");
            push_value(com.program, op::push_null);
            return null_type();
        }

        // Second, it might be a function call
        auto params = type_names{};
        params.reserve(node.args.size());
        for (const auto& arg : node.args) {
            params.push_back(type_of_expr(com, *arg));
        }
        
        const auto struct_type = resolve_type(com, node.token, inner.struct_name);
        if (const auto func = get_function(com, to_string(struct_type), inner.name); func) {
            verify_function_call(*func, params, node.token);
            for (std::size_t i = 0; i != node.args.size(); ++i) {
                push_function_arg(com, *node.args.at(i), func->sig.params[i], node.token);
            }
            push_function_call(com, *func);
            return func->sig.return_type;
        }

        // Lastly, it might be a builtin function
        if (const auto b = get_builtin_id(inner.name, params); b.has_value()) {
            const auto& builtin = get_builtin(*b);
            for (std::size_t i = 0; i != builtin.args.size(); ++i) {
                push_function_arg(com, *node.args.at(i), builtin.args[i], node.token);
            }
            push_value(com.program, op::builtin_call, *b);
            return get_builtin(*b).return_type;
        }
    }

    // Otherwise, the expression must be a function pointer.
    const auto type = type_of_expr(com, *node.expr);
    node.token.assert(type.is_function_ptr(), "unable to call non-callable type {}", type);

    const auto& sig = std::get<type_function_ptr>(type);

    auto args_size = std::size_t{0};
    for (std::size_t i = 0; i != node.args.size(); ++i) {
        push_function_arg(com, *node.args.at(i), sig.param_types[i], node.token);
        args_size += com.types.size_of(sig.param_types[i]);
    }

    // push the function pointer and call it
    push_expr_val(com, *node.expr);
    push_value(com.program, op::call, args_size);
    return *sig.return_type;
}

auto push_expr_val(compiler& com, const node_member_call_expr& node) -> type_name
{
    const auto type = type_of_expr(com, *node.expr);

    // Handle .size() calls on arrays
    if (type.is_array() && node.function_name == "size") {
        node.token.assert(node.other_args.empty(), "{}.size() takes no extra arguments", type);
        push_value(com.program, op::push_u64, array_length(type));
        return u64_type();
    }

    // Handle .size() calls on spans
    if (type.is_span() && node.function_name == "size") {
        node.token.assert(node.other_args.empty(), "{}.size() takes no extra arguments", type);
        push_expr_ptr(com, *node.expr); // push pointer to span
        push_value(com.program, op::push_u64, sizeof(std::byte*));
        push_value(com.program, op::u64_add); // offset to the size value
        push_value(com.program, op::load, com.types.size_of(u64_type())); // load the size
        return u64_type();
    }

    const auto stripped_type = [&] {
        auto t = type;
        while (t.is_ptr()) { t = t.remove_ptr(); }
        return t;
    }();

    auto params = std::vector<type_name>{};
    params.push_back(stripped_type.add_ptr());
    for (const auto& arg : node.other_args) {
        params.push_back(type_of_expr(com, *arg));
    }

    const auto func = get_function(com, to_string(stripped_type), node.function_name);
    node.token.assert(func.has_value(), "could not find member function {}::{}", stripped_type, node.function_name);
    verify_function_call(*func, params, node.token);

    auto t = push_expr_ptr(com, *node.expr); // self
    while (t.is_ptr()) { // allow for calling member functions through pointers
        push_value(com.program, op::load, sizeof(std::byte*));
        t = t.remove_ptr();
    }
    for (std::size_t i = 0; i != node.other_args.size(); ++i) {
        push_function_arg(com, *node.other_args.at(i), func->sig.params[i + 1], node.token);
    }
    push_function_call(com, *func);
    return func->sig.return_type;
}

auto push_expr_val(compiler& com, const node_array_expr& node) -> type_name
{
    node.token.assert(!node.elements.empty(), "cannot have empty array literals");

    const auto inner_type = push_expr_val(com, *node.elements.front());
    for (const auto& element : node.elements | std::views::drop(1)) {
        const auto element_type = push_expr_val(com, *element);
        node.token.assert_eq(element_type, inner_type, "array has mismatching element types");
    }
    return inner_type.add_array(node.elements.size());
}

auto push_expr_val(compiler& com, const node_repeat_array_expr& node) -> type_name
{
    node.token.assert(node.size != 0, "cannot have empty array literals");

    const auto inner_type = type_of_expr(com, *node.value);
    for (std::size_t i = 0; i != node.size; ++i) {
        push_expr_val(com, *node.value);
    }
    return inner_type.add_array(node.size);
}

auto push_expr_val(compiler& com, const node_addrof_expr& node) -> type_name
{
    const auto type = push_expr_ptr(com, *node.expr);
    return type.add_ptr();
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

    const auto [type, is_const] = type_of_expr(com, *node.expr).strip_const();
    node.token.assert(
        type.is_array() || type.is_span(),
        "can only span arrays and other spans, not {}", type
    );

    // Bounds checking (TODO: BOUNDS CHECKING ON SPANS TOO)
    if (type.is_array() && com.debug && node.lower_bound && node.upper_bound) {
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

    push_expr_ptr(com, *node.expr);

    // If we are a span, we want the address that it holds rather than its own address,
    // so switch the pointer by loading what it's pointing at.
    if (type.is_span()) {
        push_value(com.program, op::load, sizeof(std::byte*));
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
    } else if (type.is_span()) {
        // Push the span pointer, offset to the size, and load the size
        push_expr_ptr(com, *node.expr);
        push_value(com.program, op::push_u64, sizeof(std::byte*), op::u64_add);
        push_value(com.program, op::load, com.types.size_of(u64_type()));
    } else {
        push_value(com.program, op::push_u64, array_length(type));
    }

    if (is_const && type.is_array()) {
        return type.remove_array().add_const().add_span();
    }
    return type.remove_array().add_span();
}

auto push_expr_val(compiler& com, const node_new_expr& node) -> type_name
{
    if (node.size) {
        const auto count = push_expr_val(com, *node.size);
        node.token.assert_eq(count, u64_type(), "invalid array size type");
        const auto type = resolve_type(com, node.token, node.type);
        push_value(com.program, op::alloc_span, com.types.size_of(type));
        push_expr_val(com, *node.size); // push the size again to make the second half of the span
        return type.add_span();
    }

    const auto type = resolve_type(com, node.token, node.type);
    push_value(com.program, op::alloc_ptr, com.types.size_of(type));
    return type.add_ptr();
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
    const auto scope = scope_guard::scope(com);
    for (const auto& seq_node : node.sequence) {
        push_stmt(com, *seq_node);
    }
}

auto push_loop(compiler& com, std::function<void()> body) -> void
{
    const auto loop_scope = scope_guard::loop(com);
    
    const auto begin_pos = com.program.size();
    {
        const auto body_scope = scope_guard::scope(com);
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

void push_break(compiler& com, const token& tok)
{
    tok.assert(com.scopes.in_loop(), "cannot use 'break' outside of a loop");
    push_value(com.program, op::jump);
    const auto pos = push_value(com.program, std::uint64_t{0}); // filled in later
    com.scopes.get_loop_info().breaks.push_back(pos);
}

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
    const auto scope = scope_guard::scope(com);

    const auto iter_type = type_of_expr(com, *node.iter);

    const auto is_array = iter_type.is_array();
    const auto is_lvalue_span = iter_type.is_span() && is_lvalue_expr(*node.iter);
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
    if (iter_type.is_array()) {
        push_value(com.program, op::push_u64, array_length(iter_type));
        declare_var(com, node.token, "#:size", u64_type());
    } else {
        node.token.assert(is_lvalue_expr(*node.iter), "for-loops only supported for lvalue spans");
        push_expr_ptr(com, *node.iter); // push pointer to span
        push_value(com.program, op::push_u64, sizeof(std::byte*));
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
            push_expr_ptr(com, *node.iter);
            if (iter_type.is_span()) {
                push_value(com.program, op::load, sizeof(std::byte*));
            }
        }
        load_variable(com, node.token, "#:idx");
        push_value(com.program, op::push_u64, com.types.size_of(inner));
        push_value(com.program, op::u64_mul);
        push_value(com.program, op::u64_add);
        declare_var(com, node.token, node.name, inner.add_ptr());

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
    node.token.assert(!com.functions.contains(node.name), "{}", message);

    auto fields = type_fields{};
    for (const auto& p : node.fields) {
        fields.emplace_back(field{ .name=p.name, .type=resolve_type(com, node.token, p.type) });
    }

    com.types.add(make_type(node.name), fields);
    for (const auto& function : node.functions) {
        push_stmt(com, *function);
    }
}

void push_stmt(compiler& com, const node_break_stmt& node)
{
    push_break(com, node.token);
}

void push_stmt(compiler& com, const node_continue_stmt& node)
{
    node.token.assert(com.scopes.in_loop(), "cannot use 'continue' outside of a loop");
    push_value(com.program, op::jump);
    const auto pos = push_value(com.program, std::uint64_t{0}); // filled in later
    com.scopes.get_loop_info().continues.push_back(pos);
}

auto push_stmt(compiler& com, const node_declaration_stmt& node) -> void
{
    const auto type = push_expr_val(com, *node.expr);
    declare_var(com, node.token, node.name, node.add_const ? type.add_const() : type);
}

auto is_assignable(const type_name& lhs, const type_name& rhs) -> bool
{
    return lhs.remove_const() == rhs.remove_const() && !lhs.is_const();
}

void push_stmt(compiler& com, const node_assignment_stmt& node)
{
    const auto rhs = push_expr_val(com, *node.expr);
    const auto lhs = push_expr_ptr(com, *node.position);
    node.token.assert(is_assignable(lhs, rhs), "cannot assign a '{}' to a '{}'", rhs, lhs);
    push_value(com.program, op::save, com.types.size_of(lhs));
    return;
}

auto ends_in_return(const node_stmt& node) -> bool
{
    return std::visit(overloaded{
        [&](const node_sequence_stmt& n) {
            if (n.sequence.empty()) { return false; }
            return ends_in_return(*n.sequence.back());
        },
        [&](const node_if_stmt& n) {
            if (!n.else_body) { return false; } // both branches must exist
            return ends_in_return(*n.body) && ends_in_return(*n.else_body);
        },
        [](const node_return_stmt&) { return true; },
        [](const auto&)             { return false; }
    }, node);
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
    if (get_function(com, to_string(struct_type), name)) {
        tok.error("multiple definitions of function {}", name);
    }

    auto sig = signature{};
    push_value(com.program, op::jump);
    const auto jump_op = push_value(com.program, std::uint64_t{0});
    const auto begin_pos = com.program.size(); // First op code after the jump
    
    {
        const auto scope = scope_guard::function(com, null_type());

        for (const auto& arg : node_sig.params) {
            auto type = resolve_type(com, tok, arg.type);
            sig.params.push_back({type});
            declare_var(com, tok, arg.name, type);
        }

        sig.return_type = resolve_type(com, tok, node_sig.return_type);
        com.scopes.get_function_info().return_type = sig.return_type;
        const auto full_name = std::format("{}::{}", struct_type, name);
        com.functions[full_name] = function_info{.sig=sig, .ptr=begin_pos, .tok=tok};

        push_stmt(com, *body);

        if (!ends_in_return(*body)) {
            // A function returning null does not need a final return statement, and in this case
            // we manually add a return value of null here.
            if (sig.return_type == null_type()) {
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
    const auto sig = compile_function_body(com, node.token, struct_type, node.function_name, node.sig, node.body);

    // First argument must be a pointer to an instance of the class
    node.token.assert(sig.params.size() > 0, "member functions must have at least one arg");
    const auto actual = sig.params.front();
    const auto expected = struct_type.add_const().add_ptr();
    if (!is_assignable(expected, actual)) {
        node.token.error(
            "'{}' bad 1st arg: expected {} or {}, got {}",
            node.function_name,
            struct_type.add_ptr(),
            struct_type.add_const().add_ptr(),
            actual);
    }
}

void push_stmt(compiler& com, const node_return_stmt& node)
{
    node.token.assert(com.scopes.in_function(), "can only return within functions");
    const auto return_type = push_expr_val(com, *node.return_value);
    node.token.assert_eq(return_type, com.scopes.get_function_info().return_type, "wrong return type");
    push_value(com.program, op::ret, com.types.size_of(return_type));
}

void push_stmt(compiler& com, const node_expression_stmt& node)
{
    const auto type = push_expr_val(com, *node.expr);
    push_value(com.program, op::pop, com.types.size_of(type));
}

void push_stmt(compiler& com, const node_delete_stmt& node)
{
    const auto type = type_of_expr(com, *node.expr);
    if (type.is_span()) {
        push_expr_val(com, *node.expr);
        push_value(com.program, op::dealloc_span, com.types.size_of(type.remove_span()));
    } else if (type.is_ptr()) {
        push_expr_val(com, *node.expr);
        push_value(com.program, op::dealloc_ptr, com.types.size_of(type.remove_ptr()));
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

// Temp: remove this for a more efficient function
auto string_replace(
    std::string subject, std::string_view search, std::string_view replace
)
    -> std::string
{
    std::size_t pos = 0;
    while ((pos = subject.find(search, pos)) != std::string::npos) {
         subject.replace(pos, search.length(), replace);
         pos += replace.length();
    }
    return subject;
}

// Temp: remove this for a more efficient function
auto string_split(std::string s, std::string_view delimiter) -> std::vector<std::string>
{
    std::size_t pos_start = 0;
    std::size_t pos_end = 0;
    std::string token;
    std::vector<std::string> res;

    while ((pos_end = s.find(delimiter, pos_start)) != std::string::npos) {
        token = s.substr(pos_start, pos_end - pos_start);
        pos_start = pos_end + delimiter.length();
        res.push_back(token);
    }

    res.push_back(s.substr(pos_start));
    return res;
}

auto push_print_fundamental(compiler& com, const node_expr& node, const token& tok) -> void
{
    const auto type = push_expr_val(com, node).remove_const();
    if (type == null_type()) { push_value(com.program, op::print_null); }
    else if (type == bool_type()) { push_value(com.program, op::print_bool); }
    else if (type == char_type()) { push_value(com.program, op::print_char); }
    else if (type == i32_type()) { push_value(com.program, op::print_i32); }
    else if (type == i64_type()) { push_value(com.program, op::print_i64); }
    else if (type == u64_type()) { push_value(com.program, op::print_u64); }
    else if (type == f64_type()) { push_value(com.program, op::print_f64); }
    else if (type == char_type().add_const().add_span() || type == char_type().add_span()) {
        push_value(com.program, op::print_char_span);
    }
    else { tok.error("Cannot print value of type {}", type); }
}

void push_stmt(compiler& com, const node_print_stmt& node)
{
    const auto parts = string_split(string_replace(node.message, "\\n", "\n"), "{}");
    if (parts.size() != node.args.size() + 1) {
        node.token.error("Not enough args to fill all placeholders");
    }

    if (!parts.front().empty()) {
        push_value(com.program, op::push_string_literal);
        push_value(com.program, insert_into_rom(com, parts.front()), parts.front().size());
        push_value(com.program, op::print_char_span);
    }
    for (std::size_t i = 0; i != node.args.size(); ++i) {
        push_print_fundamental(com, *node.args.at(i), node.token);

        if (!parts[i+1].empty()) {
            push_value(com.program, op::push_string_literal);
            push_value(com.program, insert_into_rom(com, parts[i+1]), parts[i+1].size());
            push_value(com.program, op::print_char_span);
        }
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
        const auto global_scope = scope_guard::scope(com);
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
                    std::print("    {}\n", curr.lexically_relative(main_dir).string());
                    push_stmt(com, *mod.root);
                    done.emplace(curr);
                    return true;
                }
                return false;
            });
            const auto after = remaining.size();
            if (before == after) {
                std::print("Cyclic dependency detected among the following files:");
                for (const auto& mod : remaining) {
                    std::print(" {}", mod.lexically_relative(main_dir).string());
                }
                std::print("\n");
                std::exit(1);
            }
        }
    }

    if (com.scopes.size() > 0) {
        panic(
            "Logic Error: There are {} unhandled scopes at the end of compilation",
            com.scopes.size()
        );
    }

    return { com.program, com.read_only_data };
}

}