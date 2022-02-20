#include "typecheck.hpp"
#include "vocabulary.hpp"
#include "type.hpp"
#include "utility/overloaded.hpp"

#include <algorithm>
#include <ranges>
#include <unordered_map>
#include <stack>

namespace anzu {
namespace {

auto return_key() -> std::string
{
    static const auto ret = std::string{tk_return};
    return ret;
}

struct typecheck_scope
{
    std::unordered_map<std::string, const node_function_def_stmt*> functions;
    std::unordered_map<std::string, type>                          variables;
};

struct typecheck_context
{
    std::vector<typecheck_scope> scopes;
    anzu::type_store types;

    std::unordered_map<const node_function_def_stmt*, std::unordered_set<signature>> checked_sigs;
};

auto typecheck_node(typecheck_context& ctx, const node_stmt& node) -> void;
auto typecheck_expr(typecheck_context& ctx, const node_expr& expr) -> type;

auto get_token(const node_stmt& node) -> token
{
    return std::visit([](const auto& n) { return n.token; }, node);
}

auto get_token(const node_expr& node) -> token
{
    return std::visit([](const auto& n) { return n.token; }, node);
}

template <typename... Args>
[[noreturn]] void type_error(const token& tok, std::string_view msg, Args&&... args)
{
    const auto formatted_msg = std::format(msg, std::forward<Args>(args)...);
    anzu::print("[ERROR] ({}:{}) {}\n", tok.line, tok.col, formatted_msg);
    std::exit(1);
}

auto verify_real_type(typecheck_context& ctx, const token& tok, const type& t) -> void
{
    if (!ctx.types.is_registered_type(t)) {
        type_error(tok, "'{}' is not a recognised type", t);
    }
}

auto type_of_bin_op(const type& lhs, const type& rhs, const token& op_token) -> type
{
    const auto op = op_token.text;
    const auto invalid_expr = [=]() {
        type_error(op_token, "could not evaluate '{} {} {}'", lhs, op, rhs);
    };

    if (lhs != rhs) {
        invalid_expr();
    }

    if (match(lhs, generic_list_type()).has_value()) {// No support for having these in binary ops.
        invalid_expr();
    }

    if (lhs == null_type()) { // No support for having these in binary ops.
        invalid_expr();
    }

    if (lhs == str_type()) {
        // Allowed: string concatenation and equality check
        if (op == tk_add) {
            return str_type();
        }
        if (op == tk_eq || op == tk_ne) {
            return bool_type();
        }
        invalid_expr();
    }

    if (lhs == bool_type()) {
        if (op == tk_or || op == tk_and || op == tk_eq || op == tk_ne) {
            return bool_type();
        }
        invalid_expr();
    }

    if (lhs == int_type()) {
        if (is_comparison(op)) {
            return bool_type();
        }
        return int_type();
    }

    invalid_expr();
    return int_type(); // Unreachable
}

// Returns true if any of the parameters to the function are incomplete. If none of the
// paraneters are incomplete but the return type is, an error is raised.
auto is_function_generic(const node_function_def_stmt& node) -> bool
{
    const auto& args = node.sig.args;
    const auto is_generic = std::any_of(begin(args), end(args), [](const auto& arg) {
        return !is_type_complete(arg.type);
    });
    if (!is_generic && !is_type_complete(node.sig.return_type)) {
        type_error(
            node.token,
            "function '{}' has incomplete return type '{}' but no incomplete parameter",
            node.name, to_string(node.sig.return_type)
        );
    }
    return is_generic;
}

auto fetch_function_def(
    const typecheck_context& ctx, const token& tok, const std::string& function_name
)
    -> const node_function_def_stmt*
{
    for (const auto& scope : ctx.scopes | std::views::reverse) {
        if (auto it = scope.functions.find(function_name); it != scope.functions.end()) {
            return it->second;
        }
    }

    type_error(tok, "could not find function '{}'", function_name);
}

auto fetch_function_signature(
    const typecheck_context& ctx, const token& tok, const std::string& function_name
)
    -> signature
{
    for (const auto& scope : ctx.scopes | std::views::reverse) {
        if (auto it = scope.functions.find(function_name); it != scope.functions.end()) {
            return it->second->sig;
        }
    }

    if (anzu::is_builtin(function_name)) {
        return anzu::fetch_builtin(function_name).sig;
    }

    type_error(tok, "could not find function '{}'", function_name);
}

auto check_function_ends_with_return(const node_function_def_stmt& node) -> void
{
    // Functions returning null don't need a return statement.
    if (node.sig.return_type == null_type()) {
        return;
    }

    const auto bad_function = [&]() {
        type_error(node.token, "function '{}' does not end in a return statement\n", node.name);
    };

    const auto& body = *node.body;
    if (std::holds_alternative<node_return_stmt>(body)) {
        return;
    }

    if (std::holds_alternative<node_sequence_stmt>(body)) {
        const auto& seq = std::get<node_sequence_stmt>(body).sequence;
        if (seq.empty() || !std::holds_alternative<node_return_stmt>(*seq.back())) {
            bad_function();
        }
    }
    else {
        bad_function();
    }
}

// Given a function, fetch its signature and verify that it can be invoked with the given
// args. If any of the parameters are generic, their types are matched and used to deduce
// the return type (if generic). Returns the signature with any generic types bound.
auto get_typechecked_signature(
    typecheck_context& ctx,
    const token& tok,
    const std::string& function_name,
    const std::vector<node_expr_ptr>& args
)
    -> signature
{
    const auto sig = fetch_function_signature(ctx, tok, function_name);

    if (sig.args.size() != args.size()) {
        type_error(
            tok,
            "function '{}' expected {} args, got {}",
            function_name, sig.args.size(), args.size()
        );
    }

    auto ret_sig = signature{};
    auto matches = std::unordered_map<int, type>{};

    auto ait = args.begin();
    auto sit = sig.args.begin();
    for (; ait != args.end(); ++ait, ++sit) {
        const auto actual_type = typecheck_expr(ctx, **ait);
        const auto pattern_type = sit->type;
        const auto arg_match = match(actual_type, pattern_type);
        if (!arg_match.has_value()) {
            type_error(tok, "'{}' does not match '{}'", actual_type, pattern_type);
        }
        for (const auto& [key, type] : arg_match.value()) {
            if (auto it = matches.find(key); it != matches.end()) {
                if (it->second != type) {
                    type_error(tok, "bad function call (WIP, make error better)");
                }
            }
            else {
                matches.emplace(key, type);
            }
        }
        auto arg = signature::arg{};
        arg.name = sit->name;
        arg.type = actual_type;
        ret_sig.args.push_back(arg);
    }

    ret_sig.return_type = bind_generics(sig.return_type, matches);
    if (!is_type_complete(ret_sig.return_type)) {
        type_error(tok, "could not deduce return type for '{}'", function_name);
    }
    return ret_sig;
}

auto typecheck_function_body_with_signature(
    typecheck_context& ctx,
    const node_function_def_stmt& node,
    const signature& sig
)
    -> void
{
    ctx.scopes.emplace_back();
    for (const auto& arg : sig.args) {
        verify_real_type(ctx, node.token, arg.type);
        ctx.scopes.back().variables[arg.name] = arg.type;
    }
    verify_real_type(ctx, node.token, sig.return_type);
    ctx.scopes.back().variables[return_key()] = sig.return_type; // Expose the return type for children
 
    typecheck_node(ctx, *node.body);
    ctx.scopes.pop_back();

    check_function_ends_with_return(node);
}

auto typecheck_function_call(
    typecheck_context& ctx,
    const token& tok,
    const std::string& function_name,
    const std::vector<node_expr_ptr>& args
)
    -> type
{
    const auto signature = get_typechecked_signature(ctx, tok, function_name, args);

    if (!is_builtin(function_name)) {
        const auto* function_def = fetch_function_def(ctx, tok, function_name);
        if (is_function_generic(*function_def)) {
            auto& checked_sigs = ctx.checked_sigs[function_def];
            if (checked_sigs.contains(signature)) {
                checked_sigs.insert(signature);
                typecheck_function_body_with_signature(ctx, *function_def, signature);
            }
        }
    }

    return signature.return_type;
}

auto typecheck_expr(typecheck_context& ctx, const node_expr& expr) -> type
{
    return std::visit(overloaded {
        [&](const node_literal_expr& node) {
            return type_of(node.value);
        },
        [&](const node_variable_expr& node) {
            for (const auto& scope : ctx.scopes | std::views::reverse) {
                if (auto it = scope.variables.find(node.name); it != scope.variables.end()) {
                    return it->second;
                }
            }
            type_error(node.token, "could not find variable '{}'\n", node.name);
        },
        [&](const node_function_call_expr& node) {
            return typecheck_function_call(ctx, node.token, node.function_name, node.args);
        },
        [&](const node_bin_op_expr& node) {
            return type_of_bin_op(
                typecheck_expr(ctx, *node.lhs), typecheck_expr(ctx, *node.rhs), node.token
            );
        },
        [&](const node_list_expr& node) {
            // For now, empty lists are lists of ints. When we can explicitly state the type when
            // declaring a value, this is a compile time error.
            if (node.elements.empty()) {
                return concrete_list_type(int_type());
            }
            const auto subtype = typecheck_expr(ctx, *node.elements.front());
            for (const auto& subexpr : node.elements | std::views::drop(1)) {
                if (typecheck_expr(ctx, *subexpr) != subtype) {
                    type_error(node.token, "list elements must all be the same type\n");
                }
            }
            return concrete_list_type(subtype);
        }
    }, expr);
};

void verify_expression_type(typecheck_context& ctx, const node_expr& expr, const type& expected)
{
    const auto actual = typecheck_expr(ctx, expr);
    if (!match(actual, expected)) {
        type_error(get_token(expr), "expected '{}', got '{}'", expected, actual);
    }
}

auto typecheck_node(typecheck_context& ctx, const node_sequence_stmt& node) -> void
{
    for (const auto& child : node.sequence) {
        typecheck_node(ctx, *child);
    }
}

auto typecheck_node(typecheck_context& ctx, const node_while_stmt& node) -> void
{
    verify_expression_type(ctx, *node.condition, bool_type());
    typecheck_node(ctx, *node.body);
}

auto typecheck_node(typecheck_context& ctx, const node_if_stmt& node) -> void
{
    verify_expression_type(ctx, *node.condition, bool_type());
    typecheck_node(ctx, *node.body);
    if (node.else_body) {
        typecheck_node(ctx, *node.else_body);
    }
}

auto typecheck_node(typecheck_context& ctx, const node_for_stmt& node) -> void
{
    const auto container_type = typecheck_expr(ctx, *node.container);
    const auto expected_type = generic_list_type();
    auto matches = match(container_type, expected_type);
    if (!matches.has_value()) {
        type_error(get_token(*node.container), "expected '{}', got '{}'", expected_type, container_type);
    }
    ctx.scopes.back().variables[node.var] = matches->at(0);
    typecheck_node(ctx, *node.body);
}

auto typecheck_node(typecheck_context& ctx, const node_break_stmt&) -> void
{
}

auto typecheck_node(typecheck_context& ctx, const node_continue_stmt&) -> void
{
}

auto typecheck_node(typecheck_context& ctx, const node_declaration_stmt& node) -> void
{
    if (ctx.scopes.back().variables.contains(node.name)) {
        type_error(node.token, "cannot declare variable '{}', name already in use", node.name);
    }
    ctx.scopes.back().variables[node.name] = typecheck_expr(ctx, *node.expr);
}

auto typecheck_node(typecheck_context& ctx, const node_assignment_stmt& node) -> void
{
    auto it = ctx.scopes.back().variables.find(node.name);
    if (it == ctx.scopes.back().variables.end()) {
        type_error(node.token, "cannot assign to '{}', name not declared", node.name);
    }

    const auto expr_type = typecheck_expr(ctx, *node.expr);
    if (expr_type != it->second) {
        type_error(node.token, "cannot assign to '{}', incorrect type", node.name);
    }

    ctx.scopes.back().variables[node.name] = expr_type;
}

auto typecheck_node(typecheck_context& ctx, const node_function_def_stmt& node) -> void
{
    ctx.scopes.back().functions[node.name] = &node; // Make name available in outer scope

    // If this is a generic function, we cannot perform type checking on it here.
    // Instead, store the name, and type check it at the function call sites.
    if (is_function_generic(node)) {
        ctx.checked_sigs[&node] = {};
    }
    else {
        typecheck_function_body_with_signature(ctx, node, node.sig);
    }

}

auto typecheck_node(typecheck_context& ctx, const node_function_call_stmt& node) -> void
{
    typecheck_function_call(ctx, node.token, node.function_name, node.args);
}

auto typecheck_node(typecheck_context& ctx, const node_return_stmt& node)
{
    const auto& return_type = ctx.scopes.back().variables.at(return_key());
    verify_expression_type(ctx, *node.return_value, return_type);
}

auto typecheck_node(typecheck_context& ctx, const node_stmt& node) -> void
{
    std::visit([&](const auto& n) { typecheck_node(ctx, n); }, node);
}

}

auto type_of(const anzu::object& object) -> type
{
    if (object.is<int>()) {
        return int_type();
    }
    if (object.is<bool>()) {
        return bool_type();
    }
    if (object.is<std::string>()) {
        return str_type();
    }
    if (object.is<object_list>()) {
        const auto& list = object.as<object_list>();
        if (list->empty()) {
            return concrete_list_type(int_type());
        }
        const auto subtype = type_of(list->front());
        for (const auto& subelem : *list | std::views::drop(1)) {
            if (type_of(subelem) != subtype) {
                anzu::print("WHOOPS! Not a homogeneous list (temporary)\n");
                std::exit(1);
            }
        }
        return concrete_list_type(subtype);
    }
    if (object.is<object_null>()) {
        return null_type();
    }
    anzu::print("WHOOPS! Unknown type\n");
    std::exit(1);
    return null_type();
}

auto typecheck_ast(const node_stmt_ptr& ast) -> void
{
    auto ctx = typecheck_context{};
    ctx.scopes.emplace_back(); // Global scope
    typecheck_node(ctx, *ast);
}

}