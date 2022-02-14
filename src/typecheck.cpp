#include "typecheck.hpp"
#include "vocabulary.hpp"
#include "type.hpp"

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

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };

struct typecheck_scope
{
    std::unordered_map<std::string, function_signature> functions;
    std::unordered_map<std::string, type>               variables;
};

struct typecheck_context
{
    std::stack<typecheck_scope> scopes;
    anzu::type_store types;
};

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

    if (lhs == make_any() || rhs == make_any()) {
        return make_any();
    }

    if (lhs != rhs) {
        invalid_expr();
    }

    if (lhs == make_list() || lhs == make_null()) { // No support for having these in binary ops.
        invalid_expr();
    }

    if (lhs == make_str()) {
        // Allowed: string concatenation and equality check
        if (op == tk_add) {
            return make_str();
        }
        if (op == tk_eq || op == tk_ne) {
            return make_bool();
        }
        invalid_expr();
    }

    if (lhs == make_bool()) {
        if (op == tk_or || op == tk_and || op == tk_eq || op == tk_ne) {
            return make_bool();
        }
        invalid_expr();
    }

    if (is_comparison(op)) {
        return make_bool();
    }
    return make_int();
}

auto fetch_function_signature(
    const typecheck_context& ctx,
    const token& tok,
    const std::string& function_name
)
    -> function_signature
{
    const auto& scope = ctx.scopes.top();
    if (auto it = scope.functions.find(function_name); it != scope.functions.end()) {
        return it->second;
    }

    if (anzu::is_builtin(function_name)) {
        return anzu::fetch_builtin(function_name).sig;
    }

    type_error(tok, "could not find function '{}'", function_name);
}

auto type_of_expr(const typecheck_context& ctx, const node_expr& expr) -> type
{
    return std::visit(overloaded {
        [&](const node_literal_expr& node) {
            return type_of(node.value);
        },
        [&](const node_variable_expr& node) {
            const auto& top = ctx.scopes.top();
            return top.variables.at(node.name);
        },
        [&](const node_function_call_expr& node) {
            const auto& func_def = fetch_function_signature(
                ctx, node.token, node.function_name
            );
            return func_def.return_type;
        },
        [&](const node_bin_op_expr& node) {
            return type_of_bin_op(
                type_of_expr(ctx, *node.lhs), type_of_expr(ctx, *node.rhs), node.token
            );
        },
        [&](const node_list_expr& node) {
            // For now, empty lists are lists of ints. When we can explicitly state the type when
            // declaring a value, this is a compile time error.
            if (node.elements.empty()) {
                return make_list_of(make_int());
            }
            const auto subtype = type_of_expr(ctx, *node.elements.front());
            for (const auto& subexpr : node.elements | std::views::drop(1)) {
                if (type_of_expr(ctx, *subexpr) != subtype) {
                    type_error(node.token, "list elements must all be the same type\n");
                }
            }
            return make_list_of(subtype);
        }
    }, expr);
};

void verify_expression_type(typecheck_context& ctx, const node_expr& expr, const type& expected)
{
    const auto actual = type_of_expr(ctx, expr);
    if (actual != make_any() && actual != expected) {
        type_error(get_token(expr), "expected '{}', got '{}'", expected, actual);
    }
}

auto typecheck_node(typecheck_context& ctx, const node_stmt& node) -> void;

auto typecheck_node(typecheck_context& ctx, const node_sequence_stmt& node) -> void
{
    for (const auto& child : node.sequence) {
        typecheck_node(ctx, *child);
    }
}

auto typecheck_node(typecheck_context& ctx, const node_while_stmt& node) -> void
{
    verify_expression_type(ctx, *node.condition, make_bool());
    typecheck_node(ctx, *node.body);
}

auto typecheck_node(typecheck_context& ctx, const node_if_stmt& node) -> void
{
    verify_expression_type(ctx, *node.condition, make_bool());
    typecheck_node(ctx, *node.body);
}

auto typecheck_node(typecheck_context& ctx, const node_for_stmt& node) -> void
{
    ctx.scopes.top().variables[node.var] = make_any(); // Can't know type yet :(
    verify_expression_type(ctx, *node.container, make_list());
    typecheck_node(ctx, *node.body);
}

auto typecheck_node(typecheck_context& ctx, const node_break_stmt&) -> void
{
}

auto typecheck_node(typecheck_context& ctx, const node_continue_stmt&) -> void
{
}

auto typecheck_node(typecheck_context& ctx, const node_assignment_stmt& node) -> void
{
    ctx.scopes.top().variables[node.name] = type_of_expr(ctx, *node.expr);
}

auto check_function_ends_with_return(const node_function_def_stmt& node) -> void
{
    // Functions returning null don't need a return statement.
    if (node.sig.return_type == make_null()) {
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

auto typecheck_node(typecheck_context& ctx, const node_function_def_stmt& node) -> void
{
    ctx.scopes.top().functions[node.name] = node.sig; // Make name available in outer scope

    ctx.scopes.emplace();
    for (const auto& arg : node.sig.args) {
        verify_real_type(ctx, node.token, arg.type);
        ctx.scopes.top().variables[arg.name] = arg.type;
    }
    verify_real_type(ctx, node.token, node.sig.return_type);
    ctx.scopes.top().variables[return_key()] = node.sig.return_type; // Expose the return type for children
    ctx.scopes.top().functions[node.name] = node.sig;             // Make available for recursion
    typecheck_node(ctx, *node.body);
    ctx.scopes.pop();

    check_function_ends_with_return(node);
}

auto typecheck_node(typecheck_context& ctx, const node_function_call_stmt& node) -> void
{
    const auto sig = fetch_function_signature(ctx, node.token, node.function_name);
    if (sig.args.size() != node.args.size()) {
        type_error(
            node.token,
            "function '{}' expected {} args, got {}",
            node.function_name, sig.args.size(), node.args.size()
        );
    }

    for (std::size_t idx = 0; idx != sig.args.size(); ++idx) {
        const auto& expected = sig.args.at(idx).type;
        const auto& actual = type_of_expr(ctx, *node.args[idx]);
        if (expected != make_any() && actual != make_any() && expected != actual) {
            type_error(
                node.token,
                "invalid function call, arg {} expects type {}, got {}\n",
                idx, expected, actual
            );
        }
    }
}

auto typecheck_node(typecheck_context& ctx, const node_return_stmt& node)
{
    const auto& return_type = ctx.scopes.top().variables.at(return_key());
    verify_expression_type(ctx, *node.return_value, return_type);
}

auto typecheck_node(typecheck_context& ctx, const node_debug_stmt& node)
{
}

auto typecheck_node(typecheck_context& ctx, const node_stmt& node) -> void
{
    std::visit([&](const auto& n) { typecheck_node(ctx, n); }, node);
}

}

auto type_of(const anzu::object& object) -> type
{
    if (object.is<int>()) {
        return make_int();
    }
    if (object.is<bool>()) {
        return make_bool();
    }
    if (object.is<std::string>()) {
        return make_str();
    }
    if (object.is<object_list>()) {
        const auto& list = object.as<object_list>();
        if (list->empty()) {
            return make_list_of(make_int());
        }
        const auto subtype = type_of(list->front());
        for (const auto& subelem : *list | std::views::drop(1)) {
            if (type_of(subelem) != subtype) {
                anzu::print("WHOOPS! Not a homogeneous list (temporary)\n");
                std::exit(1);
            }
        }
        return make_list_of(subtype);
    }
    if (object.is<object_null>()) {
        return make_null();
    }
    return make_any();
}

auto typecheck_ast(const node_stmt_ptr& ast) -> void
{
    auto ctx = typecheck_context{};
    ctx.scopes.emplace(); // Global scope
    typecheck_node(ctx, *ast);
}

}