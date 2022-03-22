#include "typecheck.hpp"
#include "vocabulary.hpp"
#include "operators.hpp"
#include "type.hpp"
#include "utility/overloaded.hpp"

#include <algorithm>
#include <ranges>
#include <unordered_map>
#include <stack>

namespace anzu {
namespace {

template <typename... Args>
[[noreturn]] void type_error(const token& tok, std::string_view msg, Args&&... args)
{
    const auto formatted_msg = std::format(msg, std::forward<Args>(args)...);
    anzu::print("[ERROR] ({}:{}) {}\n", tok.line, tok.col, formatted_msg);
    std::exit(1);
}

auto return_key() -> std::string
{
    static const auto ret = std::string{tk_return};
    return ret;
}

// TODO: Clean this up
struct typecheck_context
{
    std::unordered_map<std::string, const node_function_def_stmt*> functions;

    using var_types = std::unordered_map<std::string, type_name>;

    var_types globals;
    std::optional<var_types> locals;

    std::unordered_map<const node_function_def_stmt*, std::unordered_set<signature>> checked_sigs;

    type_info types;
};

auto current_vars(typecheck_context& ctx) -> typecheck_context::var_types&
{
    return ctx.locals ? *ctx.locals : ctx.globals;
}

auto declare_var(
    typecheck_context& ctx, const token& tok, const std::string& name, const type_name& type
)
    -> void
{
    auto& vars = current_vars(ctx);
    if (auto it = vars.find(name); it != vars.end()) {
        if (it->second != type) {
            type_error(tok, "cannot assign to '{}', incorrect type", name);
        }
    } else {
        vars.emplace(name, type);
    }
}

auto typecheck_node(typecheck_context& ctx, const node_stmt& node) -> void;
auto typecheck_expr(typecheck_context& ctx, const node_expr& expr) -> type_name;

auto get_token(const node_stmt& node) -> token
{
    return std::visit([](const auto& n) { return n.token; }, node);
}

auto get_token(const node_expr& node) -> token
{
    return std::visit([](const auto& n) { return n.token; }, node);
}

auto verify_real_type(typecheck_context& ctx, const token& tok, const type_name& t) -> void
{
    if (!ctx.types.types.is_registered_type(t)) {
        type_error(tok, "'{}' is not a recognised type", t);
    }
}

auto type_of_bin_op(const type_name& lhs, const type_name& rhs, const token& op_token) -> type_name
{
    const auto op = op_token.text;
    const auto info = resolve_bin_op({ .op = op, .lhs = lhs, .rhs = rhs });
    if (!info) {
        type_error(op_token, "could not evaluate '{} {} {}'", lhs, op, rhs);
    };
    return info->result_type;
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
            node.name, node.sig.return_type
        );
    }
    return is_generic;
}

auto fetch_function_def(
    const typecheck_context& ctx, const token& tok, const std::string& function_name
)
    -> const node_function_def_stmt*
{
    if (auto it = ctx.functions.find(function_name); it != ctx.functions.end()) {
        return it->second;
    }
    type_error(tok, "could not find function '{}'", function_name);
}

auto fetch_function_signature(
    const typecheck_context& ctx, const token& tok, const std::string& function_name
)
    -> signature
{
    if (auto it = ctx.functions.find(function_name); it != ctx.functions.end()) {
        return it->second->sig;
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
    auto matches = std::unordered_map<int, type_name>{};

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
    ctx.locals.emplace();
    for (const auto& arg : sig.args) {
        verify_real_type(ctx, node.token, arg.type);
        ctx.locals->emplace(arg.name, arg.type);
    }
    verify_real_type(ctx, node.token, sig.return_type);
    ctx.locals->emplace(return_key(), sig.return_type); // Expose return type for children
 
    typecheck_node(ctx, *node.body);
    ctx.locals.reset();

    check_function_ends_with_return(node);
}

auto typecheck_function_call(
    typecheck_context& ctx,
    const token& tok,
    const std::string& function_name,
    const std::vector<node_expr_ptr>& args
)
    -> type_name
{
    // If this is a type name, its a constructor call
    const auto as_type_name = make_type(function_name);
    print("is {} a registered type? {}\n", function_name, ctx.types.types.is_registered_type(as_type_name));
    if (ctx.types.types.is_registered_type(as_type_name)) {
        const auto fields = ctx.types.types.get_fields(as_type_name);
        if (fields.size() != args.size()) {
            type_error(
                tok,
                "Invalid number of args for {} constructor, expected {} got {}\n",
                function_name, fields.size(), args.size()
            );
        }
        for (std::size_t i = 0; i != args.size(); ++i) {
            const auto given_type = typecheck_expr(ctx, *args[i]);
            if (fields[i].type != given_type) {
                type_error(
                    tok,
                    "Invalid type at position {} for {} constructor, expected {} got {}\n",
                    i, function_name, fields[i].type, given_type
                );
            }
        }
        return as_type_name;
    }

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

auto typecheck_expr(typecheck_context& ctx, const node_expr& expr) -> type_name
{
    const auto expr_type = std::visit(overloaded {
        [&](const node_literal_expr& node) {
            return node.value.type;
        },
        [&](const node_variable_expr& node) {
            if (ctx.locals) {
                if (auto it = ctx.locals->find(node.name); it != ctx.locals->end()) {
                    return it->second;
                }
            }
            if (auto it = ctx.globals.find(node.name); it != ctx.globals.end()) {
                return it->second;
            }
            type_error(node.token, "could not find variable '{}'\n", node.name);
        },
        [&](const node_field_expr& node) {
            const auto parent_type = typecheck_expr(ctx, *node.expression);
            const auto fields = ctx.types.types.get_fields(parent_type);
            for (const auto& field : fields) {
                if (field.name == node.field_name) {
                    return field.type;
                }
            }
            type_error(node.token, "type '{}' has no field named '{}'\n", parent_type, node.field_name);
            return int_type();
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

    ctx.types.expr_types.emplace(&expr, expr_type);
    return expr_type;
};

void verify_expression_type(typecheck_context& ctx, const node_expr& expr, const type_name& expected)
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

auto typecheck_node(typecheck_context& ctx, const node_struct_stmt& node) -> void
{
    if (ctx.types.types.is_registered_type(node.name)) {
        type_error(node.token, "type '{}' is already defined\n", node.name);
    }
    for (const auto& field : node.fields) {
        if (!ctx.types.types.is_registered_type(field.type)) {
            type_error(
                node.token,
                "unknown type '{}' of field {} for struct {}\n",
                field.type, field.name, node.name
            );
        }
    }
    ctx.types.types.register_type(node.name, node.fields);
}

auto typecheck_node(typecheck_context& ctx, const node_for_stmt& node) -> void
{
    const auto container_type = typecheck_expr(ctx, *node.container);
    const auto expected_type = generic_list_type();
    auto matches = match(container_type, expected_type);
    if (!matches.has_value()) {
        type_error(get_token(*node.container), "expected '{}', got '{}'", expected_type, container_type);
    }
    declare_var(ctx, node.token, node.var, matches->at(0));
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
    if (current_vars(ctx).contains(node.name)) {
        type_error(node.token, "cannot declare variable '{}', name already in use", node.name);
    }
    current_vars(ctx).emplace(node.name, typecheck_expr(ctx, *node.expr));
}

auto typecheck_node(typecheck_context& ctx, const node_assignment_stmt& node) -> void
{
    const auto expr_type = typecheck_expr(ctx, *node.expr);
    if (ctx.locals) {
        if (auto it = ctx.locals->find(node.name); it != ctx.locals->end()) {
            if (expr_type != it->second) {
                type_error(node.token, "cannot assign to '{}', incorrect type", node.name);
            }
            return;
        }
    }

    if (auto it = ctx.globals.find(node.name); it != ctx.globals.end()) {
        if (expr_type != it->second) {
            type_error(node.token, "cannot assign to '{}', incorrect type", node.name);
        }
        return;
    }

    type_error(node.token, "cannot assign to '{}', name not declared", node.name);
}

auto typecheck_node(typecheck_context& ctx, const node_field_assignment_stmt& node) -> void
{
    if (ctx.locals) {
        if (auto it = ctx.locals->find(node.name); it != ctx.locals->end()) {
            auto type = it->second;
            for (const auto& field_name : node.fields) {
                for (const auto& field : ctx.types.types.get_fields(type)) {
                    if (field.name == field_name) {
                        type = field.type;
                        break;
                    }
                    type_error(node.token, "type '{}' has no field '{}\n", type, field_name);
                }
            }

            const auto rhs_type = typecheck_expr(ctx, *node.expr);
            if (rhs_type != type) {
                type_error(
                    node.token, "(field assignment) tried to assign '{}' to '{}'\n", rhs_type, type
                );
            }
            return;
        }
    }
    if (auto it = ctx.globals.find(node.name); it != ctx.globals.end()) {
        auto type = it->second;
        for (const auto& field_name : node.fields) {
            for (const auto& field : ctx.types.types.get_fields(type)) {
                if (field.name == field_name) {
                    type = field.type;
                    break;
                }
                type_error(node.token, "type '{}' has no field '{}\n", type, field_name);
            }
        }

        const auto rhs_type = typecheck_expr(ctx, *node.expr);
        if (rhs_type != type) {
            type_error(
                node.token, "(field assignment) tried to assign '{}' to '{}'\n", rhs_type, type
            );
        }
        return;
    }
    print("typechecking for node field assignment not implemented\n");
}

auto typecheck_node(typecheck_context& ctx, const node_function_def_stmt& node) -> void
{
    ctx.functions.emplace(node.name, &node);

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
    if (!ctx.locals) {
        type_error(node.token, "return statements can only be within functions");
    }
    const auto& return_type = ctx.locals->at(return_key());
    verify_expression_type(ctx, *node.return_value, return_type);
}

auto typecheck_node(typecheck_context& ctx, const node_stmt& node) -> void
{
    std::visit([&](const auto& n) { typecheck_node(ctx, n); }, node);
}

}

auto typecheck_ast(const node_stmt_ptr& ast) -> type_info
{
    auto ctx = typecheck_context{};
    typecheck_node(ctx, *ast);
    print("is vec3 registered? {}\n", ctx.types.types.is_registered_type(make_type("vec3")));
    return ctx.types;
}

}