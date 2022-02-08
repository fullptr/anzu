#include "parser.hpp"
#include "functions.hpp"
#include "typecheck.hpp"
#include "vocabulary.hpp"

#include <optional>
#include <unordered_set>
#include <string_view>
#include <vector>
#include <memory>
#include <unordered_map>
#include <string>
#include <stack>

namespace anzu {
namespace {

template <typename... Args>
[[noreturn]] void parser_error(const token& tok, std::string_view msg, Args&&... args)
{
    const auto formatted_msg = std::format(msg, std::forward<Args>(args)...);
    anzu::print("[ERROR] ({}:{}) {}\n", tok.line, tok.col, formatted_msg);
    std::exit(1);
}

void add_variable(parser_context& ctx, std::string_view name, std::string_view type)
{
    ctx.current_scope().variables[std::string{name}] = std::string{type};
}

auto handle_list_literal(parser_context& ctx) -> anzu::object;

auto try_parse_literal(parser_context& ctx) -> std::optional<anzu::object>
{
    auto& tokens = ctx.tokens;
    if (tokens.curr().type == token_type::number) {
        return { anzu::to_int(tokens.consume().text) };
    }
    if (tokens.curr().type == token_type::string) {
        return { tokens.consume().text };
    }
    if (tokens.consume_maybe(tk_true)) {
        return { true };
    }
    if (tokens.consume_maybe(tk_false)) {
        return { false };
    }
    if (tokens.consume_maybe(tk_null)) {
        return anzu::null_object();
    }
    if (tokens.consume_maybe(tk_lbracket)) {
        return { handle_list_literal(ctx) };
    }
    return std::nullopt;
};

auto handle_list_literal(parser_context& ctx) -> anzu::object
{
    auto list = std::make_shared<std::vector<anzu::object>>();
    ctx.tokens.consume_comma_separated_list(tk_rbracket, [&] {
        auto obj = try_parse_literal(ctx);
        if (!obj.has_value()) {
            parser_error(ctx.tokens.curr(), "failed to parse string literal");
        }
        list->push_back(obj.value());
    });
    return { list };
}

auto precedence_table()
{
    auto table = std::array<std::unordered_set<std::string_view>, 6>{};
    table[0] = {};
    table[1] = {tk_mul, tk_div, tk_mod};
    table[2] = {tk_add, tk_sub};
    table[3] = {tk_lt, tk_le, tk_gt, tk_ge, tk_eq, tk_ne};
    table[4] = {tk_and};
    table[5] = {tk_or};
    return table;
}
static const auto bin_ops_table = precedence_table();

auto parse_expression(parser_context& ctx) -> node_expr_ptr;
auto parse_function_call_expr(parser_context& ctx) -> node_expr_ptr;

auto parse_single_factor(parser_context& ctx) -> node_expr_ptr
{
    auto& tokens = ctx.tokens;
    if (ctx.tokens.consume_maybe(tk_lparen)) {
        auto expr = parse_expression(ctx);
        ctx.tokens.consume_only(tk_rparen);
        return expr;
    }  
    else if (auto factor = anzu::try_parse_literal(ctx); factor.has_value()) {
        auto node = std::make_unique<anzu::node_expr>();
        auto& expr = node->emplace<anzu::node_literal_expr>();
        expr.value = *factor;
        return node;
    }
    else if (tokens.peek_next(tk_lparen)) {
        return parse_function_call_expr(ctx);
    }
    else if (tokens.curr().type != token_type::name) {
        parser_error(ctx.tokens.curr(), "'{}' is not a name, cannot be used in an expresion", tokens.curr().text);
    }
    auto node = std::make_unique<anzu::node_expr>();
    auto& expr = node->emplace<anzu::node_variable_expr>();
    expr.name = tokens.consume().text;
    return node;
}

// Level is the precendence level, the lower the number, the tighter the factors bind.
auto parse_compound_factor(parser_context& ctx, std::int64_t level) -> node_expr_ptr
{
    if (level == 0) {
        return parse_single_factor(ctx);
    }

    auto left = parse_compound_factor(ctx, level - 1);
    while (ctx.tokens.valid() && bin_ops_table[level].contains(ctx.tokens.curr().text)) {
        auto op = ctx.tokens.consume();

        auto node = std::make_unique<anzu::node_expr>();
        auto& expr = node->emplace<anzu::node_bin_op_expr>();
        expr.lhs = std::move(left);
        expr.op = op;
        expr.rhs = parse_compound_factor(ctx, level - 1);

        left = std::move(node);
    }
    return left;
}

auto parse_expression(parser_context& ctx) -> node_expr_ptr
{
    return parse_compound_factor(ctx, std::ssize(bin_ops_table) - 1i64);
}

auto parse_expression_type_checked(parser_context& ctx, std::string_view exp_type) -> node_expr_ptr
{
    auto& tok = ctx.tokens.curr();
    auto expr = parse_expression(ctx);
    auto type = type_of_expr(ctx, *expr);
    if (type != tk_any && exp_type != type) {
        parser_error(tok, "expected '{}', got '{}'", exp_type, type);
    }
    return expr;
}

auto parse_expression_store_type(parser_context& ctx, std::string_view name) -> node_expr_ptr
{
    auto expr = parse_expression(ctx);
    auto type = type_of_expr(ctx, *expr);
    add_variable(ctx, name, type);
    return expr;
}

auto parse_assigment_stmt(parser_context& ctx) -> node_stmt_ptr
{
    if (ctx.tokens.curr().type != token_type::name) {
        parser_error(ctx.tokens.curr(), "'{}' is not a valid name", ctx.tokens.curr().text);
    }
    auto name = ctx.tokens.consume().text;
    ctx.tokens.consume_only(tk_assign);

    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_assignment_stmt>();
    stmt.name = name;
    stmt.expr = parse_expression_store_type(ctx, stmt.name);
    return node;
}

auto parse_statement(parser_context& ctx) -> node_stmt_ptr;
auto parse_statement_list(parser_context& ctx) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_sequence_stmt>();
    while (ctx.tokens.valid() && !anzu::is_sentinel(ctx.tokens.curr().text)) {
        stmt.sequence.push_back(parse_statement(ctx));
    }

    // If there is only one element in the sequence, return that directly
    if (stmt.sequence.size() == 1) {
        node = std::move(stmt.sequence.back());
    }
    return node;
}

auto parse_while_body(parser_context& ctx) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_while_stmt>();
    stmt.condition = parse_expression_type_checked(ctx, tk_bool);
    ctx.tokens.consume_only(tk_do);
    stmt.body = parse_statement_list(ctx);
    ctx.tokens.consume_only(tk_end);
    return node;
}

auto parse_if_body(parser_context& ctx) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_if_stmt>();
    stmt.condition = parse_expression_type_checked(ctx, tk_bool);
    ctx.tokens.consume_only(tk_do);
    stmt.body = parse_statement_list(ctx);

    if (ctx.tokens.consume_maybe(tk_elif)) {
        stmt.else_body = parse_if_body(ctx);
    }
    else if (ctx.tokens.consume_maybe(tk_else)) {
        stmt.else_body = parse_statement_list(ctx);
        ctx.tokens.consume_only(tk_end);
    }
    else {
        ctx.tokens.consume_only(tk_end);
    }
    
    return node;
}

auto parse_for_body(parser_context& ctx) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_for_stmt>();
    if (ctx.tokens.curr().type != token_type::name) {
        parser_error(ctx.tokens.curr(), "invalid for loop, bind target must be a name");
    }
    stmt.var = ctx.tokens.consume().text;
    add_variable(ctx, stmt.var, tk_any);
    ctx.tokens.consume_only(tk_in);
    stmt.container = parse_expression_type_checked(ctx, tk_list);
    ctx.tokens.consume_only(tk_do);
    stmt.body = parse_statement_list(ctx);
    ctx.tokens.consume_only(tk_end);
    return node;
}

auto parse_function_def(parser_context& ctx) -> node_stmt_ptr
{
    constexpr auto error_msg = std::string_view{"failed to parse signature for '{}', '{}' is not a type"};

    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_function_def_stmt>();
    if (ctx.tokens.curr().type != token_type::name) {
        parser_error(ctx.tokens.curr(), "expected function name");
    }   
    stmt.name = ctx.tokens.consume().text;
    ctx.tokens.consume_only(tk_lparen);
    ctx.tokens.consume_comma_separated_list(tk_rparen, [&] {
        if (ctx.tokens.curr().type != token_type::name) {
            parser_error(ctx.tokens.curr(), "failed to parse function argument");
        }
        auto arg = function_signature::arg{};
        arg.name = ctx.tokens.consume().text;

        if (ctx.tokens.consume_maybe(tk_colon)) {
            const auto type = ctx.tokens.consume();
            if (!is_type(type.text)) {
                parser_error(ctx.tokens.curr(), error_msg, stmt.name, type.text);
            }
            arg.type = type.text;
        }

        stmt.sig.args.push_back(arg);
    });
    ctx.scopes.top().functions[stmt.name] = stmt.sig;
    
    // New scope for variable definitions
    ctx.scopes.emplace();

    // Add the function to its own scope to allow for recursion
    ctx.scopes.top().functions[stmt.name] = stmt.sig;

    for (const auto& arg : stmt.sig.args) {
        add_variable(ctx, arg.name, arg.type);
    }

    if (ctx.tokens.consume_maybe(tk_rarrow)) {
        const auto type = ctx.tokens.consume();
        if (!is_type(type.text)) {
            parser_error(ctx.tokens.curr(), error_msg, stmt.name, type.text);
        }
    }
    ctx.tokens.consume_only(tk_do);
    stmt.body = parse_statement_list(ctx);
    ctx.tokens.consume_only(tk_end);
    ctx.scopes.pop();
    return node;
}

auto parse_return(parser_context& ctx) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_return_stmt>();
    
    if (!anzu::is_sentinel(ctx.tokens.curr().text)) {
        auto expr = parse_expression(ctx);
        auto type = type_of_expr(ctx, *expr);
        stmt.return_value = std::move(expr);
        // TODO: Check type against function signature.
    } else {
        stmt.return_value = std::make_unique<anzu::node_expr>();
        stmt.return_value->emplace<anzu::node_literal_expr>().value = anzu::null_object();
        // TODO: Disallow, always require a return statement.
    }
    return node;
}

template <typename NodeVariant, typename NodeType>
auto parse_function_call(parser_context& ctx) -> std::unique_ptr<NodeVariant>
{
    auto node = std::make_unique<NodeVariant>();
    auto& out = node->emplace<NodeType>();

    out.function_name = ctx.tokens.consume().text;
    ctx.tokens.consume_only(tk_lparen);
    
    ctx.tokens.consume_comma_separated_list(tk_rparen, [&] {
        out.args.push_back(parse_expression(ctx));
    });

    type_check_function_call(ctx, out.function_name, out.args);
    return node;
}

auto parse_function_call_expr(parser_context& ctx) -> node_expr_ptr
{
    return parse_function_call<anzu::node_expr, anzu::node_function_call_expr>(ctx);
}

auto parse_function_call_stmt(parser_context& ctx) -> node_stmt_ptr
{
    return parse_function_call<anzu::node_stmt, anzu::node_function_call_stmt>(ctx);
}

auto parse_statement(parser_context& ctx) -> node_stmt_ptr
{
    auto& tokens = ctx.tokens;
    if (tokens.consume_maybe(tk_function)) {
        return parse_function_def(ctx);
    }
    else if (tokens.consume_maybe(tk_return)) {
        return parse_return(ctx);
    }
    else if (tokens.consume_maybe(tk_while)) {
        return parse_while_body(ctx);
    }
    else if (tokens.consume_maybe(tk_if)) {
        return parse_if_body(ctx);
    }
    else if (tokens.consume_maybe(tk_for)) {
        return parse_for_body(ctx);
    }
    else if (tokens.consume_maybe(tk_break)) {
        auto node = std::make_unique<anzu::node_stmt>();
        node->emplace<anzu::node_break_stmt>();
        return node;
    }
    else if (tokens.consume_maybe(tk_continue)) {
        auto node = std::make_unique<anzu::node_stmt>();
        node->emplace<anzu::node_continue_stmt>();
        return node;
    }
    else if (tokens.peek_next(tk_assign)) { // <name> '='
        return parse_assigment_stmt(ctx);
    }
    else if (tokens.peek_next(tk_lparen)) { // <name> '('
        return parse_function_call_stmt(ctx);
    }
    else {
        parser_error(ctx.tokens.curr(), "unknown statement '{}'", ctx.tokens.curr().text);
    }
}

}

auto parse(const std::vector<anzu::token>& tokens) -> node_stmt_ptr
{
    auto ctx = anzu::parser_context{ .tokens = {tokens} };
    ctx.scopes.emplace();

    auto root = std::make_unique<anzu::node_stmt>();
    auto& seq = root->emplace<anzu::node_sequence_stmt>();
    while (ctx.tokens.valid()) {
        seq.sequence.push_back(parse_statement(ctx));
    }
    return root;
}

}