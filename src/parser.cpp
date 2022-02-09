#include "parser.hpp"
#include "functions.hpp"
#include "typecheck.hpp"
#include "vocabulary.hpp"

#include <unordered_set>
#include <string_view>
#include <vector>
#include <memory>

namespace anzu {
namespace {

template <typename... Args>
[[noreturn]] void parser_error(const token& tok, std::string_view msg, Args&&... args)
{
    const auto formatted_msg = std::format(msg, std::forward<Args>(args)...);
    anzu::print("[ERROR] ({}:{}) {}\n", tok.line, tok.col, formatted_msg);
    std::exit(1);
}

auto parse_expression(parser_context& ctx) -> node_expr_ptr;

auto parse_literal(parser_context& ctx) -> anzu::object
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
        auto list = std::make_shared<std::vector<anzu::object>>();
        ctx.tokens.consume_comma_separated_list(tk_rbracket, [&] {
            list->push_back(parse_literal(ctx));
        });
        return { list };
    }
    parser_error(ctx.tokens.curr(), "failed to parse string literal");
};

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
    return node;
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

auto parse_function_call_expr(parser_context& ctx) -> node_expr_ptr
{
    return parse_function_call<anzu::node_expr, anzu::node_function_call_expr>(ctx);
}

auto parse_single_factor(parser_context& ctx) -> node_expr_ptr
{
    auto& tokens = ctx.tokens;
    if (ctx.tokens.consume_maybe(tk_lparen)) {
        auto expr = parse_expression(ctx);
        ctx.tokens.consume_only(tk_rparen);
        return expr;
    }  
    if (tokens.peek_next(tk_lparen)) {
        return parse_function_call_expr(ctx);
    }
    if (tokens.curr().type == token_type::name) {
        auto node = std::make_unique<anzu::node_expr>();
        auto& expr = node->emplace<anzu::node_variable_expr>();
        expr.name = tokens.consume().text;
        return node;
    }

    auto node = std::make_unique<anzu::node_expr>();
    auto& expr = node->emplace<anzu::node_literal_expr>();
    expr.value = parse_literal(ctx);
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

auto parse_name(parser_context& ctx)
{
    const auto token = ctx.tokens.consume();
    if (token.type != token_type::name) {
        parser_error(token, "'{}' is not a valid name", token.text);
    }
    return token.text;   
}

auto parse_type(parser_context& ctx)
{
    const auto token = ctx.tokens.consume();
    if (token.type != token_type::keyword) {
        parser_error(token, "'{}' is not a valid type", token.text);
    }
    return token.text;   
}

auto parse_statement(parser_context& ctx) -> node_stmt_ptr;

auto parse_function_def_stmt(parser_context& ctx) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_function_def_stmt>();

    ctx.scopes.emplace();
    ctx.tokens.consume_only(tk_function);
    stmt.name = parse_name(ctx);
    ctx.tokens.consume_only(tk_lparen);
    ctx.tokens.consume_comma_separated_list(tk_rparen, [&] {
        auto arg = function_signature::arg{};
        arg.name = parse_name(ctx);
        if (ctx.tokens.consume_maybe(tk_colon)) {
            arg.type = parse_type(ctx);
        }

        stmt.sig.args.push_back(arg);
    });    

    if (ctx.tokens.consume_maybe(tk_rarrow)) {
        stmt.sig.return_type = parse_type(ctx); // TODO: Check this
    }

    ctx.scopes.top().functions[stmt.name] = stmt.sig; // Allows for recursion
    stmt.body = parse_statement(ctx);
    ctx.scopes.pop();

    ctx.scopes.top().functions[stmt.name] = stmt.sig; // Make function available
    return node;
}

auto parse_return_stmt(parser_context& ctx) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_return_stmt>();
    
    ctx.tokens.consume_only(tk_return);

    // TODO: Make return statements mandatory and check the expr type
    // against the function signature
    if (!anzu::is_sentinel(ctx.tokens.curr().text)) {
        auto expr = parse_expression(ctx);
        auto type = type_of_expr(ctx, *expr);
        stmt.return_value = std::move(expr);
    } else {
        stmt.return_value = std::make_unique<anzu::node_expr>();
        stmt.return_value->emplace<anzu::node_literal_expr>().value = anzu::null_object();
    }
    return node;
}

auto parse_while_stmt(parser_context& ctx) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_while_stmt>();

    ctx.tokens.consume_only(tk_while);
    stmt.condition = parse_expression(ctx);
    stmt.body = parse_statement(ctx);
    return node;
}

auto parse_if_stmt(parser_context& ctx) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_if_stmt>();

    ctx.tokens.consume_only(tk_if);
    stmt.condition = parse_expression(ctx);
    stmt.body = parse_statement(ctx);
    if (ctx.tokens.consume_maybe(tk_else)) {
        stmt.else_body = parse_statement(ctx);
    }
    return node;
}

auto parse_for_stmt(parser_context& ctx) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_for_stmt>();

    ctx.tokens.consume_only(tk_for);
    stmt.var = parse_name(ctx);
    ctx.tokens.consume_only(tk_in);
    stmt.container = parse_expression(ctx);
    stmt.body = parse_statement(ctx);
    return node;
}

auto parse_assignment_stmt(parser_context& ctx) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_assignment_stmt>();

    stmt.name = parse_name(ctx);
    ctx.tokens.consume_only(tk_assign);
    stmt.expr = parse_expression(ctx);
    return node;
}

auto parse_function_call_stmt(parser_context& ctx) -> node_stmt_ptr
{
    return parse_function_call<anzu::node_stmt, anzu::node_function_call_stmt>(ctx);
}

auto parse_braced_statement_list(parser_context& ctx) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_sequence_stmt>();
    
    ctx.tokens.consume_only(tk_lbrace);
    while (ctx.tokens.valid() && !anzu::is_sentinel(ctx.tokens.curr().text)) {
        stmt.sequence.push_back(parse_statement(ctx));
    }
    ctx.tokens.consume_only(tk_rbrace);

    // If there is only one element in the sequence, return that directly
    if (stmt.sequence.size() == 1) {
        node = std::move(stmt.sequence.back());
    }
    return node;
}

auto parse_statement(parser_context& ctx) -> node_stmt_ptr
{
    auto& tokens = ctx.tokens;
    if (tokens.peek(tk_function)) {
        return parse_function_def_stmt(ctx);
    }
    if (tokens.peek(tk_return)) {
        return parse_return_stmt(ctx);
    }
    if (tokens.peek(tk_while)) {
        return parse_while_stmt(ctx);
    }
    if (tokens.peek(tk_if)) {
        return parse_if_stmt(ctx);
    }
    if (tokens.peek(tk_for)) {
        return parse_for_stmt(ctx);
    }
    if (tokens.consume_maybe(tk_break)) {
        auto node = std::make_unique<anzu::node_stmt>();
        node->emplace<anzu::node_break_stmt>();
        return node;
    }
    if (tokens.consume_maybe(tk_continue)) {
        auto node = std::make_unique<anzu::node_stmt>();
        node->emplace<anzu::node_continue_stmt>();
        return node;
    }
    if (tokens.peek_next(tk_assign)) { // <name> '='
        return parse_assignment_stmt(ctx);
    }
    if (tokens.peek_next(tk_lparen)) { // <name> '('
        return parse_function_call_stmt(ctx);
    }
    if (tokens.peek(tk_lbrace)) {
        return parse_braced_statement_list(ctx);
    }
    parser_error(ctx.tokens.curr(), "unknown statement '{}'", ctx.tokens.curr().text);
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