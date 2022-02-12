#include "parser.hpp"
#include "functions.hpp"
#include "vocabulary.hpp"
#include "type.hpp"

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

auto parse_expression(tokenstream& tokens) -> node_expr_ptr;
auto parse_statement(tokenstream& tokens) -> node_stmt_ptr;

auto parse_literal(tokenstream& tokens) -> anzu::object
{
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
        tokens.consume_comma_separated_list(tk_rbracket, [&] {
            list->push_back(parse_literal(tokens));
        });
        return { list };
    }
    parser_error(tokens.curr(), "failed to parse literal");
};

template <typename NodeVariant, typename NodeType>
auto parse_function_call(tokenstream& tokens) -> std::unique_ptr<NodeVariant>
{
    auto node = std::make_unique<NodeVariant>();
    auto& out = node->emplace<NodeType>();

    out.function_name = tokens.consume().text;
    tokens.consume_only(tk_lparen);
    tokens.consume_comma_separated_list(tk_rparen, [&] {
        out.args.push_back(parse_expression(tokens));
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

auto parse_function_call_expr(tokenstream& tokens) -> node_expr_ptr
{
    return parse_function_call<anzu::node_expr, anzu::node_function_call_expr>(tokens);
}

auto parse_single_factor(tokenstream& tokens) -> node_expr_ptr
{
    if (tokens.consume_maybe(tk_lparen)) {
        auto expr = parse_expression(tokens);
        tokens.consume_only(tk_rparen);
        return expr;
    }  
    if (tokens.peek_next(tk_lparen)) {
        return parse_function_call_expr(tokens);
    }
    if (tokens.curr().type == token_type::name) {
        auto node = std::make_unique<anzu::node_expr>();
        auto& expr = node->emplace<anzu::node_variable_expr>();
        expr.name = tokens.consume().text;
        return node;
    }

    auto node = std::make_unique<anzu::node_expr>();
    auto& expr = node->emplace<anzu::node_literal_expr>();
    expr.value = parse_literal(tokens);
    return node;
}

// Level is the precendence level, the lower the number, the tighter the factors bind.
auto parse_compound_factor(tokenstream& tokens, std::int64_t level) -> node_expr_ptr
{
    if (level == 0) {
        return parse_single_factor(tokens);
    }

    auto factor = parse_compound_factor(tokens, level - 1);
    while (tokens.valid() && bin_ops_table[level].contains(tokens.curr().text)) {
        auto node = std::make_unique<anzu::node_expr>();
        auto& expr = node->emplace<anzu::node_bin_op_expr>();
        expr.lhs = std::move(factor);
        expr.op = tokens.consume();
        expr.rhs = parse_compound_factor(tokens, level - 1);
        factor = std::move(node);
    }
    return factor;
}

auto parse_expression(tokenstream& tokens) -> node_expr_ptr
{
    return parse_compound_factor(tokens, std::ssize(bin_ops_table) - 1i64);
}

auto parse_name(tokenstream& tokens)
{
    const auto token = tokens.consume();
    if (token.type != token_type::name) {
        parser_error(token, "'{}' is not a valid name", token.text);
    }
    return token.text;   
}

auto parse_type(tokenstream& tokens) -> type
{
    if (tokens.consume_maybe(tk_lbracket)) {
        const auto id = tokens.consume_int();
        tokens.consume_only(tk_rbracket);
        return { type_generic{ .id = id } };
    }
    const auto type_name = tokens.consume().text;
    if (tokens.consume_maybe(tk_gt)) {
        auto ret = type{};
        auto& compound = ret.emplace<type_compound>();
        compound.name = type_name;
        tokens.consume_comma_separated_list(tk_lt, [&] {
            compound.subtypes.push_back(parse_type(tokens));
        });
        return ret;
    }
    return { type_simple{ .name = type_name } };
}

auto parse_function_def_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_function_def_stmt>();

    tokens.consume_only(tk_function);
    stmt.name = parse_name(tokens);
    tokens.consume_only(tk_lparen);
    tokens.consume_comma_separated_list(tk_rparen, [&] {
        auto arg = function_signature::arg{};
        arg.name = parse_name(tokens);
        tokens.consume_only(tk_colon);
        arg.type = parse_type(tokens);
        stmt.sig.args.push_back(arg);
    });    
    tokens.consume_only(tk_rarrow);
    stmt.sig.return_type = parse_type(tokens);
    stmt.body = parse_statement(tokens);
    return node;
}

auto parse_return_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_return_stmt>();
    
    tokens.consume_only(tk_return);
    if (!anzu::is_sentinel(tokens.curr().text)) {
        stmt.return_value = parse_expression(tokens);
    } else {
        stmt.return_value = std::make_unique<anzu::node_expr>();
        stmt.return_value->emplace<anzu::node_literal_expr>().value = anzu::null_object();
    }
    return node;
}

auto parse_while_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_while_stmt>();

    tokens.consume_only(tk_while);
    stmt.condition = parse_expression(tokens);
    stmt.body = parse_statement(tokens);
    return node;
}

auto parse_if_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_if_stmt>();

    tokens.consume_only(tk_if);
    stmt.condition = parse_expression(tokens);
    stmt.body = parse_statement(tokens);
    if (tokens.consume_maybe(tk_else)) {
        stmt.else_body = parse_statement(tokens);
    }
    return node;
}

auto parse_for_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_for_stmt>();

    tokens.consume_only(tk_for);
    stmt.var = parse_name(tokens);
    tokens.consume_only(tk_in);
    stmt.container = parse_expression(tokens);
    stmt.body = parse_statement(tokens);
    return node;
}

auto parse_assignment_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_assignment_stmt>();

    stmt.name = parse_name(tokens);
    tokens.consume_only(tk_assign);
    stmt.expr = parse_expression(tokens);
    return node;
}

auto parse_function_call_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    return parse_function_call<anzu::node_stmt, anzu::node_function_call_stmt>(tokens);
}

auto parse_braced_statement_list(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_sequence_stmt>();
    
    tokens.consume_only(tk_lbrace);
    while (!tokens.consume_maybe(tk_rbrace)) {
        stmt.sequence.push_back(parse_statement(tokens));
    }

    // If there is only one element in the sequence, return that directly
    if (stmt.sequence.size() == 1) {
        node = std::move(stmt.sequence.back());
    }
    return node;
}

auto parse_statement(tokenstream& tokens) -> node_stmt_ptr
{
    if (tokens.peek(tk_function)) {
        return parse_function_def_stmt(tokens);
    }
    if (tokens.peek(tk_return)) {
        return parse_return_stmt(tokens);
    }
    if (tokens.peek(tk_while)) {
        return parse_while_stmt(tokens);
    }
    if (tokens.peek(tk_if)) {
        return parse_if_stmt(tokens);
    }
    if (tokens.peek(tk_for)) {
        return parse_for_stmt(tokens);
    }
    if (tokens.consume_maybe(tk_break)) {
        return std::make_unique<node_stmt>(node_break_stmt{});
    }
    if (tokens.consume_maybe(tk_continue)) {
        return std::make_unique<node_stmt>(node_continue_stmt{});
    }
    if (tokens.peek_next(tk_assign)) { // <name> '='
        return parse_assignment_stmt(tokens);
    }
    if (tokens.peek_next(tk_lparen)) { // <name> '('
        return parse_function_call_stmt(tokens);
    }
    if (tokens.peek(tk_lbrace)) {
        return parse_braced_statement_list(tokens);
    }
    if (tokens.consume_maybe(tk_debug)) {
        return std::make_unique<node_stmt>(node_debug_stmt{});
    }
    parser_error(tokens.curr(), "unknown statement '{}'", tokens.curr().text);
}

}

auto parse(const std::vector<anzu::token>& tokens) -> node_stmt_ptr
{
    auto stream = tokenstream{tokens};

    auto root = std::make_unique<anzu::node_stmt>();
    auto& seq = root->emplace<anzu::node_sequence_stmt>();
    while (stream.valid()) {
        seq.sequence.push_back(parse_statement(stream));
    }
    return root;
}

}