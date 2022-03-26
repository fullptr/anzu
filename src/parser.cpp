#include "parser.hpp"
#include "functions.hpp"
#include "vocabulary.hpp"
#include "type.hpp"

#include <unordered_set>
#include <string_view>
#include <vector>
#include <memory>
#include <charconv>

namespace anzu {
namespace {

template <typename... Args>
[[noreturn]] void parser_error(const token& tok, std::string_view msg, Args&&... args)
{
    const auto formatted_msg = std::format(msg, std::forward<Args>(args)...);
    anzu::print("[ERROR] ({}:{}) {}\n", tok.line, tok.col, formatted_msg);
    std::exit(1);
}

auto to_int(std::string_view token) -> int
{
    auto result = int{};
    const auto [ptr, ec] = std::from_chars(token.data(), token.data() + token.size(), result);
    if (ec != std::errc{}) {
        anzu::print("type error: cannot convert '{}' to int\n", token);
        std::exit(1);
    }
    return result;
}

auto parse_expression(tokenstream& tokens) -> node_expr_ptr;
auto parse_statement(tokenstream& tokens) -> node_stmt_ptr;

auto parse_literal(tokenstream& tokens) -> object
{
    if (tokens.curr().type == token_type::number) {
        return make_int(to_int(tokens.consume().text));
    }
    if (tokens.curr().type == token_type::string) {
        return make_str(tokens.consume().text);
    }
    if (tokens.consume_maybe(tk_true)) {
        return make_bool(true);
    }
    if (tokens.consume_maybe(tk_false)) {
        return make_bool(false);
    }
    if (tokens.consume_maybe(tk_null)) {
        return make_null();
    }
    parser_error(tokens.curr(), "failed to parse literal");
};

template <typename NodeVariant, typename NodeType>
auto parse_function_call(tokenstream& tokens) -> std::unique_ptr<NodeVariant>
{
    auto node = std::make_unique<NodeVariant>();
    auto& out = node->emplace<NodeType>();
    out.token = tokens.consume();

    out.function_name = out.token.text;
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
    auto node = std::make_unique<anzu::node_expr>();
    
    if (tokens.consume_maybe(tk_lparen)) {
        node = parse_expression(tokens);
        tokens.consume_only(tk_rparen);
    }
    else if (tokens.peek(tk_lbracket)) {
        auto& expr = node->emplace<anzu::node_list_expr>();
        expr.token = tokens.consume();
        tokens.consume_comma_separated_list(tk_rbracket, [&] {
            expr.elements.push_back(parse_expression(tokens));
        });
    }
    else if (tokens.peek(tk_addrof)) {
        auto& expr = node->emplace<anzu::node_addrof_expr>();
        expr.token = tokens.consume();
        expr.expr = parse_expression(tokens);
    }
    else if (tokens.peek(tk_deref)) {
        auto& expr = node->emplace<anzu::node_deref_expr>();
        expr.token = tokens.consume();
        expr.expr = parse_expression(tokens);
    }
    else if (tokens.peek_next(tk_lparen)) {
        node = parse_function_call_expr(tokens);
    }
    else if (tokens.curr().type == token_type::name) {
        auto& expr = node->emplace<anzu::node_variable_expr>();
        expr.token = tokens.consume();
        expr.name = expr.token.text;
    }
    else {
        auto& expr = node->emplace<anzu::node_literal_expr>();
        expr.token = tokens.curr();
        expr.value = parse_literal(tokens);
    }

    while (tokens.peek(tk_fullstop)) {
        auto new_node = std::make_unique<anzu::node_expr>();
        auto& expr = new_node->emplace<anzu::node_field_expr>();
        expr.token = tokens.consume();
        expr.field_name = tokens.consume().text;
        expr.expression = std::move(node);
        node = std::move(new_node);
    }

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
        expr.token = tokens.consume();
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

auto parse_type(tokenstream& tokens) -> type_name
{
    if (tokens.consume_maybe(tk_lbracket)) {
        const auto id = tokens.consume_int();
        tokens.consume_only(tk_rbracket);
        return { type_generic{ .id = id } };
    }
    const auto type_name_text = tokens.consume().text;
    if (tokens.consume_maybe(tk_lt)) {
        auto ret = type_name{};
        auto& compound = ret.emplace<type_compound>();
        compound.name = type_name_text;
        tokens.consume_comma_separated_list(tk_gt, [&] {
            compound.subtypes.push_back(parse_type(tokens));
        });
        return ret;
    }
    
    return { type_simple{ .name = type_name_text } };
}

auto parse_function_def_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_function_def_stmt>();

    stmt.token = tokens.consume_only(tk_function);
    stmt.name = parse_name(tokens);
    tokens.consume_only(tk_lparen);
    tokens.consume_comma_separated_list(tk_rparen, [&] {
        auto arg = signature::arg{};
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
    
    stmt.token = tokens.consume_only(tk_return);
    if (!anzu::is_sentinel(tokens.curr().text)) {
        stmt.return_value = parse_expression(tokens);
    } else {
        stmt.return_value = std::make_unique<anzu::node_expr>();
        auto& ret_expr = stmt.return_value->emplace<anzu::node_literal_expr>();
        ret_expr.value = anzu::make_null();
        ret_expr.token = stmt.token;
    }
    return node;
}

auto parse_while_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_while_stmt>();

    stmt.token = tokens.consume_only(tk_while);
    stmt.condition = parse_expression(tokens);
    stmt.body = parse_statement(tokens);
    return node;
}

auto parse_if_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_if_stmt>();

    stmt.token = tokens.consume_only(tk_if);
    stmt.condition = parse_expression(tokens);
    stmt.body = parse_statement(tokens);
    if (tokens.valid() && tokens.consume_maybe(tk_else)) {
        stmt.else_body = parse_statement(tokens);
    }
    return node;
}

auto parse_for_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_for_stmt>();

    stmt.token = tokens.consume_only(tk_for);
    stmt.var = parse_name(tokens);
    tokens.consume_only(tk_in);
    stmt.container = parse_expression(tokens);
    stmt.body = parse_statement(tokens);
    return node;
}

auto parse_struct_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_struct_stmt>();

    stmt.token = tokens.consume_only(tk_struct);
    stmt.name = make_type(parse_name(tokens));
    tokens.consume_only(tk_lbrace);
    while (!tokens.consume_maybe(tk_rbrace)) {
        stmt.fields.emplace_back();
        auto& f = stmt.fields.back();
        f.name = parse_name(tokens);
        tokens.consume_only(tk_colon);
        f.type = parse_type(tokens);
    }

    return node;
}

auto parse_declaration_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_declaration_stmt>();

    stmt.name = parse_name(tokens);
    stmt.token = tokens.consume_only(tk_declare);
    stmt.expr = parse_expression(tokens);
    return node;
}

auto parse_assignment_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_assignment_stmt>();

    stmt.position = parse_expression(tokens);
    stmt.token = tokens.consume_only(tk_assign);
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
    
    stmt.token = tokens.consume_only(tk_lbrace);
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
    if (tokens.peek(tk_struct)) {
        return parse_struct_stmt(tokens);
    }
    if (tokens.peek(tk_break)) {
        return std::make_unique<node_stmt>(node_break_stmt{ tokens.consume() });
    }
    if (tokens.peek(tk_continue)) {
        return std::make_unique<node_stmt>(node_continue_stmt{ tokens.consume() });
    }
    if (tokens.peek_next(tk_declare)) { // <name> ':=' <expr>
        return parse_declaration_stmt(tokens);
    }
    if (tokens.peek_next(tk_lparen)) { // <name> '('
        return parse_function_call_stmt(tokens);
    }
    if (tokens.peek(tk_lbrace)) {
        return parse_braced_statement_list(tokens);
    }
    return parse_assignment_stmt(tokens);
}

auto parse_top_level_statement(tokenstream& tokens) -> node_stmt_ptr
{
    if (tokens.peek(tk_function)) {
        return parse_function_def_stmt(tokens);
    }
    return parse_statement(tokens);
}

}

auto parse(const std::vector<anzu::token>& tokens) -> node_stmt_ptr
{
    auto stream = tokenstream{tokens};

    auto root = std::make_unique<anzu::node_stmt>();
    auto& seq = root->emplace<anzu::node_sequence_stmt>();
    while (stream.valid()) {
        seq.sequence.push_back(parse_top_level_statement(stream));
    }
    return root;
}

}