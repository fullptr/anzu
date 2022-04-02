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

auto to_int(std::string_view token) -> block_int
{
    auto result = block_int{};
    const auto [ptr, ec] = std::from_chars(token.data(), token.data() + token.size(), result);
    if (ec != std::errc{}) {
        anzu::print("type error: cannot convert '{}' to int\n", token);
        std::exit(1);
    }
    return result;
}
   
auto to_float(std::string_view token) -> block_float
{
    auto result = block_float{};
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
    if (tokens.curr().type == token_type::integer) {
        return make_int(to_int(tokens.consume().text));
    }
    if (tokens.curr().type == token_type::floating) {
        return make_float(to_float(tokens.consume().text));
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

auto parse_function_call(tokenstream& tokens) -> node_expr_ptr
{
    auto node = std::make_unique<anzu::node_expr>();
    auto& out = node->emplace<anzu::node_function_call_expr>();
    out.token = tokens.consume();

    out.function_name = out.token.text;
    tokens.consume_only(tk_lparen);
    tokens.consume_comma_separated_list(tk_rparen, [&] {
        out.args.push_back(parse_expression(tokens));
    });
    return node;
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
    else if (tokens.peek(tk_sub) || tokens.peek(tk_bang)) {
        auto& expr = node->emplace<node_unary_op_expr>();
        expr.token = tokens.consume();
        expr.expr = parse_single_factor(tokens);
    }
    else if (tokens.peek(tk_ampersand)) {
        auto& expr = node->emplace<anzu::node_addrof_expr>();
        expr.token = tokens.consume();
        expr.expr = parse_single_factor(tokens);
    }
    else if (tokens.peek(tk_size_of)) {
        auto& expr = node->emplace<node_sizeof_expr>();
        expr.token = tokens.consume();
        tokens.consume_only(tk_lparen);
        expr.expr = parse_expression(tokens);
        tokens.consume_only(tk_rparen);
    }
    else if (tokens.peek(tk_mul)) {
        auto& expr = node->emplace<anzu::node_deref_expr>();
        expr.token = tokens.consume();
        expr.expr = parse_single_factor(tokens);
    }
    else if (tokens.peek_next(tk_lparen)) {
        node = parse_function_call(tokens);
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

    while (tokens.peek(tk_fullstop) || tokens.peek(tk_rarrow) || tokens.peek(tk_lbracket)) {
        auto new_node = std::make_unique<node_expr>();
        if (tokens.peek(tk_fullstop)) {
            auto& expr = new_node->emplace<node_field_expr>();
            expr.token = tokens.consume();
            expr.field_name = tokens.consume().text;
            expr.expr = std::move(node);
        } else if (tokens.peek(tk_rarrow)) {
            auto& expr = new_node->emplace<node_arrow_expr>();
            expr.token = tokens.consume();
            expr.field_name = tokens.consume().text;
            expr.expr = std::move(node);
        } else {
            auto& expr = new_node->emplace<node_subscript_expr>();
            expr.token = tokens.consume();
            expr.index = parse_expression(tokens);
            tokens.consume_only(tk_rbracket);
            expr.expr = std::move(node);
        }
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
        auto& expr = node->emplace<anzu::node_binary_op_expr>();
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
    if (tokens.consume_maybe(tk_list)) {
        tokens.consume_only(tk_lt);
        const auto inner_type = parse_type(tokens);
        tokens.consume_only(tk_comma);
        const auto count = tokens.consume_int();
        tokens.consume_only(tk_gt);
        return {type_list{
            .inner_type = {inner_type}, .count=static_cast<std::size_t>(count)
        }};
    }
    if (tokens.consume_maybe(tk_ptr)) {
        tokens.consume_only(tk_lt);
        const auto inner_type = parse_type(tokens);
        tokens.consume_only(tk_gt);
        return {type_ptr{
            .inner_type = { inner_type }
        }};
    }
    return {type_simple{.name=tokens.consume().text}};
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

auto parse_braced_statement_list(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_sequence_stmt>();
    
    stmt.token = tokens.consume_only(tk_lbrace);
    while (!tokens.consume_maybe(tk_rbrace)) {
        stmt.sequence.push_back(parse_statement(tokens));
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
    if (tokens.peek(tk_lbrace)) {
        return parse_braced_statement_list(tokens);
    }

    auto expr = parse_expression(tokens);
    if (tokens.peek(tk_assign)) {
        auto node = std::make_unique<anzu::node_stmt>();
        auto& stmt = node->emplace<anzu::node_assignment_stmt>();
        stmt.token = tokens.consume();
        stmt.position = std::move(expr);
        stmt.expr = parse_expression(tokens);
        return node;
    } else {
        auto node = std::make_unique<anzu::node_stmt>();
        auto& stmt = node->emplace<anzu::node_expression_stmt>();
        stmt.token = std::visit([](auto&& n) { return n.token; }, *expr);
        stmt.expr = std::move(expr);
        return node;
    }
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