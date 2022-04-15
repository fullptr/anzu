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
    print("[ERROR] ({}:{}) {}\n", tok.line, tok.col, formatted_msg);
    std::exit(1);
}

template <typename... Args>
[[noreturn]] void parser_assert(bool cond, const token& tok, std::string_view msg, Args&&... args)
{
    if (!cond) {
        parser_error(tok, msg, std::forward<Args>(args)...);
    }
}

auto to_i32(std::string_view token) -> std::int32_t
{
    token.remove_suffix(tk_i32.size());
    auto result = std::int32_t{};
    const auto [ptr, ec] = std::from_chars(token.data(), token.data() + token.size(), result);
    if (ec != std::errc{}) {
        print("type error: cannot convert '{}' to '{}'\n", token, tk_i32);
        std::exit(1);
    }
    return result;
}

auto to_i64(std::string_view token) -> std::int64_t
{
    if (token.ends_with(tk_i64)) {
        token.remove_suffix(tk_i64.size());
    }
    auto result = std::int64_t{};
    const auto [ptr, ec] = std::from_chars(token.data(), token.data() + token.size(), result);
    if (ec != std::errc{}) {
        print("type error: cannot convert '{}' to '{}'\n", token, tk_i64);
        std::exit(1);
    }
    return result;
}

auto to_u64(std::string_view token) -> std::uint64_t
{
    if (token.ends_with(tk_u64)) {
        token.remove_suffix(tk_u64.size());
    } else if (token.ends_with('u')) {
        token.remove_suffix(1);
    }
    auto result = std::uint64_t{};
    const auto [ptr, ec] = std::from_chars(token.data(), token.data() + token.size(), result);
    if (ec != std::errc{}) {
        print("type error: cannot convert '{}' to uint\n", token);
        std::exit(1);
    }
    return result;
}

auto to_f64(std::string_view token) -> double
{
    auto result = double{};
    const auto [ptr, ec] = std::from_chars(token.data(), token.data() + token.size(), result);
    if (ec != std::errc{}) {
        print("type error: cannot convert '{}' to float\n", token);
        std::exit(1);
    }
    return result;
}

auto parse_expression(tokenstream& tokens) -> node_expr_ptr;
auto parse_statement(tokenstream& tokens) -> node_stmt_ptr;

auto parse_literal(tokenstream& tokens) -> object
{
    if (tokens.curr().type == token_type::i32) {
        return make_i32(to_i32(tokens.consume().text));
    }
    if (tokens.curr().type == token_type::i64) {
        return make_i64(to_i64(tokens.consume().text));
    }
    if (tokens.curr().type == token_type::u64) {
        return make_u64(to_u64(tokens.consume().text));
    }
    if (tokens.curr().type == token_type::f64) {
        return make_f64(to_f64(tokens.consume().text));
    }
    if (tokens.curr().type == token_type::character) {
        const auto c = tokens.consume().text;
        parser_assert(c.size() == 1, tokens.curr(), "failed to parse char ({})", c);
        return make_char(c.front());
    }
    if (tokens.curr().type == token_type::string) {
        auto ret = object{};
        for (char c : tokens.curr().text) {
            ret.data.push_back(static_cast<std::byte>(c));
        }
        ret.type = concrete_list_type(char_type(), tokens.curr().text.size());
        tokens.consume();
        return ret;
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
    parser_error(tokens.curr(), "failed to parse literal ({})", tokens.curr().text);
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
    auto node = std::make_unique<node_expr>();
    auto& out = node->emplace<node_function_call_expr>();
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
    auto node = std::make_unique<node_expr>();
    
    if (tokens.consume_maybe(tk_lparen)) {
        node = parse_expression(tokens);
        tokens.consume_only(tk_rparen);
    }
    else if (tokens.peek(tk_lbracket)) {
        auto& expr = node->emplace<node_list_expr>();
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
        auto& expr = node->emplace<node_addrof_expr>();
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
        auto& expr = node->emplace<node_deref_expr>();
        expr.token = tokens.consume();
        expr.expr = parse_single_factor(tokens);
    }
    else if (tokens.peek_next(tk_lparen)) {
        node = parse_function_call(tokens);
    }
    else if (tokens.curr().type == token_type::name) {
        auto& expr = node->emplace<node_variable_expr>();
        expr.token = tokens.consume();
        expr.name = expr.token.text;
    }
    else {
        auto& expr = node->emplace<node_literal_expr>();
        expr.token = tokens.curr();
        expr.value = parse_literal(tokens);
    }

    while (tokens.peek(tk_fullstop) || tokens.peek(tk_rarrow) || tokens.peek(tk_lbracket)) {
        auto new_node = std::make_unique<node_expr>();
        if (tokens.peek(tk_fullstop)) {
            const auto tok = tokens.consume();
            if (tokens.peek_next(tk_lparen)) {
                auto& expr = new_node->emplace<node_member_function_call_expr>();
                expr.token = tok;
                expr.function_name = tokens.consume().text;
                tokens.consume_only(tk_lparen);
                tokens.consume_comma_separated_list(tk_rparen, [&] {
                    expr.args.push_back(parse_expression(tokens));
                });
                expr.expr = std::move(node);
            } else {
                auto& expr = new_node->emplace<node_field_expr>();
                expr.token = tok;
                expr.field_name = tokens.consume().text;
                expr.expr = std::move(node);
            }
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
        auto node = std::make_unique<node_expr>();
        auto& expr = node->emplace<node_binary_op_expr>();
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
    if (tokens.consume_maybe(tk_ampersand)) {
        return {type_ptr{ .inner_type={parse_type(tokens)} }};
    }
    auto type = type_name{type_simple{.name=tokens.consume().text}};
    while (tokens.consume_maybe(tk_lbracket)) {
        auto new_type = type_name{type_list{
            .inner_type=type, .count=static_cast<std::size_t>(tokens.consume_i64())
        }};
        tokens.consume_only(tk_rbracket);
        type = new_type;
    }
    return type;
}

auto parse_function_def_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_unique<node_stmt>();
    auto& stmt = node->emplace<node_function_def_stmt>();

    stmt.token = tokens.consume_only(tk_function);
    stmt.name = parse_name(tokens);
    tokens.consume_only(tk_lparen);
    tokens.consume_comma_separated_list(tk_rparen, [&]{
        auto arg = function_arg{};
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

auto parse_member_function_def_stmt(
    const std::string& struct_name, tokenstream& tokens
)
    -> node_stmt_ptr
{
    auto node = std::make_unique<node_stmt>();
    auto& stmt = node->emplace<node_member_function_def_stmt>();

    stmt.token = tokens.consume_only(tk_function);
    stmt.struct_name = struct_name;
    stmt.function_name = parse_name(tokens);
    tokens.consume_only(tk_lparen);
    tokens.consume_comma_separated_list(tk_rparen, [&]{
        auto arg = function_arg{};
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
    auto node = std::make_unique<node_stmt>();
    auto& stmt = node->emplace<node_return_stmt>();
    
    stmt.token = tokens.consume_only(tk_return);
    if (!is_sentinel(tokens.curr().text)) {
        stmt.return_value = parse_expression(tokens);
    } else {
        stmt.return_value = std::make_unique<node_expr>();
        auto& ret_expr = stmt.return_value->emplace<node_literal_expr>();
        ret_expr.value = make_null();
        ret_expr.token = stmt.token;
    }
    return node;
}

auto parse_while_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_unique<node_stmt>();
    auto& stmt = node->emplace<node_while_stmt>();

    stmt.token = tokens.consume_only(tk_while);
    stmt.condition = parse_expression(tokens);
    stmt.body = parse_statement(tokens);
    return node;
}

auto parse_if_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_unique<node_stmt>();
    auto& stmt = node->emplace<node_if_stmt>();

    stmt.token = tokens.consume_only(tk_if);
    stmt.condition = parse_expression(tokens);
    stmt.body = parse_statement(tokens);
    if (tokens.consume_maybe(tk_else)) {
        stmt.else_body = parse_statement(tokens);
    }
    return node;
}

auto parse_struct_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_unique<node_stmt>();
    auto& stmt = node->emplace<node_struct_stmt>();

    stmt.token = tokens.consume_only(tk_struct);
    stmt.name = parse_name(tokens);
    tokens.consume_only(tk_lbrace);
    while (!tokens.consume_maybe(tk_rbrace)) {
        if (tokens.peek(tk_function)) {
            stmt.functions.emplace_back(parse_member_function_def_stmt(stmt.name, tokens));
        } else {
            stmt.fields.emplace_back();
            auto& f = stmt.fields.back();
            f.name = parse_name(tokens);
            tokens.consume_only(tk_colon);
            f.type = parse_type(tokens);
        }
    }

    return node;
}

auto parse_declaration_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_unique<node_stmt>();
    auto& stmt = node->emplace<node_declaration_stmt>();

    stmt.name = parse_name(tokens);
    stmt.token = tokens.consume_only(tk_declare);
    stmt.expr = parse_expression(tokens);
    return node;
}

auto parse_braced_statement_list(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_unique<node_stmt>();
    auto& stmt = node->emplace<node_sequence_stmt>();
    
    stmt.token = tokens.consume_only(tk_lbrace);
    while (!tokens.consume_maybe(tk_rbrace)) {
        stmt.sequence.push_back(parse_statement(tokens));
    }

    return node;
}

auto parse_statement(tokenstream& tokens) -> node_stmt_ptr
{
    if (tokens.peek(tk_function) || tokens.peek(tk_struct)) {
        parser_error(tokens.curr(), "functions and structs can only be declared in the global scope");
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

    auto node = std::make_unique<node_stmt>();
    auto expr = parse_expression(tokens);
    if (tokens.peek(tk_assign)) {
        auto& stmt = node->emplace<node_assignment_stmt>();
        stmt.token = tokens.consume();
        stmt.position = std::move(expr);
        stmt.expr = parse_expression(tokens);
    } else {
        auto& stmt = node->emplace<node_expression_stmt>();
        stmt.token = std::visit([](auto&& n) { return n.token; }, *expr);
        stmt.expr = std::move(expr);
    }
    return node;
}

auto parse_top_level_statement(tokenstream& tokens) -> node_stmt_ptr
{
    if (tokens.peek(tk_function)) {
        return parse_function_def_stmt(tokens);
    }
    if (tokens.peek(tk_struct)) {
        return parse_struct_stmt(tokens);
    }
    return parse_statement(tokens);
}

}

auto parse(const std::vector<token>& tokens) -> node_stmt_ptr
{
    auto stream = tokenstream{tokens};

    auto root = std::make_unique<node_stmt>();
    auto& seq = root->emplace<node_sequence_stmt>();
    while (stream.valid()) {
        seq.sequence.push_back(parse_top_level_statement(stream));
    }
    return root;
}

}