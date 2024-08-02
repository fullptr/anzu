#include "parser.hpp"
#include "object.hpp"
#include "functions.hpp"
#include "lexer.hpp"
#include "utility/common.hpp"
#include "parse_expression.hpp"

#include <string_view>
#include <vector>
#include <memory>
#include <charconv>

namespace anzu {
namespace {

auto parse_statement(tokenstream& tokens) -> node_stmt_ptr;

auto parse_name(tokenstream& tokens) -> std::string
{
    return std::string{tokens.consume_only(token_type::identifier).text};
}

auto parse_function_def_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_shared<node_stmt>();
    auto& stmt = node->emplace<node_function_def_stmt>();
    stmt.token = tokens.consume_only(token_type::kw_function);
    stmt.name = parse_name(tokens);

    if (tokens.consume_maybe(token_type::bang)) {
        tokens.consume_only(token_type::left_paren);
        tokens.consume_comma_separated_list(token_type::right_paren, [&]{
            stmt.template_types.push_back(parse_name(tokens));
        });
    }

    tokens.consume_only(token_type::left_paren);
    tokens.consume_comma_separated_list(token_type::right_paren, [&]{
        auto param = node_parameter{};
        param.name = parse_name(tokens);
        tokens.consume_only(token_type::colon);
        param.type = parse_expression(tokens);
        stmt.sig.params.push_back(param);
    });

    if (tokens.consume_maybe(token_type::arrow)) {
        stmt.sig.return_type = parse_expression(tokens);
    }
    stmt.body = parse_statement(tokens);
    return node;
}

auto parse_member_function_def_stmt(const std::string& struct_name, tokenstream& tokens)
    -> node_stmt_ptr
{
    auto node = std::make_shared<node_stmt>();
    auto& stmt = node->emplace<node_member_function_def_stmt>();

    stmt.token = tokens.consume_only(token_type::kw_function);
    stmt.struct_name = struct_name;
    stmt.function_name = parse_name(tokens);

    if (tokens.consume_maybe(token_type::bang)) {
        tokens.consume_only(token_type::left_paren);
        tokens.consume_comma_separated_list(token_type::right_paren, [&]{
            stmt.template_types.push_back(parse_name(tokens));
        });
    }

    tokens.consume_only(token_type::left_paren);
    tokens.consume_comma_separated_list(token_type::right_paren, [&]{
        auto param = node_parameter{};
        param.name = parse_name(tokens);
        tokens.consume_only(token_type::colon);
        param.type = parse_expression(tokens);
        stmt.sig.params.push_back(param);
    });
    if (tokens.consume_maybe(token_type::arrow)) {
        stmt.sig.return_type = parse_expression(tokens);
    }
    stmt.body = parse_statement(tokens);
    return node;
}

auto parse_return_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_shared<node_stmt>();
    auto& stmt = node->emplace<node_return_stmt>();

    stmt.token = tokens.consume_only(token_type::kw_return);
    if (tokens.peek(token_type::semicolon)) {
        stmt.return_value = std::make_shared<node_expr>();
        auto& ret_expr = stmt.return_value->emplace<node_literal_null_expr>();
        ret_expr.token = stmt.token;
    } else {
        stmt.return_value = parse_expression(tokens);
    }
    tokens.consume_only(token_type::semicolon);
    return node;
}

auto parse_loop_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_shared<node_stmt>();
    auto& stmt = node->emplace<node_loop_stmt>();

    stmt.token = tokens.consume_only(token_type::kw_loop);
    stmt.body = parse_statement(tokens);
    return node;
}

auto parse_while_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_shared<node_stmt>();
    auto& stmt = node->emplace<node_while_stmt>();

    stmt.token = tokens.consume_only(token_type::kw_while);
    stmt.condition = parse_expression(tokens);
    stmt.body = parse_statement(tokens);
    return node;
}

auto parse_for_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_shared<node_stmt>();
    auto& stmt = node->emplace<node_for_stmt>();

    stmt.token = tokens.consume_only(token_type::kw_for);
    stmt.name = parse_name(tokens);
    tokens.consume_only(token_type::kw_in);
    stmt.iter = parse_expression(tokens);
    stmt.body = parse_statement(tokens);
    return node;
}

auto parse_if_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_shared<node_stmt>();
    auto& stmt = node->emplace<node_if_stmt>();

    stmt.token = tokens.consume_only(token_type::kw_if);
    stmt.condition = parse_expression(tokens);
    stmt.body = parse_statement(tokens);
    if (tokens.consume_maybe(token_type::kw_else)) {
        stmt.else_body = parse_statement(tokens);
    }
    return node;
}

auto parse_struct_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_shared<node_stmt>();
    auto& stmt = node->emplace<node_struct_stmt>();

    stmt.token = tokens.consume_only(token_type::kw_struct);
    stmt.name = parse_name(tokens);
    tokens.consume_only(token_type::left_brace);
    while (!tokens.consume_maybe(token_type::right_brace)) {
        if (tokens.peek(token_type::kw_function)) {
            stmt.functions.emplace_back(parse_member_function_def_stmt(stmt.name, tokens));
        } else {
            stmt.fields.emplace_back();
            auto& f = stmt.fields.back();
            f.name = parse_name(tokens);
            tokens.consume_only(token_type::colon);
            f.type = parse_expression(tokens);
            tokens.consume_only(token_type::semicolon);
        }
    }

    return node;
}

auto parse_declaration_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_shared<node_stmt>();
    auto& stmt = node->emplace<node_declaration_stmt>();

    stmt.token = tokens.consume();

    switch (stmt.token.type) {
        case token_type::kw_let: { stmt.add_const = true; } break;
        case token_type::kw_var: { stmt.add_const = false; } break;
        default: stmt.token.error("declaration must start with 'let' or 'var', not {}",
                                  stmt.token.text);
    }

    stmt.name = parse_name(tokens);
    if (tokens.consume_maybe(token_type::colon)) {
        stmt.explicit_type = parse_expression(tokens);
        tokens.consume_only(token_type::equal);
    } else {
        tokens.consume_only(token_type::colon_equal);
    }
    stmt.expr = parse_expression(tokens);
    tokens.consume_only(token_type::semicolon);
    return node;
}

auto parse_arena_declaration_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_shared<node_stmt>();
    auto& stmt = node->emplace<node_arena_declaration_stmt>();
    stmt.token = tokens.consume();
    stmt.name = parse_name(tokens);
    return node;
}

auto parse_print_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_shared<node_stmt>();
    auto& stmt = node->emplace<node_print_stmt>();

    stmt.token = tokens.consume_only(token_type::kw_print);
    tokens.consume_only(token_type::left_paren);
    const auto message_token = tokens.consume_only(token_type::string);
    stmt.message = std::string{message_token.text};
    if (tokens.consume_maybe(token_type::comma)) {
        tokens.consume_comma_separated_list(token_type::right_paren, [&] {
            stmt.args.push_back(parse_expression(tokens));
        });
    } else {
        tokens.consume_only(token_type::right_paren);
    }
    return node;
}

auto parse_braced_statement_list(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_shared<node_stmt>();
    auto& stmt = node->emplace<node_sequence_stmt>();

    stmt.token = tokens.consume_only(token_type::left_brace);
    while (!tokens.consume_maybe(token_type::right_brace)) {
        stmt.sequence.push_back(parse_statement(tokens));
    }

    return node;
}

auto parse_assert_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_shared<node_stmt>();
    auto& stmt = node->emplace<node_assert_stmt>();

    stmt.token = tokens.consume_only(token_type::kw_assert);
    stmt.expr = parse_expression(tokens);
    tokens.consume_only(token_type::semicolon);
    return node;
}

auto parse_break_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto ret = std::make_shared<node_stmt>(node_break_stmt{ tokens.consume() });
    tokens.consume_only(token_type::semicolon);
    return ret;
}

auto parse_continue_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto ret = std::make_shared<node_stmt>(node_continue_stmt{ tokens.consume() });
    tokens.consume_only(token_type::semicolon);
    return ret;
}

auto parse_statement(tokenstream& tokens) -> node_stmt_ptr
{
    const auto drain_semicolons = scope_exit([&] {
        while (tokens.consume_maybe(token_type::semicolon));
    });

    const auto& curr = tokens.curr();
    switch (curr.type) {
        case token_type::kw_function: curr.error("functions can only exist in global scope");
        case token_type::kw_struct:   curr.error("structs can only exist in global scope");
        case token_type::kw_return:   return parse_return_stmt(tokens);
        case token_type::kw_loop:     return parse_loop_stmt(tokens);
        case token_type::kw_while:    return parse_while_stmt(tokens);
        case token_type::kw_for:      return parse_for_stmt(tokens);
        case token_type::kw_if:       return parse_if_stmt(tokens);
        case token_type::kw_assert:   return parse_assert_stmt(tokens);
        case token_type::kw_break:    return parse_break_stmt(tokens);
        case token_type::kw_continue: return parse_continue_stmt(tokens);
        case token_type::left_brace:  return parse_braced_statement_list(tokens);
        case token_type::kw_let:
        case token_type::kw_var:      return parse_declaration_stmt(tokens);
        case token_type::kw_arena:    return parse_arena_declaration_stmt(tokens);
        case token_type::kw_print:    return parse_print_stmt(tokens);
    }

    auto node = std::make_shared<node_stmt>();
    auto expr = parse_expression(tokens);
    if (tokens.peek(token_type::equal)) {
        auto& stmt = node->emplace<node_assignment_stmt>();
        stmt.token = tokens.consume();
        stmt.position = expr;
        stmt.expr = parse_expression(tokens);
    } else {
        auto& stmt = node->emplace<node_expression_stmt>();
        stmt.token = std::visit([](auto&& n) { return n.token; }, *expr);
        stmt.expr = expr;
    }
    tokens.consume_only(token_type::semicolon);
    return node;
}

auto parse_top_level_statement(tokenstream& tokens) -> node_stmt_ptr
{
    const auto drain_semicolons = scope_exit([&] {
        while (tokens.consume_maybe(token_type::semicolon));
    });
    if (!tokens.valid()) return nullptr;

    const auto& curr = tokens.curr();
    switch (curr.type) {
        case token_type::kw_function: return parse_function_def_stmt(tokens);
        case token_type::kw_struct:   return parse_struct_stmt(tokens);
        default:                      return parse_statement(tokens);
    }
}

}

auto parse(const std::filesystem::path& file) -> anzu_module
{
    auto new_module = anzu_module{};
    new_module.source_code = anzu::read_file(file);
    new_module.root = std::make_shared<node_stmt>();
    auto& seq = new_module.root->emplace<node_sequence_stmt>();

    auto stream = tokenstream{*new_module.source_code};
    while (stream.valid()) {
        while (stream.consume_maybe(token_type::semicolon));
        seq.sequence.push_back(parse_top_level_statement(stream));
    }
    return new_module;
}

}