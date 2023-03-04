#include "parser.hpp"
#include "object.hpp"
#include "functions.hpp"

#include <unordered_set>
#include <string_view>
#include <vector>
#include <memory>
#include <charconv>

namespace anzu {
namespace {

auto parse_i32(const token& tok) -> object
{
    auto text = tok.text;

    tok.assert(text.ends_with(i32_sv), "expected suffix '{}'\n", i32_sv);
    text.remove_suffix(i32_sv.size());
    
    auto result = std::int32_t{};
    const auto [ptr, ec] = std::from_chars(text.data(), text.data() + text.size(), result);
    tok.assert(ec == std::errc{}, "cannot convert '{}' to '{}'\n", text, i32_sv);

    const auto bytes = as_bytes(result);
    return object{ .data={bytes.begin(), bytes.end()}, .type=i32_type() };
}

auto parse_i64(const token& tok) -> object
{
    auto text = tok.text;

    if (text.ends_with(i64_sv)) {
        text.remove_suffix(i64_sv.size());
    }

    auto result = std::int64_t{};
    const auto [ptr, ec] = std::from_chars(text.data(), text.data() + text.size(), result);
    tok.assert(ec == std::errc{}, "cannot convert '{}' to '{}'\n", text, i64_sv);

    const auto bytes = as_bytes(result);
    return object{ .data={bytes.begin(), bytes.end()}, .type=i64_type() };
}

auto parse_u64(const token& tok) -> object
{
    auto text = std::string_view{tok.text};

    if (text.ends_with(u64_sv)) {
        text.remove_suffix(u64_sv.size());
    } else if (text.ends_with('u')) {
        text.remove_suffix(1);
    }

    auto result = std::uint64_t{};
    const auto [ptr, ec] = std::from_chars(text.data(), text.data() + text.size(), result);
    tok.assert(ec == std::errc{}, "cannot convert '{}' to '{}'\n", text, u64_sv);

    const auto bytes = as_bytes(result);
    return object{ .data={bytes.begin(), bytes.end()}, .type=u64_type() };
}

auto parse_f64(const token& tok) -> object
{
    auto text = std::string_view{tok.text};

    auto result = double{};
    const auto [ptr, ec] = std::from_chars(text.data(), text.data() + text.size(), result);
    tok.assert(ec == std::errc{}, "cannot convert '{}' to '{}'\n", text, f64_sv);

    const auto bytes = as_bytes(result);
    return object{ .data={bytes.begin(), bytes.end()}, .type=f64_type() };
}

auto parse_char(const token& tok) -> object
{
    tok.assert_eq(tok.text.size(), 1, "failed to parse char, wrong size");
    const auto bytes = as_bytes(tok.text.front());
    return object{ .data={bytes.begin(), bytes.end()}, .type=char_type() };
}

auto parse_expression(tokenstream& tokens) -> node_expr_ptr;
auto parse_statement(tokenstream& tokens) -> node_stmt_ptr;
auto parse_type(tokenstream& tokens) -> type_name;
auto parse_type_node(tokenstream& tokens) -> node_type_ptr;

auto parse_literal(tokenstream& tokens) -> object
{
    const auto token = tokens.consume();
    switch (token.type) {
        case token_type::int32:     return parse_i32(token);
        case token_type::int64:     return parse_i64(token);
        case token_type::uint64:    return parse_u64(token);
        case token_type::float64:   return parse_f64(token);
        case token_type::character: return parse_char(token);
        case token_type::kw_true:   return object{ .data={std::byte{1}}, .type=bool_type() };
        case token_type::kw_false:  return object{ .data={std::byte{0}}, .type=bool_type() };
        case token_type::kw_null:   return object{ .data={std::byte{0}}, .type=null_type() };
        case token_type::string: {
            auto ret = object{};
            for (char c : token.text) {
                ret.data.push_back(static_cast<std::byte>(c));
            }
            ret.type = concrete_list_type(char_type(), token.text.size());
            return ret;
        }
    }
    token.error("failed to parse literal ({})", token.text);
};

auto precedence_table()
{
    using tt = token_type;
    auto table = std::array<std::unordered_set<tt>, 6>{};
    table[0] = {tt::bar_bar}; // or
    table[1] = {tt::ampersand_ampersand}; // and
    table[2] = {tt::equal_equal, tt::bang_equal}; // == !=
    table[3] = {tt::less, tt::less_equal, tt::greater, tt::greater_equal}; // < <= > >=
    table[4] = {tt::plus, tt::minus}; // + -
    table[5] = {tt::star, tt::slash, tt::percent}; // * / %
    return table;
}
static const auto bin_ops_table = precedence_table();

auto parse_member_access(tokenstream& tokens, node_expr_ptr& node)
{
    auto new_node = std::make_shared<node_expr>();
    const auto tok = tokens.consume();
    if (tokens.peek_next(token_type::left_paren)) {
        auto addrof = std::make_shared<node_expr>();
        auto& addrof_inner = addrof->emplace<node_addrof_expr>();
        addrof_inner.expr = node;
        addrof_inner.token = tok;
        auto& expr = new_node->emplace<node_call_expr>();
        expr.expr = std::make_shared<node_expr>(node_name_expr{
            .struct_name = std::make_shared<node_type>(node_expr_type{node}),
            .name = std::string{tokens.consume().text}
        });
        expr.token = tok;
        tokens.consume_only(token_type::left_paren);
        expr.args.push_back(addrof);
        tokens.consume_comma_separated_list(token_type::right_paren, [&] {
            expr.args.push_back(parse_expression(tokens));
        });
    } else {
        auto& expr = new_node->emplace<node_field_expr>();
        expr.token = tok;
        expr.field_name = tokens.consume().text;
        expr.expr = node;
    }
    node = new_node;
}

auto parse_single_factor(tokenstream& tokens) -> node_expr_ptr
{
    auto node = std::make_shared<node_expr>();
    
    if (tokens.consume_maybe(token_type::left_paren)) {
        node = parse_expression(tokens);
        tokens.consume_only(token_type::right_paren);
    }
    else if (tokens.peek(token_type::left_bracket)) {
        const auto tok = tokens.consume();
        auto first = parse_expression(tokens);
        if (tokens.consume_maybe(token_type::semicolon)) {
            auto& expr = node->emplace<node_repeat_list_expr>();
            expr.token = tok;
            expr.value = first;
            expr.size = tokens.consume_u64();
            tokens.consume_only(token_type::right_bracket);
        } else {
            auto& expr = node->emplace<node_list_expr>();
            expr.token = tok;
            expr.elements.push_back(first);
            if (tokens.consume_maybe(token_type::comma)) {
                tokens.consume_comma_separated_list(token_type::right_bracket, [&] {
                    expr.elements.push_back(parse_expression(tokens));
                });
            } else {
                tokens.consume_only(token_type::right_bracket);
            }
        }
    }
    else if (tokens.peek(token_type::minus) || tokens.peek(token_type::bang)) {
        auto& expr = node->emplace<node_unary_op_expr>();
        expr.token = tokens.consume();
        expr.expr = parse_single_factor(tokens);
    }
    else if (tokens.peek(token_type::ampersand)) {
        auto& expr = node->emplace<node_addrof_expr>();
        expr.token = tokens.consume();
        expr.expr = parse_single_factor(tokens);
    }
    else if (tokens.peek(token_type::kw_sizeof)) {
        auto& expr = node->emplace<node_sizeof_expr>();
        expr.token = tokens.consume();
        tokens.consume_only(token_type::left_paren);
        expr.expr = parse_expression(tokens);
        tokens.consume_only(token_type::right_paren);
    }
    else if (tokens.peek(token_type::star)) {
        auto& expr = node->emplace<node_deref_expr>();
        expr.token = tokens.consume();
        expr.expr = parse_single_factor(tokens);
    }
    else if (tokens.peek(token_type::identifier)) {
        auto& expr = node->emplace<node_name_expr>();
        expr.token = tokens.consume();
        expr.name = expr.token.text;
    }
    else if (tokens.peek(token_type::kw_new)) {
        auto& expr = node->emplace<node_new_expr>();
        expr.token = tokens.consume();
        expr.type = parse_type_node(tokens);
        if (tokens.consume_maybe(token_type::colon)) {
            expr.size = parse_expression(tokens);
        } else {
            expr.size = nullptr;
        }
    }
    else {
        auto& expr = node->emplace<node_literal_expr>();
        expr.token = tokens.curr();
        expr.value = parse_literal(tokens);
    }

    // Handle postfix expressions, such as field access, arrow access and subscripts.
    while (true) {
        if (tokens.peek(token_type::dot)) {
            parse_member_access(tokens, node);
            continue;
        }

        // For x->y, parse as (*x).y by first wrapping x in a node_deref_expr
        if (tokens.peek(token_type::arrow)) {
            auto deref_node = std::make_shared<node_expr>();
            auto& deref_inner = deref_node->emplace<node_deref_expr>();
            deref_inner.token = std::visit([](auto&& n) { return n.token; }, *node);
            deref_inner.expr = node;
            node = deref_node;
            parse_member_access(tokens, node);
            continue;
        }

        // Subscript expression or a subspan access
        if (tokens.peek(token_type::left_bracket)) {
            const auto token = tokens.consume();
            auto new_node = std::make_shared<node_expr>();
            if (tokens.consume_maybe(token_type::right_bracket)) {
                auto& expr = new_node->emplace<node_span_expr>();
                expr.token = token;
                expr.expr = node;
                node = new_node;
            } else { // either a subspan or subscript access
                const auto inner_expr = parse_expression(tokens);
                if (tokens.consume_maybe(token_type::colon)) { // subspan
                    auto& expr = new_node->emplace<node_span_expr>();
                    expr.token = token;
                    expr.expr = node;
                    expr.lower_bound = inner_expr;
                    expr.upper_bound = parse_expression(tokens);
                    node = new_node;
                } else { // subscript access
                    auto& expr = new_node->emplace<node_subscript_expr>();
                    expr.token = token;
                    expr.index = inner_expr;
                    expr.expr = node;
                    node = new_node;
                }
                tokens.consume_only(token_type::right_bracket);
            }
            continue;
        }

        // Callable expressions
        if (tokens.peek(token_type::left_paren)) {
            auto new_node = std::make_shared<node_expr>();
            auto& inner = new_node->emplace<node_call_expr>();
            inner.token = tokens.consume();
            inner.expr = node;
            tokens.consume_comma_separated_list(token_type::right_paren, [&] {
                inner.args.push_back(parse_expression(tokens));
            });
            node = new_node;
            continue;
        }

        break;
    }

    return node;
}

// Level is the precendence level, the lower the number, the tighter the factors bind.
auto parse_compound_factor(tokenstream& tokens, std::int64_t level) -> node_expr_ptr
{
    if (level == std::ssize(bin_ops_table)) {
        return parse_single_factor(tokens);
    }

    auto factor = parse_compound_factor(tokens, level + 1);
    while (tokens.valid() && bin_ops_table[level].contains(tokens.curr().type)) {
        auto node = std::make_shared<node_expr>();
        auto& expr = node->emplace<node_binary_op_expr>();
        expr.lhs = factor;
        expr.token = tokens.consume();
        expr.rhs = parse_compound_factor(tokens, level + 1);
        factor = node;
    }
    return factor;
}

auto parse_expression(tokenstream& tokens) -> node_expr_ptr
{
    return parse_compound_factor(tokens, 0);
}

auto parse_name(tokenstream& tokens)
{
    const auto token = tokens.consume();
    if (token.type != token_type::identifier) {
        token.error("'{}' is not a valid name", token.text);
    }
    return token.text;   
}

auto parse_type(tokenstream& tokens) -> type_name
{
    if (tokens.consume_maybe(token_type::ampersand)) {
        return {type_ptr{ .inner_type={parse_type(tokens)} }};
    }
    if (tokens.consume_maybe(token_type::left_paren)) {
        auto ret = type_function_ptr{};
        tokens.consume_comma_separated_list(token_type::right_paren, [&]{
            ret.param_types.push_back(parse_type(tokens));
        });
        tokens.consume_only(token_type::arrow);
        ret.return_type = make_value<type_name>(parse_type(tokens));
        return ret;
    }

    auto type = type_name{type_simple{.name=std::string{tokens.consume().text}}};
    while (tokens.consume_maybe(token_type::left_bracket)) {
        if (tokens.consume_maybe(token_type::right_bracket)) {
            auto new_type = type_name{type_span{ .inner_type=type }};
            type = new_type;
        } else {
            auto new_type = type_name{type_list{
                .inner_type=type, .count=static_cast<std::size_t>(tokens.consume_u64())
            }};
            type = new_type;
            tokens.consume_only(token_type::right_bracket);
        }
    }
    return type;
}

auto parse_type_node(tokenstream& tokens) -> node_type_ptr
{
    if (tokens.peek(token_type::kw_typeof)) {
        auto node = std::make_shared<node_type>();
        auto& inner = node->emplace<node_expr_type>();
        inner.token = tokens.consume();
        tokens.consume_only(token_type::left_paren);
        inner.expr = parse_expression(tokens);
        tokens.consume_only(token_type::right_paren);
        return node;
    }

    const auto type = parse_type(tokens);
    return std::make_shared<node_type>(node_named_type{type});
}

auto parse_function_def_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_shared<node_stmt>();
    auto& stmt = node->emplace<node_function_def_stmt>();

    stmt.token = tokens.consume_only(token_type::kw_function);
    stmt.name = parse_name(tokens);
    tokens.consume_only(token_type::left_paren);
    tokens.consume_comma_separated_list(token_type::right_paren, [&]{
        auto param = node_parameter{};
        param.name = parse_name(tokens);
        tokens.consume_only(token_type::colon);
        param.type = parse_type_node(tokens);
        stmt.sig.params.push_back(param);
    });    
    if (tokens.consume_maybe(token_type::arrow)) {
        stmt.sig.return_type = parse_type_node(tokens);
    } else {
        stmt.sig.return_type = std::make_shared<node_type>(node_named_type{null_type()});
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
    
    tokens.consume_only(token_type::left_paren);
    tokens.consume_comma_separated_list(token_type::right_paren, [&]{
        auto param = node_parameter{};
        param.name = parse_name(tokens);
        tokens.consume_only(token_type::colon);
        param.type = parse_type_node(tokens);
        stmt.sig.params.push_back(param);
    });
    if (tokens.consume_maybe(token_type::arrow)) {
        stmt.sig.return_type = parse_type_node(tokens);
    } else {
        stmt.sig.return_type = std::make_shared<node_type>(node_named_type{null_type()});
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
        auto& ret_expr = stmt.return_value->emplace<node_literal_expr>();
        ret_expr.value = object{ .data={std::byte{0}}, .type=null_type() };
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
            f.type = parse_type_node(tokens);
            tokens.consume_only(token_type::semicolon);
        }
    }

    return node;
}

auto parse_declaration_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_shared<node_stmt>();
    auto& stmt = node->emplace<node_declaration_stmt>();

    stmt.name = parse_name(tokens);
    stmt.token = tokens.consume_only(token_type::colon_equal);
    stmt.expr = parse_expression(tokens);
    tokens.consume_only(token_type::semicolon);
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

auto parse_delete_stmt(tokenstream& tokens) -> node_stmt_ptr
{
    auto node = std::make_shared<node_stmt>();
    auto& stmt = node->emplace<node_delete_stmt>();

    stmt.token = tokens.consume_only(token_type::kw_delete);
    stmt.expr = parse_expression(tokens);
    if (tokens.consume_maybe(token_type::colon)) {
        stmt.size = parse_expression(tokens);
    } else {
        stmt.size = std::make_shared<node_expr>();
        auto& inner = stmt.size->emplace<node_literal_expr>();
        inner.value = parse_u64(token{.text="1u"});
    }
    tokens.consume_only(token_type::semicolon);

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
    while (tokens.consume_maybe(token_type::semicolon));
    if (!tokens.valid()) return nullptr;

    const auto& curr = tokens.curr();
    switch (curr.type) {
        case token_type::kw_function: curr.error("functions can only exist in global scope");
        case token_type::kw_struct:   curr.error("structs can only exist in global scope");
        case token_type::kw_return:   return parse_return_stmt(tokens);
        case token_type::kw_loop:     return parse_loop_stmt(tokens);
        case token_type::kw_while:    return parse_while_stmt(tokens);
        case token_type::kw_for:      return parse_for_stmt(tokens);
        case token_type::kw_if:       return parse_if_stmt(tokens);
        case token_type::kw_delete:   return parse_delete_stmt(tokens);
        case token_type::kw_assert:   return parse_assert_stmt(tokens);
        case token_type::kw_break:    return parse_break_stmt(tokens);
        case token_type::kw_continue: return parse_continue_stmt(tokens);
        case token_type::left_brace:  return parse_braced_statement_list(tokens);
    }

    if (tokens.peek(token_type::identifier) && tokens.peek_next(token_type::colon_equal)) {
        return parse_declaration_stmt(tokens);
    }

    auto node = std::make_shared<node_stmt>();
    auto expr = parse_expression(tokens);
    if (tokens.peek(token_type::equal)) {
        auto& stmt = node->emplace<node_assignment_stmt>();
        stmt.token = tokens.consume();
        stmt.position = expr;
        stmt.expr = parse_expression(tokens);
        tokens.consume_only(token_type::semicolon);
    } else {
        auto& stmt = node->emplace<node_expression_stmt>();
        stmt.token = std::visit([](auto&& n) { return n.token; }, *expr);
        stmt.expr = expr;
        tokens.consume_only(token_type::semicolon);
    }
    return node;
}

auto parse_top_level_statement(tokenstream& tokens) -> node_stmt_ptr
{
    while (tokens.consume_maybe(token_type::semicolon));
    if (tokens.peek(token_type::kw_function)) {
        return parse_function_def_stmt(tokens);
    }
    if (tokens.peek(token_type::kw_struct)) {
        return parse_struct_stmt(tokens);
    }
    return parse_statement(tokens);
}

}

auto parse(lex_result&& lex_res) -> parse_result
{
    auto res = parse_result{};
    res.source_file = std::move(lex_res.source_file);
    res.source_code = std::move(lex_res.source_code);

    auto stream = tokenstream{lex_res.tokens};
    res.root = std::make_shared<node_stmt>();
    auto& seq = res.root->emplace<node_sequence_stmt>();
    while (stream.valid()) {
        while (stream.consume_maybe(token_type::semicolon));
        if (stream.consume_maybe(token_type::kw_import)) {
            auto module_name = std::string{};
            while (!stream.peek(token_type::semicolon)) {
                module_name += stream.consume().text;
            }
            res.required_modules.emplace(
                std::filesystem::absolute(res.source_file.parent_path() / module_name)
            );
            stream.consume_only(token_type::semicolon);
        } else {
            seq.sequence.push_back(parse_top_level_statement(stream));
        }
    }
    return res;
}

}