#include "parse_expression.hpp"

namespace anzu {
namespace {

enum precedence {
  PREC_NONE,
  PREC_ASSIGNMENT,  // =
  PREC_OR,          // or
  PREC_AND,         // and
  PREC_EQUALITY,    // == !=
  PREC_COMPARISON,  // < > <= >=
  PREC_TERM,        // + -
  PREC_FACTOR,      // * /
  PREC_UNARY,       // ! -
  PREC_CALL,        // . ()
  PREC_PRIMARY
};

using parse_func = auto(*) (tokenstream&) -> node_expr_ptr;
struct parse_rule
{
    parse_func prefix;
    parse_func infix;
    precedence prec;
};

auto parse_precedence(tokenstream& tokens, precedence prec) -> node_expr_ptr;
auto get_rule(token_type tt) -> const parse_rule*;

template <typename ExprType, token_type TokenType>
auto parse_number(tokenstream& tokens) -> node_expr_ptr
{
    const auto tok = tokens.consume_only(TokenType);
    auto node = std::make_shared<node_expr>();
    auto& inner = node->emplace<ExprType>();
    inner.token = tok;
    auto text = tok.text;

    const auto [ptr, ec] = std::from_chars(text.data(), text.data() + text.size(), inner.value);
    tok.assert(ec == std::errc{}, "cannot convert '{}' to '{}'\n", text, TokenType);
    return node;
}

auto parse_i32(tokenstream& tokens) -> node_expr_ptr
{
    return parse_number<node_literal_i32_expr, token_type::int32>(tokens);
}

auto parse_i64(tokenstream& tokens) -> node_expr_ptr
{
    return parse_number<node_literal_i64_expr, token_type::int64>(tokens);
}

auto parse_u64(tokenstream& tokens) -> node_expr_ptr
{
    return parse_number<node_literal_u64_expr, token_type::uint64>(tokens);
}

auto parse_f64(tokenstream& tokens) -> node_expr_ptr
{
    return parse_number<node_literal_f64_expr, token_type::float64>(tokens);
}

auto parse_char(tokenstream& tokens) -> node_expr_ptr
{
    const auto tok = tokens.consume_only(token_type::character);
    auto node = std::make_shared<node_expr>();
    auto& inner = node->emplace<node_literal_char_expr>();
    inner.token = tok;
    inner.value = tok.text.front();
    return node;
}

auto parse_string(tokenstream& tokens) -> node_expr_ptr
{
    const auto tok = tokens.consume_only(token_type::string);
    auto node = std::make_shared<node_expr>();
    auto& inner = node->emplace<node_literal_string_expr>();
    inner.token = tok;
    inner.value = tok.text;
    return node;
}

auto parse_true(tokenstream& tokens) -> node_expr_ptr
{
    const auto tok = tokens.consume_only(token_type::kw_true);
    auto node = std::make_shared<node_expr>();
    auto& inner = node->emplace<node_literal_bool_expr>();
    inner.token = tok;
    inner.value = true;
    return node;
}

auto parse_false(tokenstream& tokens) -> node_expr_ptr
{
    const auto tok = tokens.consume_only(token_type::kw_false);
    auto node = std::make_shared<node_expr>();
    auto& inner = node->emplace<node_literal_bool_expr>();
    inner.token = tok;
    inner.value = false;
    return node;
}

auto parse_null(tokenstream& tokens) -> node_expr_ptr
{
    const auto tok = tokens.consume_only(token_type::kw_null);
    auto node = std::make_shared<node_expr>();
    auto& inner = node->emplace<node_literal_null_expr>();
    inner.token = tok;
    return node;
}

auto parse_nullptr(tokenstream& tokens) -> node_expr_ptr
{
    const auto tok = tokens.consume_only(token_type::kw_nullptr);
    auto node = std::make_shared<node_expr>();
    auto& inner = node->emplace<node_literal_nullptr_expr>();
    inner.token = tok;
    return node;
}

auto parse_grouping(tokenstream& tokens) -> node_expr_ptr
{
    tokens.consume_only(token_type::left_paren);
    const auto node = parse_expr(tokens);
    tokens.consume_only(token_type::right_paren);
    return node;
}

auto parse_unary(tokenstream& tokens) -> node_expr_ptr
{
    const auto op = tokens.consume();
    auto node = parse_precedence(tokens, precedence::PREC_UNARY);
    switch (op.type) {
        case token_type::minus: {
            auto new_node = std::make_shared<node_expr>();
            auto& inner = new_node->emplace<node_unary_op_expr>();
            inner.token = op;
            inner.expr = node;
            node = new_node;
        } break;
        default: break;
    }
    op.error("unrecognised unary op '{}'", op.type);
}

auto parse_binary(tokenstream& tokens) -> node_expr_ptr
{
    const auto op = tokens.consume();
    auto rule = get_rule(op.type);
    //auto node = parse_precedence(rule->precedence + 1);
    return nullptr;
}

auto parse_precedence(tokenstream& tokens, precedence prec) -> node_expr_ptr
{
    return nullptr;
}

static const auto rules = std::unordered_map<token_type, parse_rule>
{
    {token_type::left_paren, parse_rule{parse_grouping, nullptr,      precedence::PREC_NONE}},
    {token_type::minus,      parse_rule{parse_unary,    parse_binary, precedence::PREC_TERM}},
    {token_type::plus,       parse_rule{nullptr,        parse_binary, precedence::PREC_TERM}},
    {token_type::slash,      parse_rule{nullptr,        parse_binary, precedence::PREC_FACTOR}},
    {token_type::star,       parse_rule{nullptr,        parse_binary, precedence::PREC_FACTOR}},
    {token_type::int32,      parse_rule{parse_i32,      nullptr,      precedence::PREC_NONE}},
    {token_type::int64,      parse_rule{parse_i64,      nullptr,      precedence::PREC_NONE}},
    {token_type::uint64,     parse_rule{parse_u64,      nullptr,      precedence::PREC_NONE}},
    {token_type::float64,    parse_rule{parse_f64,      nullptr,      precedence::PREC_NONE}},
    {token_type::character,  parse_rule{parse_char,     nullptr,      precedence::PREC_NONE}},
    {token_type::kw_true,    parse_rule{parse_true,     nullptr,      precedence::PREC_NONE}},
    {token_type::kw_false,   parse_rule{parse_false,    nullptr,      precedence::PREC_NONE}},
    {token_type::kw_null,    parse_rule{parse_null,     nullptr,      precedence::PREC_NONE}},
    {token_type::kw_nullptr, parse_rule{parse_nullptr,  nullptr,      precedence::PREC_NONE}}
};

auto get_rule(token_type tt) -> const parse_rule*
{
    if (rules.contains(tt)) return &rules.at(tt);
    return nullptr;
}


}

auto parse_expr(tokenstream& tokens) -> node_expr_ptr
{
    return parse_precedence(tokens, precedence::PREC_ASSIGNMENT);
}
    
}