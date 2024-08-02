#include "parse_expression.hpp"

namespace anzu {
namespace {

enum precedence : int {
  PREC_NONE,
  PREC_ASSIGNMENT,  // =
  PREC_OR,          // or
  PREC_AND,         // and
  PREC_EQUALITY,    // == !=
  PREC_COMPARISON,  // < > <= >=
  PREC_TERM,        // + -
  PREC_FACTOR,      // * /
  PREC_UNARY,       // ! -
  PREC_CALL,        // . () [] | @
  PREC_SCOPE,       // ::
  PREC_PRIMARY
};

using prefix_func = auto(*) (tokenstream&) -> node_expr_ptr;
using midfix_func = auto(*) (tokenstream&, const node_expr_ptr&) -> node_expr_ptr;
struct parse_rule
{
    prefix_func prefix;
    midfix_func midfix;
    precedence  prec;
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
    //std::print("parse_i32\n");
    return parse_number<node_literal_i32_expr, token_type::int32>(tokens);
}

auto parse_i64(tokenstream& tokens) -> node_expr_ptr
{
    //std::print("parse_i64\n");
    return parse_number<node_literal_i64_expr, token_type::int64>(tokens);
}

auto parse_u64(tokenstream& tokens) -> node_expr_ptr
{
    //std::print("parse_u64\n");
    return parse_number<node_literal_u64_expr, token_type::uint64>(tokens);
}

auto parse_f64(tokenstream& tokens) -> node_expr_ptr
{
    //std::print("parse_f64\n");
    return parse_number<node_literal_f64_expr, token_type::float64>(tokens);
}

auto parse_char(tokenstream& tokens) -> node_expr_ptr
{
    //std::print("parse_char\n");
    const auto tok = tokens.consume_only(token_type::character);
    auto node = std::make_shared<node_expr>();
    auto& inner = node->emplace<node_literal_char_expr>();
    inner.token = tok;
    inner.value = tok.text.front();
    return node;
}

auto parse_string(tokenstream& tokens) -> node_expr_ptr
{
    //std::print("parse_string\n");
    const auto tok = tokens.consume_only(token_type::string);
    auto node = std::make_shared<node_expr>();
    auto& inner = node->emplace<node_literal_string_expr>();
    inner.token = tok;
    inner.value = tok.text;
    return node;
}

auto parse_true(tokenstream& tokens) -> node_expr_ptr
{
    //std::print("parse_true\n");
    const auto tok = tokens.consume_only(token_type::kw_true);
    auto node = std::make_shared<node_expr>();
    auto& inner = node->emplace<node_literal_bool_expr>();
    inner.token = tok;
    inner.value = true;
    return node;
}

auto parse_false(tokenstream& tokens) -> node_expr_ptr
{
    //std::print("parse_false\n");
    const auto tok = tokens.consume_only(token_type::kw_false);
    auto node = std::make_shared<node_expr>();
    auto& inner = node->emplace<node_literal_bool_expr>();
    inner.token = tok;
    inner.value = false;
    return node;
}

auto parse_null(tokenstream& tokens) -> node_expr_ptr
{
    //std::print("parse_null\n");
    const auto tok = tokens.consume_only(token_type::kw_null);
    auto node = std::make_shared<node_expr>();
    auto& inner = node->emplace<node_literal_null_expr>();
    inner.token = tok;
    return node;
}

auto parse_nullptr(tokenstream& tokens) -> node_expr_ptr
{
    //std::print("parse_nullptr\n");
    const auto tok = tokens.consume_only(token_type::kw_nullptr);
    auto node = std::make_shared<node_expr>();
    auto& inner = node->emplace<node_literal_nullptr_expr>();
    inner.token = tok;
    return node;
}

auto parse_name(tokenstream& tokens) -> node_expr_ptr
{
    //std::print("parse_name\n");
    const auto tok = tokens.consume_only(token_type::identifier);
    auto node = std::make_shared<node_expr>();
    auto& inner = node->emplace<node_name_expr>();
    inner.token = tok;
    inner.name = std::string{tok.text};
    return node;
}

auto parse_grouping(tokenstream& tokens) -> node_expr_ptr
{
    //std::print("parse_grouping\n");
    tokens.consume_only(token_type::left_paren);
    const auto node = parse_expr(tokens);
    tokens.consume_only(token_type::right_paren);
    return node;
}

auto parse_unary(tokenstream& tokens) -> node_expr_ptr
{
    //std::print("parse_unary\n");
    const auto op = tokens.consume();
    auto inner = parse_precedence(tokens, precedence::PREC_UNARY);

    auto node = std::make_shared<node_expr>();
    auto& unary = node->emplace<node_unary_op_expr>();
    unary.token = op;
    unary.expr = inner;
    return node;
}

auto parse_typeof(tokenstream& tokens) -> node_expr_ptr
{
    //std::print("parse_typeof\n");
    const auto tok = tokens.consume_only(token_type::kw_typeof);
    tokens.consume_only(token_type::left_paren);
    auto node = std::make_shared<node_expr>();
    auto& inner = node->emplace<node_typeof_expr>();
    inner.token = tok;
    inner.expr = parse_expr(tokens);
    tokens.consume_only(token_type::right_paren);
    return node;
}

auto parse_sizeof(tokenstream& tokens) -> node_expr_ptr
{
    //std::print("parse_sizeof\n");
    const auto tok = tokens.consume_only(token_type::kw_sizeof);
    tokens.consume_only(token_type::left_paren);
    auto node = std::make_shared<node_expr>();
    auto& inner = node->emplace<node_sizeof_expr>();
    inner.token = tok;
    inner.expr = parse_expr(tokens);
    tokens.consume_only(token_type::right_paren);
    return node;
}

auto parse_binary(tokenstream& tokens, const node_expr_ptr& left) -> node_expr_ptr
{
    //std::print("parse_binary\n");
    const auto op = tokens.consume();
    auto rule = get_rule(op.type);
    auto right = parse_precedence(tokens, (precedence)(rule->prec + 1));

    auto node = std::make_shared<node_expr>();
    auto& binary = node->emplace<node_binary_op_expr>();
    binary.token = op;
    binary.lhs = left;
    binary.rhs = right;
    return node;
}

auto parse_call(tokenstream& tokens, const node_expr_ptr& left) -> node_expr_ptr
{
    //std::print("parse_call\n");
    auto node = std::make_shared<node_expr>();
    auto& call = node->emplace<node_call_expr>();
    call.token = tokens.curr();
    call.expr = left;
    if (tokens.consume_maybe(token_type::left_brace)) {
        tokens.consume_comma_separated_list(token_type::right_brace, [&] {
            call.template_args.push_back(parse_expr(tokens));
        });
    }
    tokens.consume_only(token_type::left_paren);
    tokens.consume_comma_separated_list(token_type::right_paren, [&] {
        call.args.push_back(parse_expr(tokens));
    });
    
    return node;
}

auto parse_precedence(tokenstream& tokens, precedence prec) -> node_expr_ptr
{
    //std::print("parse_precedence\n");
    const auto token = tokens.curr();
    auto rule = get_rule(token.type);
    token.assert(rule->prefix, "expected an expression");

    auto node = rule->prefix(tokens);
    while (prec <= get_rule(tokens.curr().type)->prec) {
        auto midfix = get_rule(tokens.curr().type)->midfix;
        node = midfix(tokens, node);
    }

    return node;
}

static const auto rules = std::unordered_map<token_type, parse_rule>
{
    {token_type::left_paren,          {parse_grouping, parse_call,   precedence::PREC_CALL}},
    {token_type::left_brace,          {nullptr,        parse_call,   precedence::PREC_CALL}},
    {token_type::bang,                {parse_unary,    nullptr,      precedence::PREC_NONE}},
    {token_type::minus,               {parse_unary,    parse_binary, precedence::PREC_TERM}},
    {token_type::plus,                {nullptr,        parse_binary, precedence::PREC_TERM}},
    {token_type::slash,               {nullptr,        parse_binary, precedence::PREC_FACTOR}},
    {token_type::star,                {nullptr,        parse_binary, precedence::PREC_FACTOR}},
    {token_type::int32,               {parse_i32,      nullptr,      precedence::PREC_NONE}},
    {token_type::int64,               {parse_i64,      nullptr,      precedence::PREC_NONE}},
    {token_type::uint64,              {parse_u64,      nullptr,      precedence::PREC_NONE}},
    {token_type::float64,             {parse_f64,      nullptr,      precedence::PREC_NONE}},
    {token_type::character,           {parse_char,     nullptr,      precedence::PREC_NONE}},
    {token_type::kw_true,             {parse_true,     nullptr,      precedence::PREC_NONE}},
    {token_type::kw_false,            {parse_false,    nullptr,      precedence::PREC_NONE}},
    {token_type::kw_null,             {parse_null,     nullptr,      precedence::PREC_NONE}},
    {token_type::kw_nullptr,          {parse_nullptr,  nullptr,      precedence::PREC_NONE}},
    {token_type::equal_equal,         {nullptr,        parse_binary, precedence::PREC_EQUALITY}},
    {token_type::bang_equal,          {nullptr,        parse_binary, precedence::PREC_EQUALITY}},
    {token_type::less,                {nullptr,        parse_binary, precedence::PREC_COMPARISON}},
    {token_type::less_equal,          {nullptr,        parse_binary, precedence::PREC_COMPARISON}},
    {token_type::greater,             {nullptr,        parse_binary, precedence::PREC_COMPARISON}},
    {token_type::greater_equal,       {nullptr,        parse_binary, precedence::PREC_COMPARISON}},
    {token_type::ampersand_ampersand, {nullptr,        parse_binary, precedence::PREC_AND}},
    {token_type::bar_bar,             {nullptr,        parse_binary, precedence::PREC_OR}},
    {token_type::identifier,          {parse_name,     nullptr,      precedence::PREC_NONE}},
    {token_type::equal,               {nullptr,        parse_binary, precedence::PREC_ASSIGNMENT}},
    {token_type::kw_typeof,           {parse_typeof,   nullptr,      precedence::PREC_NONE}},
    {token_type::kw_sizeof,           {parse_sizeof,   nullptr,      precedence::PREC_NONE}}
};
static constexpr auto default_rule = parse_rule{nullptr, nullptr, precedence::PREC_NONE};

auto get_rule(token_type tt) -> const parse_rule*
{
    if (rules.contains(tt)) return &rules.at(tt);
    return &default_rule;
}

}

auto parse_expr(tokenstream& tokens) -> node_expr_ptr
{
    //std::print("parse_expr\n");
    return parse_precedence(tokens, precedence::PREC_ASSIGNMENT);
}
    
}