#include "parser.hpp"
#include "functions.hpp"

#include <optional>

namespace anzu {

namespace {

template <typename... Args>
[[noreturn]] void parser_error(const parser_context& ctx, std::string_view msg, Args&&... args)
{
    const auto formatted_msg = std::format(msg, std::forward<Args>(args)...);
    anzu::print(
        "[Parser] ({}:{}) ERROR: {}\n",
        ctx.curr->line, ctx.curr->col, formatted_msg
    );
    std::exit(1);
}

void check_argc(
    const parser_context& ctx, std::string_view func, std::int64_t expected, std::int64_t actual
) {
    parser_error(ctx, "function '{}' expected {} args, got {}", func, expected, actual);
}

auto consume_maybe(parser_context& ctx, std::string_view tok) -> bool
{
    if (ctx.curr->text == tok) {
        ++ctx.curr; // skip end
        return true;
    }
    return false;
}

void consume_only(parser_context& ctx, std::string_view tok)
{
    if (!consume_maybe(ctx, tok)) {
        parser_error(ctx, "expected '{}', got '{}'", tok, ctx.curr->text);
    }
}

auto handle_list_literal(parser_context& ctx) -> anzu::object;

auto try_parse_literal(parser_context& ctx) -> std::optional<anzu::object>
{
    if (ctx.curr->type == token_type::number) {
        return { anzu::to_int((ctx.curr++)->text) };
    }
    else if (ctx.curr->type == token_type::string) {
        return { (ctx.curr++)->text };
    }
     else if (consume_maybe(ctx, TRUE_LIT)) {
        return { true };
    }
    else if (consume_maybe(ctx, FALSE_LIT)) {
        return { false };
    }
    else if (consume_maybe(ctx, NULL_LIT)) {
        return anzu::null_object();
    }
    else if (ctx.curr->text == "[") {
        return { handle_list_literal(ctx) };
    }
    return std::nullopt;
};

auto handle_list_literal(parser_context& ctx) -> anzu::object
{
    auto list = std::make_shared<std::vector<anzu::object>>();

    consume_only(ctx, "[");
    bool expect_comma = false;
    while (ctx.curr != ctx.end && ctx.curr->text != "]") {
        if (expect_comma) {
            if (ctx.curr->text != ",") { // skip commas, enforce later
                parser_error(ctx, "expected command in list literal");
            }
            ++ctx.curr;
        }
        else {
            if (auto obj = try_parse_literal(ctx); obj.has_value()) {
                list->push_back(*obj);            
            }
            else {
                parser_error(ctx, "failed to parse string literal");
            }
        }
        expect_comma = !expect_comma;
    }
    if (ctx.curr == ctx.end) {
        parser_error(ctx, "list literal nexer closed");
    }
    consume_only(ctx, "]");

    return { list };
}

auto precedence_table()
{
    auto table = std::array<std::unordered_set<std::string_view>, 6>{};
    table[0] = {};
    table[1] = {"*", "/", "%"};
    table[2] = {"+", "-"};
    table[3] = {"<", "<=", ">", ">=", "==", "!="};
    table[4] = {"&&"};
    table[5] = {"||"};
    return table;
}
static const auto bin_ops_table = precedence_table();

auto parse_expression(parser_context& ctx) -> node_expr_ptr;
auto parse_function_call_expr(parser_context& ctx) -> node_expr_ptr;
auto parse_builtin_call_expr(parser_context& ctx) -> node_expr_ptr;

auto parse_single_factor(parser_context& ctx) -> node_expr_ptr
{
    if (consume_maybe(ctx, "(")) {
        auto expr = parse_expression(ctx);
        consume_only(ctx, ")");
        return expr;
    }  
    else if (auto factor = anzu::try_parse_literal(ctx); factor.has_value()) {
        auto node = std::make_unique<anzu::node_expr>();
        auto& expr = node->emplace<anzu::node_literal_expr>();
        expr.value = *factor;
        return node;
    }
    else if (ctx.functions.contains(ctx.curr->text)) {
        return parse_function_call_expr(ctx);
    }
    else if (anzu::is_builtin(ctx.curr->text)) {
        return parse_builtin_call_expr(ctx);
    }
    else if (ctx.curr->type != token_type::name) {
        parser_error(ctx, "'{}' is not a name, cannot be used in an expresion", ctx.curr->text);
    }
    auto node = std::make_unique<anzu::node_expr>();
    auto& expr = node->emplace<anzu::node_variable_expr>();
    expr.name = (ctx.curr++)->text;
    return node;
}

// Level is the precendence level, the lower the number, the tighter the factors bind.
auto parse_compound_factor(parser_context& ctx, std::int64_t level) -> node_expr_ptr
{
    if (level == 0) {
        return parse_single_factor(ctx);
    }

    auto left = parse_compound_factor(ctx, level - 1);
    while (ctx.curr != ctx.end && bin_ops_table[level].contains(ctx.curr->text)) {
        auto op = (ctx.curr++)->text;

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

auto parse_assign_expression(parser_context& ctx) -> node_stmt_ptr
{
    if (ctx.curr->type != token_type::name) {
        parser_error(ctx, "cannot assign to '{}'", ctx.curr->text);
    }
    auto name = (ctx.curr++)->text;
    consume_only(ctx, "=");

    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_assignment_stmt>();
    stmt.name = name;
    stmt.expr = parse_expression(ctx);
    return node;
}

auto parse_statement(parser_context& ctx) -> node_stmt_ptr;
auto parse_statement_list(parser_context& ctx) -> node_stmt_ptr
{
    static const auto sentinel = std::unordered_set<std::string_view>{"end", "elif", "do", "else"};

    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_sequence_stmt>();
    while (ctx.curr != ctx.end && !sentinel.contains(ctx.curr->text)) {
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
    stmt.condition = parse_expression(ctx);
    consume_only(ctx, "do");
    stmt.body = parse_statement_list(ctx);
    consume_only(ctx, "end");
    return node;
}

auto parse_if_body(parser_context& ctx) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_if_stmt>();
    stmt.condition = parse_expression(ctx);
    consume_only(ctx, "do");
    stmt.body = parse_statement_list(ctx);

    if (consume_maybe(ctx, "elif")) {
        stmt.else_body = parse_if_body(ctx);
    }
    else if (consume_maybe(ctx, "else")) {
        stmt.else_body = parse_statement_list(ctx);
        consume_only(ctx, "end");
    }
    else {
        consume_only(ctx, "end");
    }
    
    return node;
}

auto parse_for_body(parser_context& ctx) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_for_stmt>();
    if (ctx.curr->type != token_type::name) {
        parser_error(ctx, "invalid for loop, bind target must be a name");
    }
    stmt.var = (ctx.curr++)->text;
    consume_only(ctx, "in");
    stmt.container = parse_expression(ctx); // TODO: When we have static typing, check this is a list
    consume_only(ctx, "do");
    stmt.body = parse_statement_list(ctx);
    consume_only(ctx, "end");
    return node;
}

auto parse_function_def(parser_context& ctx) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_function_def_stmt>();
    if (ctx.curr->type != token_type::name) {
        parser_error(ctx, "expected function name");
    }
    stmt.name = (ctx.curr++)->text;
    consume_only(ctx, "(");
    bool expect_comma = false;
    while (ctx.curr != ctx.end && ctx.curr->text != ")") {
        if (expect_comma) {
            if (ctx.curr->text != ",") { // skip commas, enforce later
                parser_error(ctx, "expected comma in function name list");
            }
            ++ctx.curr;
        }
        else {
            if (ctx.curr->type == token_type::name) {
                stmt.arg_names.push_back(ctx.curr->text);
                ++ctx.curr;          
            }
            else {
                parser_error(ctx, "failed to parse function signature");
            }
        }
        expect_comma = !expect_comma;
    }
    ctx.functions[stmt.name] = { .argc=std::ssize(stmt.arg_names) };
    consume_only(ctx, ")");
    consume_only(ctx, "do");
    stmt.body = parse_statement_list(ctx);
    consume_only(ctx, "end");
    return node;
}

auto parse_return(parser_context& ctx) -> node_stmt_ptr
{
    static const auto sentinel = std::unordered_set<std::string_view>{"end", "elif", "do", "else"};

    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_return_stmt>();
    
    if (!sentinel.contains(ctx.curr->text)) {
        stmt.return_value = parse_expression(ctx);
    } else {
        stmt.return_value = std::make_unique<anzu::node_expr>();
        stmt.return_value->emplace<anzu::node_literal_expr>().value = anzu::null_object();
    }
    return node;
}

auto parse_builtin_call_expr(parser_context& ctx) -> node_expr_ptr
{
    auto node = std::make_unique<anzu::node_expr>();
    auto& expr = node->emplace<anzu::node_builtin_call_expr>();
    expr.function_name = (ctx.curr++)->text;

    consume_only(ctx, "(");
    bool expect_comma = false;
    while (ctx.curr != ctx.end && ctx.curr->text != ")") {
        if (expect_comma) {
            if (ctx.curr->text != ",") { // skip commas, enforce later
                parser_error(ctx, "expected comma in function call arg list");
            }
            ++ctx.curr;
        }
        else {
            expr.args.push_back(parse_expression(ctx));
        }
        expect_comma = !expect_comma;
    }
    consume_only(ctx, ")");

    const auto argc = anzu::fetch_builtin_argc(expr.function_name);
    check_argc(ctx, expr.function_name, argc, std::ssize(expr.args));
    return node;
}

auto parse_builtin_call_stmt(parser_context& ctx) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_builtin_call_stmt>();
    stmt.function_name = (ctx.curr++)->text;

    consume_only(ctx, "(");
    bool expect_comma = false;
    while (ctx.curr != ctx.end && ctx.curr->text != ")") {
        if (expect_comma) {
            if (ctx.curr->text != ",") { // skip commas, enforce later
                parser_error(ctx, "expected comma in function call arg list");
            }
            ++ctx.curr;
        }
        else {
            stmt.args.push_back(parse_expression(ctx));
        }
        expect_comma = !expect_comma;
    }
    consume_only(ctx, ")");

    const auto argc = anzu::fetch_builtin_argc(stmt.function_name);
    check_argc(ctx, stmt.function_name, argc, std::ssize(stmt.args));
    return node;
}

auto parse_function_call_expr(parser_context& ctx) -> node_expr_ptr
{
    auto node = std::make_unique<anzu::node_expr>();
    auto& expr = node->emplace<anzu::node_function_call_expr>();
    expr.function_name = (ctx.curr++)->text;

    consume_only(ctx, "(");
    bool expect_comma = false;
    while (ctx.curr != ctx.end && ctx.curr->text != ")") {
        if (expect_comma) {
            if (ctx.curr->text != ",") { // skip commas, enforce later
                parser_error(ctx, "expected comma in function call arg list");
            }
            ++ctx.curr;
        }
        else {
            expr.args.push_back(parse_expression(ctx));
        }
        expect_comma = !expect_comma;
    }
    consume_only(ctx, ")");

    const auto argc = ctx.functions.at(expr.function_name).argc;
    check_argc(ctx, expr.function_name, argc, std::ssize(expr.args));
    return node;
}

auto parse_function_call_stmt(parser_context& ctx) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_function_call_stmt>();
    stmt.function_name = (ctx.curr++)->text;

    consume_only(ctx, "(");
    bool expect_comma = false;
    while (ctx.curr != ctx.end && ctx.curr->text != ")") {
        if (expect_comma) {
            if (ctx.curr->text != ",") { // skip commas, enforce later
                parser_error(ctx, "expected comma in function call arg list");
            }
            ++ctx.curr;
        }
        else {
            stmt.args.push_back(parse_expression(ctx));
        }
        expect_comma = !expect_comma;
    }
    consume_only(ctx, ")");

    const auto argc = ctx.functions.at(stmt.function_name).argc;
    check_argc(ctx, stmt.function_name, argc, std::ssize(stmt.args));
    return node;
}

auto parse_statement(parser_context& ctx) -> node_stmt_ptr
{
    if (consume_maybe(ctx, "function")) {
        return parse_function_def(ctx);
    }
    else if (consume_maybe(ctx, "return")) {
        return parse_return(ctx);
    }
    else if (consume_maybe(ctx, "while")) {
        return parse_while_body(ctx);
    }
    else if (consume_maybe(ctx, "if")) {
        return parse_if_body(ctx);
    }
    else if (consume_maybe(ctx, "for")) {
        return parse_for_body(ctx);
    }
    else if (consume_maybe(ctx, "break")) {
        auto node = std::make_unique<anzu::node_stmt>();
        node->emplace<anzu::node_break_stmt>();
        return node;
    }
    else if (consume_maybe(ctx, "continue")) {
        auto node = std::make_unique<anzu::node_stmt>();
        node->emplace<anzu::node_continue_stmt>();
        return node;
    }
    else if (auto next = std::next(ctx.curr); next != ctx.end && next->text == "=") {
        return parse_assign_expression(ctx);
    }
    else if (ctx.functions.contains(ctx.curr->text)) {
        return parse_function_call_stmt(ctx);
    }
    else if (anzu::is_builtin(ctx.curr->text)) {
        return parse_builtin_call_stmt(ctx);
    }
    else {
        parser_error(ctx, "unknown statement '{}'", ctx.curr->text);
    }
}

}

auto parse(const std::vector<anzu::token>& tokens) -> node_stmt_ptr
{
    auto ctx = anzu::parser_context{
        .curr = tokens.begin(), .end = tokens.end()
    };

    auto root = std::make_unique<anzu::node_stmt>();
    auto& seq = root->emplace<anzu::node_sequence_stmt>();
    while (ctx.curr != ctx.end) {
        seq.sequence.push_back(parse_statement(ctx));
    }
    return root;
}

}