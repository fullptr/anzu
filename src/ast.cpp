#include "ast.hpp"
#include "lexer.hpp"
#include "object.hpp"
#include "print.hpp"

#include <optional>
#include <tuple>

namespace anzu {
namespace {

auto consume_maybe(token_iterator& it, std::string_view tok) -> bool
{
    if (it->text == tok) {
        ++it; // skip end
        return true;
    }
    return false;
}

auto consume_only(token_iterator& it, std::string_view tok) -> void
{
    if (!consume_maybe(it, tok)) {
        anzu::print("parse error: expected '{}', got '{}'\n", tok, it->text);
        std::exit(1);
    }
}

}

void node_sequence::evaluate(compiler_context& ctx)
{
    for (const auto& node : sequence) {
        node->evaluate(ctx);
    }
}

void node_sequence::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}Sequence:\n", spaces);
    for (const auto& node : sequence) {
        node->print(indent + 1);
    }
}

void node_while_statement::evaluate(compiler_context& ctx)
{
    const auto while_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_while{});

    condition->evaluate(ctx);
    
    const auto do_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_do{});

    body->evaluate(ctx);

    const auto end_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_while_end{ .jump=while_pos }); // Jump back to start

    // Setup the control flow jumps
    ctx.program[do_pos].as<anzu::op_do>().jump = end_pos + 1; // Jump past the end if false
    for (std::intptr_t idx = do_pos + 1; idx != end_pos; ++idx) {
        // Only set break and continue jumps if they are currently not set, they may be set it
        // there are nested while loops
        if (auto op = ctx.program[idx].get_if<anzu::op_break>(); op && op->jump == -1) {
            op->jump = end_pos + 1;
        }
        else if (auto op = ctx.program[idx].get_if<anzu::op_continue>(); op && op->jump == -1) {
            op->jump = while_pos;
        }
    }
}

void node_while_statement::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}While:\n", spaces);
    anzu::print("{}- Condition:\n", spaces);
    condition->print(indent + 1);
    anzu::print("{}- Body:\n", spaces);
    body->print(indent + 1);
}

void node_if_statement::evaluate(compiler_context& ctx)
{
    const auto if_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_if{});

    condition->evaluate(ctx);
    
    const auto do_pos = std::ssize(ctx.program);
    ctx.program.emplace_back(anzu::op_do{});
    body->evaluate(ctx);

    auto else_pos = std::intptr_t{-1};
    if (else_body) {
        else_pos = std::ssize(ctx.program);
        ctx.program.emplace_back(anzu::op_else{});
        else_body->evaluate(ctx);
    }

    ctx.program.emplace_back(anzu::op_if_end{});
    if (else_pos == -1) {
        ctx.program[do_pos].as<anzu::op_do>().jump = std::ssize(ctx.program); // Jump past the end if false
    } else {
        ctx.program[do_pos].as<anzu::op_do>().jump = else_pos + 1; // Jump into the else block if false
        ctx.program[else_pos].as<anzu::op_else>().jump = std::ssize(ctx.program); // Jump past the end if false
    }
}

void node_if_statement::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}If:\n", spaces);
    anzu::print("{}- Condition:\n", spaces);
    condition->print(indent + 1);
    anzu::print("{}- Body:\n", spaces);
    body->print(indent + 1);
    if (else_body) {
        anzu::print("{}- Else:\n", spaces);
        else_body->print(indent + 1);
    }
}

void node_builtin_call::evaluate(compiler_context& ctx)
{
    ctx.program.emplace_back(anzu::op_builtin_function_call{
        .name=name,
        .func=anzu::fetch_builtin(name)
    });
}

void node_builtin_call::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}BuiltinCall: {}\n", spaces, name);
}

void node_literal::evaluate(compiler_context& ctx)
{
    ctx.program.emplace_back(anzu::op_push_const{ .value=value });
}

void node_literal::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}Literal: {}\n", spaces, value.to_repr());
}

void node_variable::evaluate(compiler_context& ctx)
{
    ctx.program.emplace_back(anzu::op_push_var{ .name=name });
}

void node_variable::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}Variable: {}\n", spaces, name);
}

void node_break::evaluate(compiler_context& ctx)
{
    ctx.program.emplace_back(anzu::op_break{});
}

void node_break::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}Break\n", spaces);
}

void node_continue::evaluate(compiler_context& ctx)
{
    ctx.program.emplace_back(anzu::op_continue{});
}

void node_continue::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}Continue\n", spaces);
}

void node_bin_op::evaluate(compiler_context& ctx)
{
    lhs->evaluate(ctx);
    rhs->evaluate(ctx);
    if      (op == "+")  { ctx.program.push_back(anzu::op_add{}); }
    else if (op == "-")  { ctx.program.push_back(anzu::op_sub{}); }
    else if (op == "*")  { ctx.program.push_back(anzu::op_mul{}); }
    else if (op == "/")  { ctx.program.push_back(anzu::op_div{}); }
    else if (op == "%")  { ctx.program.push_back(anzu::op_mod{}); }
    else if (op == "<")  { ctx.program.push_back(anzu::op_lt{}); }
    else if (op == "<=") { ctx.program.push_back(anzu::op_le{}); }
    else if (op == ">")  { ctx.program.push_back(anzu::op_gt{}); }
    else if (op == ">=") { ctx.program.push_back(anzu::op_ge{}); }
    else if (op == "==") { ctx.program.push_back(anzu::op_eq{}); }
    else if (op == "!=") { ctx.program.push_back(anzu::op_ne{}); }
    else {
        anzu::print("syntax error: unknown binary operator: '{}'\n", op);
        std::exit(1);
    }
}

void node_bin_op::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}BinOp\n", spaces);
    anzu::print("{}- Op: {}\n", spaces, op);
    anzu::print("{}- Lhs:\n", spaces);
    if (!lhs) {
        anzu::print("bin op has no lhs\n");
        std::exit(1);
    }
    lhs->print(indent + 1);
    anzu::print("{}- Rhs:\n", spaces);
    if (!rhs) {
        anzu::print("bin op has no rhs\n");
        std::exit(1);
    }
    rhs->print(indent + 1);
}

void node_assignment::evaluate(compiler_context& ctx)
{
    value->evaluate(ctx);
    ctx.program.emplace_back(anzu::op_store{ .name=name });
}

void node_assignment::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}Assignment\n", spaces);
    anzu::print("{}- Name: {}\n", spaces, name);
    anzu::print("{}- Value:\n", spaces);
    value->print(indent + 1);
}

namespace {

auto handle_list_literal(parser_context& ctx) -> anzu::object;

auto try_parse_literal(parser_context& ctx) -> std::optional<anzu::object>
{
    if (ctx.curr->type == token_type::number) {
        return { anzu::to_int((ctx.curr++)->text) };
    }
    else if (ctx.curr->type == token_type::string) {
        return { (ctx.curr++)->text };
    }
     else if (consume_maybe(ctx.curr, "true")) {
        return { true };
    }
    else if (consume_maybe(ctx.curr, "false")) {
        return { false };
    }
    else if (ctx.curr->text == "[") {
        return { handle_list_literal(ctx) };
    }
    return std::nullopt;
};

auto handle_list_literal(parser_context& ctx) -> anzu::object
{
    auto list = std::make_shared<std::vector<anzu::object>>();

    consume_only(ctx.curr, "[");
    bool expect_comma = false;
    while (ctx.curr != ctx.end && ctx.curr->text != "]") {
        if (expect_comma) {
            if (ctx.curr->text != ",") { // skip commas, enforce later
                anzu::print("syntax error: expected comma in list literal\n");
                std::exit(1);
            }
            ++ctx.curr;
        }
        else {
            if (auto obj = try_parse_literal(ctx); obj.has_value()) {
                list->push_back(*obj);            
            }
            else {
                anzu::print("syntax error: failed to parse literal\n");
                std::exit(1);
            }
        }
        expect_comma = !expect_comma;
    }
    if (ctx.curr == ctx.end) {
        anzu::print("syntax error: list literal never closed\n");
        std::exit(1);
    }
    consume_only(ctx.curr, "]");

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

auto parse_expression(parser_context& ctx) -> node_ptr;

auto parse_single_factor(parser_context& ctx) -> node_ptr
{
    if (consume_maybe(ctx.curr, "(")) {
        auto expr = parse_expression(ctx);
        consume_only(ctx.curr, ")");
        return expr;
    }  
    else if (auto factor = anzu::try_parse_literal(ctx); factor.has_value()) {
        return std::make_unique<anzu::node_literal>(*factor);
    }
    else if (anzu::is_builtin(ctx.curr->text)) {
        anzu::print("syntax error: '{}' is a function name, cannot be a factor\n", ctx.curr->text);
        std::exit(1);
    }
    else if (ctx.curr->type != token_type::name) {
        anzu::print("syntax error: '{}' is not a name, cannot be a factor\n", ctx.curr->text);
        std::exit(1);
    }
    else {
        return std::make_unique<anzu::node_variable>((ctx.curr++)->text);
    }
}

// Level is the precendence level, the lower the number, the tighter the factors bind.
auto parse_compound_factor(parser_context& ctx, std::int64_t level) -> node_ptr
{
    if (level == 0) {
        return parse_single_factor(ctx);
    }

    auto left = parse_compound_factor(ctx, level - 1);
    while (ctx.curr != ctx.end && bin_ops_table[level].contains(ctx.curr->text)) {
        auto op = (ctx.curr++)->text;

        auto new_left = std::make_unique<anzu::node_bin_op>();
        new_left->lhs = std::move(left);
        new_left->op = op;
        new_left->rhs = parse_compound_factor(ctx, level - 1);

        left = std::move(new_left);
    }
    return left;
}

auto parse_expression(parser_context& ctx) -> node_ptr
{
    return parse_compound_factor(ctx, std::ssize(bin_ops_table) - 1i64);
}

auto parse_assignment(parser_context& ctx) -> node_ptr
{
    if (ctx.curr->type != token_type::name) {
        anzu::print("syntax error: cannot assign to '{}'\n", ctx.curr->text);
        std::exit(1);
    }
    auto name = (ctx.curr++)->text;
    consume_only(ctx.curr, "=");

    auto assign = std::make_unique<anzu::node_assignment>();
    assign->name = name;
    assign->value = parse_expression(ctx);
    return assign;
}

auto parse_statement(parser_context& ctx) -> node_ptr;

// statement_list:
//     | statement
//     | statement statement_list
auto parse_statement_list(parser_context& ctx) -> node_ptr
{
    static const auto sentinel = std::unordered_set<std::string_view>{"end", "elif", "do", "else"};

    auto stmt = std::make_unique<node_sequence>();
    while (ctx.curr != ctx.end && !sentinel.contains(ctx.curr->text)) {
        stmt->sequence.push_back(parse_statement(ctx));
    }
    return stmt;
}

auto parse_while_body(parser_context& ctx) -> node_ptr
{
    auto stmt = std::make_unique<anzu::node_while_statement>();
    stmt->condition = parse_statement_list(ctx);
    consume_only(ctx.curr, "do");
    stmt->body = parse_statement_list(ctx);
    consume_only(ctx.curr, "end");
    return stmt;
}

auto parse_if_body(parser_context& ctx) -> node_ptr
{
    auto stmt = std::make_unique<node_if_statement>();
    stmt->condition = parse_statement_list(ctx);
    consume_only(ctx.curr, "do");
    stmt->body = parse_statement_list(ctx);

    if (consume_maybe(ctx.curr, "elif")) {
        stmt->else_body = parse_if_body(ctx);
    }
    else if (consume_maybe(ctx.curr, "else")) {
        stmt->else_body = parse_statement_list(ctx);
        consume_only(ctx.curr, "end");
    }
    else {
        consume_only(ctx.curr, "end");
    }
    
    return stmt;
}

auto parse_statement(parser_context& ctx) -> node_ptr
{
    if (consume_maybe(ctx.curr, "while")) {
        return parse_while_body(ctx);
    }
    else if (consume_maybe(ctx.curr, "if")) {
        return parse_if_body(ctx);
    }
    else if (consume_maybe(ctx.curr, "break")) {
        return std::make_unique<anzu::node_break>(); 
    }
    else if (consume_maybe(ctx.curr, "continue")) {
        return std::make_unique<anzu::node_continue>(); 
    }
    else if (auto next = std::next(ctx.curr); next != ctx.end && next->text == "=") {
        return parse_assignment(ctx);
    }
    else if (anzu::is_builtin(ctx.curr->text)) {
        return std::make_unique<anzu::node_builtin_call>((ctx.curr++)->text);
    }
    else if (ctx.curr != ctx.end) {
        return parse_expression(ctx);
    }
    return nullptr;
}

}

auto parse(const std::vector<anzu::token>& tokens) -> node_ptr
{
    auto ctx = anzu::parser_context{
        .curr = tokens.begin(), .end = tokens.end()
    };

    auto root = std::make_unique<anzu::node_sequence>();
    while (ctx.curr != ctx.end) {
        root->sequence.push_back(parse_statement(ctx));
    }
    return root;
}

auto compile(const std::unique_ptr<anzu::node>& root) -> std::vector<anzu::op>
{
    anzu::compiler_context ctx;
    root->evaluate(ctx);
    return ctx.program;
}

}