#include "parser.hpp"

#include <optional>

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

auto handle_list_literal(parser_context& ctx) -> anzu::object;

auto try_parse_literal(parser_context& ctx) -> std::optional<anzu::object>
{
    if (ctx.curr->type == token_type::number) {
        return { anzu::to_int((ctx.curr++)->text) };
    }
    else if (ctx.curr->type == token_type::string) {
        return { (ctx.curr++)->text };
    }
     else if (consume_maybe(ctx.curr, TRUE_LIT)) {
        return { true };
    }
    else if (consume_maybe(ctx.curr, FALSE_LIT)) {
        return { false };
    }
    else if (consume_maybe(ctx.curr, NULL_LIT)) {
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
auto parse_function_call(parser_context& ctx) -> node_ptr;
auto parse_builtin_call(parser_context& ctx) -> node_ptr;

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
    else if (ctx.functions.contains(ctx.curr->text)) {
        return parse_function_call(ctx);
    }
    else if (anzu::is_builtin(ctx.curr->text)) {
        return parse_builtin_call(ctx);
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

auto parse_assign_expression(parser_context& ctx) -> node_ptr
{
    if (ctx.curr->type != token_type::name) {
        anzu::print("syntax error: cannot assign to '{}'\n", ctx.curr->text);
        std::exit(1);
    }
    auto name = (ctx.curr++)->text;
    consume_only(ctx.curr, "=");

    auto assign = std::make_unique<anzu::node_assignment>();
    assign->name = name;
    assign->expr = parse_expression(ctx);
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
    stmt->condition = parse_expression(ctx);
    consume_only(ctx.curr, "do");
    stmt->body = parse_statement_list(ctx);
    consume_only(ctx.curr, "end");
    return stmt;
}

auto parse_if_body(parser_context& ctx) -> node_ptr
{
    auto stmt = std::make_unique<node_if_statement>();
    stmt->condition = parse_expression(ctx);
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

auto parse_function(parser_context& ctx) -> node_ptr
{
    auto ret = std::make_unique<node_function_def>();
    if (ctx.curr->type != token_type::name) {
        anzu::print("syntax error: expected function name\n");
        std::exit(1);
    }
    ret->name = (ctx.curr++)->text;
    consume_only(ctx.curr, "(");
    bool expect_comma = false;
    while (ctx.curr != ctx.end && ctx.curr->text != ")") {
        if (expect_comma) {
            if (ctx.curr->text != ",") { // skip commas, enforce later
                anzu::print("syntax error: expected comma in function name list\n");
                std::exit(1);
            }
            ++ctx.curr;
        }
        else {
            if (ctx.curr->type == token_type::name) {
                ret->arg_names.push_back(ctx.curr->text);
                ++ctx.curr;          
            }
            else {
                anzu::print("syntax error: failed to parse function signature\n");
                std::exit(1);
            }
        }
        expect_comma = !expect_comma;
    }
    ctx.functions[ret->name] = { .argc=std::ssize(ret->arg_names) };
    consume_only(ctx.curr, ")");
    consume_only(ctx.curr, "do");
    ret->body = parse_statement_list(ctx);
    consume_only(ctx.curr, "end");
    return ret;
}

auto parse_return(parser_context& ctx) -> node_ptr
{
    static const auto sentinel = std::unordered_set<std::string_view>{"end", "elif", "do", "else"};

    auto ret_node = std::make_unique<anzu::node_return>();
    if (!sentinel.contains(ctx.curr->text)) {
        ret_node->return_value = parse_expression(ctx);
    } else {
        ret_node->return_value = std::make_unique<anzu::node_literal>(anzu::null_object());
    }
    return ret_node;
}

auto parse_builtin_call(parser_context& ctx) -> node_ptr
{
    auto node = std::make_unique<anzu::node_builtin_call>();
    node->function_name = (ctx.curr++)->text;

    consume_only(ctx.curr, "(");
    bool expect_comma = false;
    while (ctx.curr != ctx.end && ctx.curr->text != ")") {
        if (expect_comma) {
            if (ctx.curr->text != ",") { // skip commas, enforce later
                anzu::print("syntax error: expected comma in function call arg list\n");
                std::exit(1);
            }
            ++ctx.curr;
        }
        else {
            node->args.push_back(parse_expression(ctx));
        }
        expect_comma = !expect_comma;
    }
    consume_only(ctx.curr, ")");

    const auto argc = anzu::fetch_builtin_argc(node->function_name);
    if (argc != std::ssize(node->args)) {
        anzu::print(
            "error: function '{}' expected {} args, got {}\n",
            node->function_name, argc, std::ssize(node->args)
        );
        std::exit(1);
    }

    return node;
}

auto parse_builtin_call_stmt(parser_context& ctx) -> node_ptr
{
    auto node = std::make_unique<anzu::node_builtin_call_statement>();
    node->function_name = (ctx.curr++)->text;

    consume_only(ctx.curr, "(");
    bool expect_comma = false;
    while (ctx.curr != ctx.end && ctx.curr->text != ")") {
        if (expect_comma) {
            if (ctx.curr->text != ",") { // skip commas, enforce later
                anzu::print("syntax error: expected comma in function call arg list\n");
                std::exit(1);
            }
            ++ctx.curr;
        }
        else {
            node->args.push_back(parse_expression(ctx));
        }
        expect_comma = !expect_comma;
    }
    consume_only(ctx.curr, ")");

    const auto argc = anzu::fetch_builtin_argc(node->function_name);
    if (argc != std::ssize(node->args)) {
        anzu::print(
            "error: function '{}' expected {} args, got {}\n",
            node->function_name, argc, std::ssize(node->args)
        );
        std::exit(1);
    }

    return node;
}

auto parse_function_call(parser_context& ctx) -> node_ptr
{
    auto node = std::make_unique<anzu::node_function_call_expression>();
    node->function_name = (ctx.curr++)->text;

    consume_only(ctx.curr, "(");
    bool expect_comma = false;
    while (ctx.curr != ctx.end && ctx.curr->text != ")") {
        if (expect_comma) {
            if (ctx.curr->text != ",") { // skip commas, enforce later
                anzu::print("syntax error: expected comma in function call arg list\n");
                std::exit(1);
            }
            ++ctx.curr;
        }
        else {
            node->args.push_back(parse_expression(ctx));
        }
        expect_comma = !expect_comma;
    }
    consume_only(ctx.curr, ")");

    const auto argc = ctx.functions.at(node->function_name).argc;
    if (argc != std::ssize(node->args)) {
        anzu::print(
            "error: function '{}' expected {} args, got {}\n",
            node->function_name, argc, std::ssize(node->args)
        );
        std::exit(1);
    }

    return node;
}

auto parse_function_call_stmt(parser_context& ctx) -> node_ptr
{
    auto node = std::make_unique<anzu::node_function_call_statement>();
    node->function_name = (ctx.curr++)->text;

    consume_only(ctx.curr, "(");
    bool expect_comma = false;
    while (ctx.curr != ctx.end && ctx.curr->text != ")") {
        if (expect_comma) {
            if (ctx.curr->text != ",") { // skip commas, enforce later
                anzu::print("syntax error: expected comma in function call arg list\n");
                std::exit(1);
            }
            ++ctx.curr;
        }
        else {
            node->args.push_back(parse_expression(ctx));
        }
        expect_comma = !expect_comma;
    }
    consume_only(ctx.curr, ")");

    const auto argc = ctx.functions.at(node->function_name).argc;
    if (argc != std::ssize(node->args)) {
        anzu::print(
            "error: function '{}' expected {} args, got {}\n",
            node->function_name, argc, std::ssize(node->args)
        );
        std::exit(1);
    }

    return node;
}

auto parse_statement(parser_context& ctx) -> node_ptr
{
    if (consume_maybe(ctx.curr, "function")) {
        return parse_function(ctx);
    }
    else if (consume_maybe(ctx.curr, "return")) {
        return parse_return(ctx);
    }
    else if (consume_maybe(ctx.curr, "while")) {
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
        return parse_assign_expression(ctx);
    }
    else if (ctx.functions.contains(ctx.curr->text)) {
        return parse_function_call_stmt(ctx);
    }
    else if (anzu::is_builtin(ctx.curr->text)) {
        return parse_builtin_call_stmt(ctx);
    }
    else {
        auto expression = parse_expression(ctx);
        anzu::print("error: unused statement\n");
        expression->print();
        std::exit(1);
    }
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

}