#include "parser.hpp"

#include <optional>

namespace anzu {

namespace {

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };

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

auto parse_expression(parser_context& ctx) -> node_expr_ptr;
auto parse_function_call_expr(parser_context& ctx) -> node_expr_ptr;
auto parse_builtin_call_expr(parser_context& ctx) -> node_expr_ptr;

auto parse_single_factor(parser_context& ctx) -> node_expr_ptr
{
    if (consume_maybe(ctx.curr, "(")) {
        auto expr = parse_expression(ctx);
        consume_only(ctx.curr, ")");
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
        anzu::print("syntax error: '{}' is not a name, cannot be a factor\n", ctx.curr->text);
        std::exit(1);
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
        anzu::print("syntax error: cannot assign to '{}'\n", ctx.curr->text);
        std::exit(1);
    }
    auto name = (ctx.curr++)->text;
    consume_only(ctx.curr, "=");

    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_assignment_stmt>();
    stmt.name = name;
    stmt.expr = parse_expression(ctx);
    return node;
}

auto parse_statement(parser_context& ctx) -> node_stmt_ptr;

// statement_list:
//     | statement
//     | statement statement_list
auto parse_statement_list(parser_context& ctx) -> node_stmt_ptr
{
    static const auto sentinel = std::unordered_set<std::string_view>{"end", "elif", "do", "else"};

    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_sequence_stmt>();
    while (ctx.curr != ctx.end && !sentinel.contains(ctx.curr->text)) {
        stmt.sequence.push_back(parse_statement(ctx));
    }
    return node;
}

auto parse_while_body(parser_context& ctx) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_while_stmt>();
    stmt.condition = parse_expression(ctx);
    consume_only(ctx.curr, "do");
    stmt.body = parse_statement_list(ctx);
    consume_only(ctx.curr, "end");
    return node;
}

auto parse_if_body(parser_context& ctx) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_if_stmt>();
    stmt.condition = parse_expression(ctx);
    consume_only(ctx.curr, "do");
    stmt.body = parse_statement_list(ctx);

    if (consume_maybe(ctx.curr, "elif")) {
        stmt.else_body = parse_if_body(ctx);
    }
    else if (consume_maybe(ctx.curr, "else")) {
        stmt.else_body = parse_statement_list(ctx);
        consume_only(ctx.curr, "end");
    }
    else {
        consume_only(ctx.curr, "end");
    }
    
    return node;
}

auto parse_function_def(parser_context& ctx) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_function_def_stmt>();
    if (ctx.curr->type != token_type::name) {
        anzu::print("syntax error: expected function name\n");
        std::exit(1);
    }
    stmt.name = (ctx.curr++)->text;
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
                stmt.arg_names.push_back(ctx.curr->text);
                ++ctx.curr;          
            }
            else {
                anzu::print("syntax error: failed to parse function signature\n");
                std::exit(1);
            }
        }
        expect_comma = !expect_comma;
    }
    ctx.functions[stmt.name] = { .argc=std::ssize(stmt.arg_names) };
    consume_only(ctx.curr, ")");
    consume_only(ctx.curr, "do");
    stmt.body = parse_statement_list(ctx);
    consume_only(ctx.curr, "end");
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
            expr.args.push_back(parse_expression(ctx));
        }
        expect_comma = !expect_comma;
    }
    consume_only(ctx.curr, ")");

    const auto argc = anzu::fetch_builtin_argc(expr.function_name);
    if (argc != std::ssize(expr.args)) {
        anzu::print(
            "error: function '{}' expected {} args, got {}\n",
            expr.function_name, argc, std::ssize(expr.args)
        );
        std::exit(1);
    }

    return node;
}

auto parse_builtin_call_stmt(parser_context& ctx) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_builtin_call_stmt>();
    stmt.function_name = (ctx.curr++)->text;

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
            stmt.args.push_back(parse_expression(ctx));
        }
        expect_comma = !expect_comma;
    }
    consume_only(ctx.curr, ")");

    const auto argc = anzu::fetch_builtin_argc(stmt.function_name);
    if (argc != std::ssize(stmt.args)) {
        anzu::print(
            "error: function '{}' expected {} args, got {}\n",
            stmt.function_name, argc, std::ssize(stmt.args)
        );
        std::exit(1);
    }

    return node;
}

auto parse_function_call_expr(parser_context& ctx) -> node_expr_ptr
{
    auto node = std::make_unique<anzu::node_expr>();
    auto& expr = node->emplace<anzu::node_function_call_expr>();
    expr.function_name = (ctx.curr++)->text;

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
            expr.args.push_back(parse_expression(ctx));
        }
        expect_comma = !expect_comma;
    }
    consume_only(ctx.curr, ")");

    const auto argc = ctx.functions.at(expr.function_name).argc;
    if (argc != std::ssize(expr.args)) {
        anzu::print(
            "error: function '{}' expected {} args, got {}\n",
            expr.function_name, argc, std::ssize(expr.args)
        );
        std::exit(1);
    }

    return node;
}

auto parse_function_call_stmt(parser_context& ctx) -> node_stmt_ptr
{
    auto node = std::make_unique<anzu::node_stmt>();
    auto& stmt = node->emplace<anzu::node_function_call_stmt>();
    stmt.function_name = (ctx.curr++)->text;

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
            stmt.args.push_back(parse_expression(ctx));
        }
        expect_comma = !expect_comma;
    }
    consume_only(ctx.curr, ")");

    const auto argc = ctx.functions.at(stmt.function_name).argc;
    if (argc != std::ssize(stmt.args)) {
        anzu::print(
            "error: function '{}' expected {} args, got {}\n",
            stmt.function_name, argc, std::ssize(stmt.args)
        );
        std::exit(1);
    }

    return node;
}

auto parse_statement(parser_context& ctx) -> node_stmt_ptr
{
    if (consume_maybe(ctx.curr, "function")) {
        return parse_function_def(ctx);
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
        auto node = std::make_unique<anzu::node_stmt>();
        node->emplace<anzu::node_break_stmt>();
        return node;
    }
    else if (consume_maybe(ctx.curr, "continue")) {
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
        auto expression = parse_expression(ctx);
        anzu::print("error: unused statement\n");
        //expression->print();
        std::exit(1);
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

auto print_node(const anzu::node_expr& root, int indent) -> void
{
    const auto spaces = std::string(4 * indent, ' ');
    std::visit(overloaded {
        [=](const node_literal_expr& node) {
            anzu::print("{}Literal: {}\n", spaces, node.value.to_repr());
        },
        [=](const node_variable_expr& node) {
            anzu::print("{}Variable: {}\n", spaces, node.name);
        },
        [=](const node_bin_op_expr& node) {
            anzu::print("{}BinOp\n", spaces);
            anzu::print("{}- Op: {}\n", spaces, node.op);
            anzu::print("{}- Lhs:\n", spaces);
            if (!node.lhs) {
                anzu::print("bin op has no lhs\n");
                std::exit(1);
            }
            print_node(*node.lhs, indent + 1);
            anzu::print("{}- Rhs:\n", spaces);
            if (!node.rhs) {
                anzu::print("bin op has no rhs\n");
                std::exit(1);
            }
            print_node(*node.rhs, indent + 1);
        },
        [=](const node_function_call_expr& node) {
            anzu::print("{}FunctionCall (Expr): {}\n", spaces, node.function_name);
            anzu::print("{}- Args:\n", spaces);
            for (const auto& arg : node.args) {
                print_node(*arg, indent + 1);
            }
        },
        [=](const node_builtin_call_expr& node) {
            anzu::print("{}BuiltinCall (Expr): {}\n", spaces, node.function_name);
            anzu::print("{}- Args:\n", spaces);
            for (const auto& arg : node.args) {
                print_node(*arg, indent + 1);
            }
        }
    }, root);
}

auto print_node(const anzu::node_stmt& root, int indent) -> void
{
    const auto spaces = std::string(4 * indent, ' ');
    std::visit(overloaded {
        [=](const node_sequence_stmt& node) {
            anzu::print("{}Sequence:\n", spaces);
            for (const auto& seq_node : node.sequence) {
                print_node(*seq_node, indent + 1);
            }
        },
        [=](const node_while_stmt& node) {
            anzu::print("{}While:\n", spaces);
            anzu::print("{}- Condition:\n", spaces);
            print_node(*node.condition, indent + 1);
            anzu::print("{}- Body:\n", spaces);
            print_node(*node.body, indent + 1);
        },
        [=](const node_if_stmt& node) {
            anzu::print("{}If:\n", spaces);
            anzu::print("{}- Condition:\n", spaces);
            print_node(*node.condition, indent + 1);
            anzu::print("{}- Body:\n", spaces);
            print_node(*node.body, indent + 1);
            if (node.else_body) {
                anzu::print("{}- Else:\n", spaces);
                print_node(*node.else_body, indent + 1);
            }
        },
        [=](const node_break_stmt& node) {
            anzu::print("{}Break\n", spaces);
        },
        [=](const node_continue_stmt& node) {
            anzu::print("{}Continue\n", spaces);
        },
        [=](const node_assignment_stmt& node) {
            anzu::print("{}AssignExpr\n", spaces);
            anzu::print("{}- Name: {}\n", spaces, node.name);
            anzu::print("{}- Value:\n", spaces);
            print_node(*node.expr, indent + 1);
        },
        [=](const node_function_def_stmt& node) {
            anzu::print("{}Function: {}", spaces, node.name);
            for (const auto& arg : node.arg_names) {
                anzu::print(" {}", arg);
            }
            anzu::print("\n");
            print_node(*node.body, indent + 1);
        },
        [=](const node_function_call_stmt& node) {
            anzu::print("{}FunctionCall (Stmt): {}\n", spaces, node.function_name);
            anzu::print("{}- Args:\n", spaces);
            for (const auto& arg : node.args) {
                print_node(*arg, indent + 1);
            }
        },
        [=](const node_builtin_call_stmt& node) {
            anzu::print("{}BuiltinCall (Stmt): {}\n", spaces, node.function_name);
            anzu::print("{}- Args:\n", spaces);
            for (const auto& arg : node.args) {
                print_node(*arg, indent + 1);
            }
        },
        [=](const node_return_stmt& node) {
            anzu::print("{}Return:\n", spaces);
            print_node(*node.return_value, indent + 1);
        },
        [=](const node_expr& node) {
            print_node(node, indent + 1);
        }
    }, root);
}

}