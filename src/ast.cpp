#include "ast.hpp"
#include "lexer.hpp"
#include "object.hpp"
#include "print.hpp"

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

void node_op::evaluate(compiler_context& ctx)
{
    ctx.program.push_back(op);
}

void node_op::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}Op: {}\n", spaces, op);
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

void node_function_definition::evaluate(compiler_context& ctx)
{
    const auto function_pos = std::ssize(ctx.program);
    ctx.program.push_back(anzu::op_function{ .name=name });
    ctx.functions[name] = {
        .argc=argc,
        .retc=retc,
        .ptr=function_pos
    };
    
    body->evaluate(ctx);

    const auto function_end_pos = std::ssize(ctx.program);
    ctx.program.push_back(anzu::op_function_end{ .retc=retc });

    ctx.program[function_pos].as<anzu::op_function>().jump = function_end_pos + 1;

    // Set retc on all return statements
    for (std::intptr_t idx = function_pos + 1; idx != function_end_pos; ++idx) {
        if (auto op = ctx.program[idx].get_if<anzu::op_return>()) {
            op->retc = retc;
        }
    }
}

void node_function_definition::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}FunctionDef:\n", spaces);
    anzu::print("{}- Argc: {}\n", spaces, argc);
    anzu::print("{}- Retc: {}\n", spaces, retc);
    anzu::print("{}- Body:\n", spaces);
    body->print(indent + 1);
}

void node_function_call::evaluate(compiler_context& ctx)
{
    auto function = ctx.functions.at(name);
    ctx.program.push_back(anzu::op_function_call{
        .name=name,
        .argc=function.argc,
        .jump=function.ptr + 1
    });
}

void node_function_call::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}FunctionCall: {}\n", spaces, name);
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

void node_return::evaluate(compiler_context& ctx)
{
    ctx.program.emplace_back(anzu::op_return{});
}

void node_return::print(int indent)
{
    const auto spaces = std::string(4 * indent, ' ');
    anzu::print("{}Return\n", spaces);
}

namespace {

// A temporary function during migration between the old and new parsers. This will
// get smaller and smaller as we move these operations to be stored in proper tree
// nodes.
auto parse_op(parser_context& ctx) -> anzu::op
{
    const auto& token = ctx.curr->text;
    ++ctx.curr;
    if (token == STORE)   return op_store{ .name=(ctx.curr++)->text };
    if (token == POP)     return op_pop{};
    if (token == DUP)     return op_dup{};
    if (token == SWAP)    return op_swap{};
    if (token == ROT)     return op_rot{};
    if (token == OVER)    return op_over{};
    if (token == ADD)     return op_add{};
    if (token == SUB)     return op_sub{};
    if (token == MUL)     return op_mul{};
    if (token == DIV)     return op_div{};
    if (token == MOD)     return op_mod{};
    if (token == EQ)      return op_eq{};
    if (token == NE)      return op_ne{};
    if (token == LT)      return op_lt{};
    if (token == LE)      return op_le{};
    if (token == GT)      return op_gt{};
    if (token == GE)      return op_ge{};
    if (token == OR)      return op_or{};
    if (token == AND)     return op_and{};
    if (token == INPUT)   return op_input{};
    if (token == DUMP)    return op_dump{};
    if (token == TO_INT)  return op_to_int{};
    if (token == TO_BOOL) return op_to_bool{};
    if (token == TO_STR)  return op_to_str{};
    return op_push_var{.name=token};
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

auto parse_function_body(parser_context& ctx) -> node_ptr
{
    auto stmt = std::make_unique<anzu::node_function_definition>();
    stmt->name = (ctx.curr++)->text;

    bool success = false;
    std::tie(std::ignore, success) = ctx.function_names.insert(stmt->name);
    if (!success) {
        anzu::print("error: multiple defintions for function '{}'\n", stmt->name);
        std::exit(1);
    }

    stmt->argc = anzu::to_int((ctx.curr++)->text);
    stmt->retc = anzu::to_int((ctx.curr++)->text);
    stmt->body = parse_statement_list(ctx);
    consume_only(ctx.curr, "end");
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

auto handle_list_literal(parser_context& ctx) -> anzu::object_list
{
    auto list = std::make_shared<std::vector<anzu::object>>();

    consume_only(ctx.curr, "[");
    while (ctx.curr != ctx.end && ctx.curr->text != "]") {
        if (ctx.curr->text == "[") { // Nested list literal
            list->push_back(handle_list_literal(ctx));
        }
        else if (ctx.curr->type == token_type::string) {
            list->push_back(ctx.curr->text);
        }
        else if (ctx.curr->type == token_type::number) {
            list->push_back(anzu::to_int(ctx.curr->text));
        }
        else if (ctx.curr->text == "true") {
            list->push_back(true);
        }
        else if (ctx.curr->text == "false") {
            list->push_back(false);
        }
        else if (ctx.curr->text == ",") {
            // Pass, delimiters currently optional, will enforce after this workes
        }
        else {
            anzu::print("could not recognise token while parsing list literal: {}\n", ctx.curr->text);
            std::exit(1);
        }
        ++ctx.curr;
    }
    if (ctx.curr == ctx.end) {
        anzu::print("end of file reached while parsing string literal\n");
        std::exit(1);
    }
    consume_only(ctx.curr, "]");

    return list;
}

auto parse_statement(parser_context& ctx) -> node_ptr
{
    if (consume_maybe(ctx.curr, "function")) {
        return parse_function_body(ctx);
    }
    else if (consume_maybe(ctx.curr, "while")) {
        return parse_while_body(ctx);
    }
    else if (consume_maybe(ctx.curr, "if")) {
        return parse_if_body(ctx);
    }
    else if (consume_maybe(ctx.curr, "return")) {
        return std::make_unique<anzu::node_return>(); 
    }
    else if (consume_maybe(ctx.curr, "break")) {
        return std::make_unique<anzu::node_break>(); 
    }
    else if (consume_maybe(ctx.curr, "continue")) {
        return std::make_unique<anzu::node_continue>(); 
    }

    else if (ctx.curr->type == token_type::number) {
        return std::make_unique<anzu::node_literal>(anzu::to_int((ctx.curr++)->text));
    }
    else if (ctx.curr->type == token_type::string) {
        return std::make_unique<anzu::node_literal>((ctx.curr++)->text);
    }
    else if (ctx.curr->text == "[") {
        return std::make_unique<anzu::node_literal>(handle_list_literal(ctx));
    }
    
    else if (ctx.function_names.contains(ctx.curr->text)) {
        return std::make_unique<anzu::node_function_call>((ctx.curr++)->text);
    }
    else if (anzu::is_builtin(ctx.curr->text)) {
        return std::make_unique<anzu::node_builtin_call>((ctx.curr++)->text);
    }
    else if (ctx.curr != ctx.end) {
        return std::make_unique<anzu::node_op>(parse_op(ctx));
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