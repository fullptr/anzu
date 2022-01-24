#pragma once
#include "op_codes.hpp"
#include "object.hpp"
#include "lexer.hpp"
#include "parser.hpp"

#include <memory>
#include <vector>

namespace anzu {

// Struct used to store information while evaluating an ast. Contains the output program
// as well as information such as function definitions.
struct ast_eval_context
{
    struct function_def
    {
        int           argc;
        int           retc;
        std::intptr_t ptr; // Pointer to the position of this function in the program.
    };

    std::vector<anzu::op> program;
    std::unordered_map<std::string, function_def> functions;
};

// I normally avoid inheritance trees, however dealing with variants here was a bit
// cumbersome and required wrapping the variant in a struct to allow recusrion (due
// to the variant needing to be defined after the nodes that contained them). Given
// that this will be a shall tree of types (depth of 1) I feel it's fine to use here.
struct node
{
    virtual ~node() {}
    virtual void evaluate(ast_eval_context& ctx) = 0;
    virtual void print(int indent = 0) = 0;
};

// This is just a temporary node for storing bytecode, with the goal
// of retiring the old parser. This will eventually get used less and less as more code
// gets translated to proper nodes.
struct node_op : public node
{
    anzu::op op;

    node_op(const anzu::op& new_op) : op(new_op) {}
    void evaluate(ast_eval_context& ctx) override;
    void print(int indent = 0) override;
};

struct node_sequence : public node
{
    std::vector<std::unique_ptr<node>> sequence;

    void evaluate(ast_eval_context& ctx) override;
    void print(int indent = 0) override;
};

struct node_while_statement : public node
{
    std::unique_ptr<node> condition;
    std::unique_ptr<node> body;

    void evaluate(ast_eval_context& ctx) override;
    void print(int indent = 0) override;
};

struct node_if_statement : public node
{
    std::unique_ptr<node> condition;
    std::unique_ptr<node> body;
    std::unique_ptr<node> else_body;

    void evaluate(ast_eval_context& ctx) override;
    void print(int indent = 0) override;
};

struct node_function_definition : public node
{
    std::string name;
    int argc;
    int retc;
    std::unique_ptr<node> body;

    void evaluate(ast_eval_context& ctx) override;
    void print(int indent = 0) override;
};

struct node_function_call : public node
{
    std::string name;

    node_function_call(const std::string& n) : name(n) {}
    void evaluate(ast_eval_context& ctx) override;
    void print(int indent = 0) override;
};

struct node_builtin_call : public node
{
    std::string name;

    node_builtin_call(const std::string& n) : name(n) {}
    void evaluate(ast_eval_context& ctx) override;
    void print(int indent = 0) override;
};

struct node_break : public node
{
    void evaluate(ast_eval_context& ctx) override;
    void print(int indent = 0) override;
};

struct node_continue : public node
{
    void evaluate(ast_eval_context& ctx) override;
    void print(int indent = 0) override;
};

struct node_return : public node
{
    void evaluate(ast_eval_context& ctx) override;
    void print(int indent = 0) override;
};

struct node_literal : public node
{
    anzu::object value;

    node_literal(const anzu::object& v) : value(v) {}
    void evaluate(ast_eval_context& ctx) override;
    void print(int indent = 0) override;
};

auto build_ast(const std::vector<anzu::token>& tokens) -> std::unique_ptr<anzu::node>;

}