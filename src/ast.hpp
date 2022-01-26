#pragma once
#include "op_codes.hpp"
#include "object.hpp"
#include "lexer.hpp"

#include <memory>
#include <vector>

namespace anzu {

struct parser_context;
struct compiler_context;

// Stack Manipulation

constexpr auto POP         = std::string_view{"pop"};
constexpr auto DUP         = std::string_view{"dup"};
constexpr auto SWAP        = std::string_view{"swap"};
constexpr auto ROT         = std::string_view{"rot"};
constexpr auto OVER        = std::string_view{"over"};

// Store Manipulation

constexpr auto STORE       = std::string_view{"->"};

// Control Flow / Functions

constexpr auto IF          = std::string_view{"if"};
constexpr auto ELIF        = std::string_view{"elif"};
constexpr auto ELSE        = std::string_view{"else"};

constexpr auto WHILE       = std::string_view{"while"};
constexpr auto BREAK       = std::string_view{"break"};
constexpr auto CONTINUE    = std::string_view{"continue"};

constexpr auto FUNCTION    = std::string_view{"function"};
constexpr auto RETURN      = std::string_view{"return"};

constexpr auto DO          = std::string_view{"do"};
constexpr auto END         = std::string_view{"end"};

// Numerical Operators

constexpr auto ADD         = std::string_view{"+"};
constexpr auto SUB         = std::string_view{"-"};
constexpr auto MUL         = std::string_view{"*"};
constexpr auto DIV         = std::string_view{"/"};
constexpr auto MOD         = std::string_view{"%"};

// Logical Operators

constexpr auto EQ          = std::string_view{"=="};
constexpr auto NE          = std::string_view{"!="};
constexpr auto LT          = std::string_view{"<"};
constexpr auto LE          = std::string_view{"<="};
constexpr auto GT          = std::string_view{">"};
constexpr auto GE          = std::string_view{">="};
constexpr auto OR          = std::string_view{"||"};
constexpr auto AND         = std::string_view{"&&"};

// IO

constexpr auto INPUT       = std::string_view{"input"};
constexpr auto DUMP        = std::string_view{"."};

// Literals

constexpr auto TRUE_LIT    = std::string_view{"true"};
constexpr auto FALSE_LIT   = std::string_view{"false"};

// Casts

constexpr auto TO_INT      = std::string_view{"(int)"};
constexpr auto TO_BOOL     = std::string_view{"(bool)"};
constexpr auto TO_STR      = std::string_view{"(str)"};

// I normally avoid inheritance trees, however dealing with variants here was a bit
// cumbersome and required wrapping the variant in a struct to allow recusrion (due
// to the variant needing to be defined after the nodes that contained them). Given
// that this will be a shall tree of types (depth of 1) I feel it's fine to use here.
struct node
{
    virtual ~node() {}
    virtual void evaluate(compiler_context& ctx) = 0;
    virtual void print(int indent = 0) = 0;
};

// This is just a temporary node for storing bytecode, with the goal
// of retiring the old parser. This will eventually get used less and less as more code
// gets translated to proper nodes.
struct node_op : public node
{
    anzu::op op;

    node_op(const anzu::op& new_op) : op(new_op) {}
    void evaluate(compiler_context& ctx) override;
    void print(int indent = 0) override;
};

struct node_sequence : public node
{
    std::vector<std::unique_ptr<node>> sequence;

    void evaluate(compiler_context& ctx) override;
    void print(int indent = 0) override;
};

struct node_while_statement : public node
{
    std::unique_ptr<node> condition;
    std::unique_ptr<node> body;

    void evaluate(compiler_context& ctx) override;
    void print(int indent = 0) override;
};

struct node_if_statement : public node
{
    std::unique_ptr<node> condition;
    std::unique_ptr<node> body;
    std::unique_ptr<node> else_body;

    void evaluate(compiler_context& ctx) override;
    void print(int indent = 0) override;
};

struct node_function_definition : public node
{
    std::string name;
    int argc;
    int retc;
    std::unique_ptr<node> body;

    void evaluate(compiler_context& ctx) override;
    void print(int indent = 0) override;
};

struct node_function_call : public node
{
    std::string name;

    node_function_call(const std::string& n) : name(n) {}
    void evaluate(compiler_context& ctx) override;
    void print(int indent = 0) override;
};

struct node_builtin_call : public node
{
    std::string name;

    node_builtin_call(const std::string& n) : name(n) {}
    void evaluate(compiler_context& ctx) override;
    void print(int indent = 0) override;
};

struct node_break : public node
{
    void evaluate(compiler_context& ctx) override;
    void print(int indent = 0) override;
};

struct node_continue : public node
{
    void evaluate(compiler_context& ctx) override;
    void print(int indent = 0) override;
};

struct node_return : public node
{
    void evaluate(compiler_context& ctx) override;
    void print(int indent = 0) override;
};

struct node_literal : public node
{
    anzu::object value;

    node_literal(const anzu::object& v) : value(v) {}
    void evaluate(compiler_context& ctx) override;
    void print(int indent = 0) override;
};

struct node_variable : public node
{
    std::string name;
    node_variable(const std::string& n) : name(n) {}
    void evaluate(compiler_context& ctx) override;
    void print(int indent = 0) override;
};

struct node_bin_op : public node
{
    std::string op; // TODO: make into enum
    std::unique_ptr<node> lhs;
    std::unique_ptr<node> rhs;

    void evaluate(compiler_context& ctx) override;
    void print(int indent = 0) override;
};

using token_iterator = std::vector<anzu::token>::const_iterator;
using node_ptr       = std::unique_ptr<anzu::node>;

// Context used while constructing an AST. Has non-owning pointers into the tokens as well
// as keeping track of function names.
struct parser_context
{
    token_iterator       curr;
    const token_iterator end;

    std::unordered_set<std::string> function_names;
};

// Struct used to store information while compiling an AST. Contains the output program
// as well as information such as function definitions.
struct compiler_context
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

auto parse(const std::vector<anzu::token>& tokens) -> std::unique_ptr<anzu::node>;
auto compile(const std::unique_ptr<anzu::node>& root) -> std::vector<anzu::op>;

}