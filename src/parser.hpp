#pragma once
#include "lexer.hpp"
#include "ast.hpp"

#include <vector>
#include <memory>
#include <unordered_map>
#include <string>

namespace anzu {

constexpr auto IF          = std::string_view{"if"};
constexpr auto ELIF        = std::string_view{"elif"};
constexpr auto ELSE        = std::string_view{"else"};
constexpr auto WHILE       = std::string_view{"while"};
constexpr auto BREAK       = std::string_view{"break"};
constexpr auto CONTINUE    = std::string_view{"continue"};
constexpr auto DO          = std::string_view{"do"};
constexpr auto END         = std::string_view{"end"};
constexpr auto TRUE_LIT    = std::string_view{"true"};
constexpr auto FALSE_LIT   = std::string_view{"false"};
constexpr auto NULL_LIT    = std::string_view{"null"};
constexpr auto FOR         = std::string_view{"for"};
constexpr auto IN          = std::string_view{"in"};

constexpr auto ADD         = std::string_view{"+"};
constexpr auto SUB         = std::string_view{"-"};
constexpr auto MUL         = std::string_view{"*"};
constexpr auto DIV         = std::string_view{"/"};
constexpr auto MOD         = std::string_view{"%"};

constexpr auto EQ          = std::string_view{"=="};
constexpr auto NE          = std::string_view{"!="};
constexpr auto LT          = std::string_view{"<"};
constexpr auto LE          = std::string_view{"<="};
constexpr auto GT          = std::string_view{">"};
constexpr auto GE          = std::string_view{">="};
constexpr auto OR          = std::string_view{"||"};
constexpr auto AND         = std::string_view{"&&"};

constexpr auto ASSIGN      = std::string_view{"="};

using token_iterator = std::vector<anzu::token>::const_iterator;

// Context used while constructing an AST. Has non-owning pointers into the
// tokens as well as keeping track of function names.
struct parser_context
{
    struct function_info
    {
        std::int64_t argc;
    };

    token_iterator       curr;
    const token_iterator end;

    std::unordered_map<std::string, function_info> functions;
};

auto parse(const std::vector<anzu::token>& tokens) -> node_stmt_ptr;

}