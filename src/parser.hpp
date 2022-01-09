#pragma once
#include "op_codes.hpp"

#include <vector>
#include <string>

namespace anzu {

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
constexpr auto OR          = std::string_view{"or"};
constexpr auto AND         = std::string_view{"and"};

// IO

constexpr auto INPUT       = std::string_view{"input"};
constexpr auto DUMP        = std::string_view{"."};

// Literals

constexpr auto TRUE_LIT    = std::string_view{"true"};
constexpr auto FALSE_LIT   = std::string_view{"false"};
constexpr auto STRING_LIT  = std::string_view{"__string"};

// Casts

constexpr auto TO_INT      = std::string_view{"(int)"};
constexpr auto TO_BOOL     = std::string_view{"(bool)"};
constexpr auto TO_STR      = std::string_view{"(str)"};

// Debug

constexpr auto PRINT_FRAME = std::string_view{"frame"};

auto parse(const std::vector<std::string>& tokens) -> std::vector<anzu::op>;

}