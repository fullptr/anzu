#pragma once
#include "op_codes.hpp"

#include <vector>
#include <string>

namespace anzu {
namespace lexer {

constexpr auto STORE       = std::string_view{"->"};
constexpr auto DUMP        = std::string_view{"."};
constexpr auto POP         = std::string_view{"pop"};
constexpr auto ADD         = std::string_view{"+"};
constexpr auto SUB         = std::string_view{"-"};
constexpr auto MUL         = std::string_view{"*"};
constexpr auto DIV         = std::string_view{"/"};
constexpr auto MOD         = std::string_view{"%"};
constexpr auto DUP         = std::string_view{"dup"};
constexpr auto PRINT_FRAME = std::string_view{"frame"};
constexpr auto DO          = std::string_view{"do"};
constexpr auto WHILE       = std::string_view{"while"};
constexpr auto IF          = std::string_view{"if"};
constexpr auto ELSE        = std::string_view{"else"};
constexpr auto END         = std::string_view{"end"};
constexpr auto EQ          = std::string_view{"=="};
constexpr auto NE          = std::string_view{"!="};
constexpr auto LT          = std::string_view{"<"};
constexpr auto LE          = std::string_view{"<="};
constexpr auto GT          = std::string_view{">"};
constexpr auto GE          = std::string_view{">="};
constexpr auto OR          = std::string_view{"or"};
constexpr auto AND         = std::string_view{"and"};
constexpr auto INPUT       = std::string_view{"input"};

auto parse_file(const std::string& file) -> std::vector<anzu::op>;

}
}