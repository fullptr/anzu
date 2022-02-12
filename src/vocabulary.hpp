#pragma once
#include <string_view>

namespace anzu {

using sv = std::string_view;

// Keywords
constexpr auto tk_break     = sv{"break"};
constexpr auto tk_continue  = sv{"continue"};
constexpr auto tk_else      = sv{"else"};
constexpr auto tk_false     = sv{"false"};
constexpr auto tk_for       = sv{"for"};
constexpr auto tk_if        = sv{"if"};
constexpr auto tk_in        = sv{"in"};
constexpr auto tk_null      = sv{"null"};
constexpr auto tk_true      = sv{"true"};
constexpr auto tk_while     = sv{"while"};
constexpr auto tk_function  = sv{"fn"};
constexpr auto tk_return    = sv{"return"};
constexpr auto tk_debug     = sv{"__debug__"};

// Builtin Types
constexpr auto tk_int       = sv{"int"};
constexpr auto tk_bool      = sv{"bool"};
constexpr auto tk_str       = sv{"str"};
constexpr auto tk_list      = sv{"list"};
constexpr auto tk_null_type = sv{"null_type"};
constexpr auto tk_any       = sv{"any"};

// Symbols
constexpr auto tk_add       = sv{"+"};
constexpr auto tk_and       = sv{"&&"};
constexpr auto tk_assign    = sv{"="};
constexpr auto tk_colon     = sv{":"};
constexpr auto tk_comma     = sv{","};
constexpr auto tk_div       = sv{"/"};
constexpr auto tk_eq        = sv{"=="};
constexpr auto tk_ge        = sv{">="};
constexpr auto tk_gt        = sv{">"};
constexpr auto tk_lbracket  = sv{"["};
constexpr auto tk_le        = sv{"<="};
constexpr auto tk_lparen    = sv{"("};
constexpr auto tk_lt        = sv{"<"};
constexpr auto tk_mod       = sv{"%"};
constexpr auto tk_mul       = sv{"*"};
constexpr auto tk_ne        = sv{"!="};
constexpr auto tk_or        = sv{"||"};
constexpr auto tk_period    = sv{"."};
constexpr auto tk_rbracket  = sv{"]"};
constexpr auto tk_rparen    = sv{")"};
constexpr auto tk_sub       = sv{"-"};
constexpr auto tk_rarrow    = sv{"->"};
constexpr auto tk_lbrace    = sv{"{"};
constexpr auto tk_rbrace    = sv{"}"};

auto is_keyword    (sv token) -> bool;
auto is_sentinel   (sv token) -> bool;
auto is_symbol     (sv token) -> bool;
auto is_comparison (sv token) -> bool;
auto is_type       (sv token) -> bool;

}