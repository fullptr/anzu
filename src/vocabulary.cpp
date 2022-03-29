#include "vocabulary.hpp"

#include <string_view>
#include <unordered_set>

namespace anzu {

auto is_keyword(std::string_view token) -> bool
{
    static const std::unordered_set<std::string_view> tokens = {
        tk_break, tk_continue, tk_else, tk_false, tk_for, tk_if,
        tk_in, tk_null, tk_true, tk_while, tk_int, tk_float, tk_bool,
        tk_str, tk_list, tk_function, tk_return, tk_struct, tk_addrof,
        tk_deref
    };
    return tokens.contains(token);
}

auto is_sentinel(std::string_view token) -> bool
{
    static const std::unordered_set<std::string_view> tokens = {
        tk_else, tk_rbrace
    };
    return tokens.contains(token);
}

auto is_symbol(std::string_view token) -> bool
{
    static const std::unordered_set<std::string_view> tokens = {
        tk_add, tk_and, tk_declare, tk_colon, tk_comma,
        tk_div, tk_eq, tk_ge, tk_gt, tk_lbracket, tk_le,
        tk_lparen, tk_lt, tk_mod, tk_mul, tk_ne, tk_or,
        tk_period, tk_rbracket, tk_rparen, tk_sub, tk_rarrow,
        tk_lbrace, tk_rbrace, tk_assign, tk_declare, tk_fullstop
    };
    return tokens.contains(token);
}

auto is_comparison(sv token) -> bool
{
    static const std::unordered_set<std::string_view> tokens = {
        tk_lt, tk_le, tk_gt, tk_ge, tk_eq, tk_ne
    };
    return tokens.contains(token);
}

}