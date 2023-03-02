#include "resolver.hpp"
#include "program.hpp"

namespace anzu {

auto resolve_operation(const type_name& lhs, const type_name& rhs, lex_token_type op)
    -> std::optional<op_info>
{
    using tt = lex_token_type;

    if (lhs != rhs) return std::nullopt;

    const auto& type = lhs;
    if (type == char_type()) {
        switch (op) {
            case tt::bang:       return op_info{ {op_char_eq{}}, bool_type() };
            case tt::bang_equal: return op_info{ {op_char_ne{}}, bool_type() };
        }
    }
    else if (type == i32_type()) {
        switch (op) {
            case tt::plus:    return op_info{ {op_i32_add{}}, type };
            case tt::minus:   return op_info{ {op_i32_sub{}}, type };
            case tt::star:    return op_info{ {op_i32_mul{}}, type };
            case tt::slash:   return op_info{ {op_i32_div{}}, type };
            case tt::percent: return op_info{ {op_i32_mod{}}, type };
        }
    }
    else if (type == i64_type()) {
        switch (op) {
            case tt::plus:          return op_info{ {op_i64_add{}}, type };
            case tt::minus:         return op_info{ {op_i64_sub{}}, type };
            case tt::star:          return op_info{ {op_i64_mul{}}, type };
            case tt::slash:         return op_info{ {op_i64_div{}}, type };
            case tt::percent:       return op_info{ {op_i64_mod{}}, type };

            case tt::equal_equal:   return op_info{ {op_i64_eq{}}, bool_type() };
            case tt::bang_equal:    return op_info{ {op_i64_ne{}}, bool_type() };
            case tt::less:          return op_info{ {op_i64_lt{}}, bool_type() };
            case tt::less_equal:    return op_info{ {op_i64_le{}}, bool_type() };
            case tt::greater:       return op_info{ {op_i64_gt{}}, bool_type() };
            case tt::greater_equal: return op_info{ {op_i64_ge{}}, bool_type() };
        }
    }
    else if (type == u64_type()) {
        switch (op) {
            case tt::plus:          return op_info{ {op_u64_add{}}, type };
            case tt::minus:         return op_info{ {op_u64_sub{}}, type };
            case tt::star:          return op_info{ {op_u64_mul{}}, type };
            case tt::slash:         return op_info{ {op_u64_div{}}, type };
            case tt::percent:       return op_info{ {op_u64_mod{}}, type };

            case tt::equal_equal:   return op_info{ {op_u64_eq{}}, bool_type() };
            case tt::bang_equal:    return op_info{ {op_u64_ne{}}, bool_type() };
            case tt::less:          return op_info{ {op_u64_lt{}}, bool_type() };
            case tt::less_equal:    return op_info{ {op_u64_le{}}, bool_type() };
            case tt::greater:       return op_info{ {op_u64_gt{}}, bool_type() };
            case tt::greater_equal: return op_info{ {op_u64_ge{}}, bool_type() };
        }
    }
    else if (type == f64_type()) {
        switch (op) {
            case tt::plus:          return op_info{ {op_f64_add{}}, type };
            case tt::minus:         return op_info{ {op_f64_sub{}}, type };
            case tt::star:          return op_info{ {op_f64_mul{}}, type };
            case tt::slash:         return op_info{ {op_f64_div{}}, type };

            case tt::equal_equal:   return op_info{ {op_f64_eq{}}, bool_type() };
            case tt::bang_equal:    return op_info{ {op_f64_ne{}}, bool_type() };
            case tt::less:          return op_info{ {op_f64_lt{}}, bool_type() };
            case tt::less_equal:    return op_info{ {op_f64_le{}}, bool_type() };
            case tt::greater:       return op_info{ {op_f64_gt{}}, bool_type() };
            case tt::greater_equal: return op_info{ {op_f64_ge{}}, bool_type() };
        }
    }
    else if (type == bool_type()) {
        switch (op) {
            case tt::ampersand_ampersand: return op_info{ {op_bool_and{}}, type };
            case tt::bar_bar:             return op_info{ {op_bool_or{}},  type };
            case tt::equal_equal:         return op_info{ {op_bool_eq{}},  type };
            case tt::bang_equal:          return op_info{ {op_bool_ne{}},  type };
        }
    }
    return std::nullopt;
}

auto resolve_operation(const type_name& type, lex_token_type op)
    -> std::optional<op_info>
{
    using tt = lex_token_type;

    if (type == i32_type()) {
        if (op == tt::minus) return op_info{ {op_i32_neg{}}, type };
    }
    else if (type == i64_type()) {
        if (op == tt::minus) return op_info{ {op_i64_neg{}}, type };
    }
    else if (type == f64_type()) {
        if (op == tt::minus) return op_info{ {op_f64_neg{}}, type };
    }
    else if (type == bool_type()) {
        if (op == tt::bang) return op_info{ {op_bool_not{}}, type };
    }
    return std::nullopt;
}

}