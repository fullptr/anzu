#include "resolver.hpp"
#include "program.hpp"
#include "vocabulary.hpp"

namespace anzu {

auto resolve_operation(
    const type_name& lhs, const type_name& rhs, const std::string& operation
)
    -> std::optional<op_info>
{
    if (is_ptr_type(lhs) && rhs == u64_type()) return op_info{ {op_u64_add{}}, lhs };
    if (lhs != rhs) return std::nullopt;

    const auto& type = lhs;
    if (type == i32_type()) {
        if (operation == tk_add) return op_info{ {op_i32_add{}}, type };
        if (operation == tk_sub) return op_info{ {op_i32_sub{}}, type };
        if (operation == tk_mul) return op_info{ {op_i32_mul{}}, type };
        if (operation == tk_div) return op_info{ {op_i32_div{}}, type };

        if (operation == tk_eq) return op_info{ {op_i32_eq{}}, bool_type() };
        if (operation == tk_ne) return op_info{ {op_i32_ne{}}, bool_type() };
        if (operation == tk_lt) return op_info{ {op_i32_lt{}}, bool_type() };
        if (operation == tk_le) return op_info{ {op_i32_le{}}, bool_type() };
        if (operation == tk_gt) return op_info{ {op_i32_gt{}}, bool_type() };
        if (operation == tk_ge) return op_info{ {op_i32_ge{}}, bool_type() };
    }
    else if (type == i64_type()) {
        if (operation == tk_add) return op_info{ {op_i64_add{}}, type };
        if (operation == tk_sub) return op_info{ {op_i64_sub{}}, type };
        if (operation == tk_mul) return op_info{ {op_i64_mul{}}, type };
        if (operation == tk_div) return op_info{ {op_i64_div{}}, type };

        if (operation == tk_eq) return op_info{ {op_i64_eq{}}, bool_type() };
        if (operation == tk_ne) return op_info{ {op_i64_ne{}}, bool_type() };
        if (operation == tk_lt) return op_info{ {op_i64_lt{}}, bool_type() };
        if (operation == tk_le) return op_info{ {op_i64_le{}}, bool_type() };
        if (operation == tk_gt) return op_info{ {op_i64_gt{}}, bool_type() };
        if (operation == tk_ge) return op_info{ {op_i64_ge{}}, bool_type() };
    }
    else if (type == u64_type()) {
        if (operation == tk_add) return op_info{ {op_u64_add{}}, type };
        if (operation == tk_sub) return op_info{ {op_u64_sub{}}, type };
        if (operation == tk_mul) return op_info{ {op_u64_mul{}}, type };
        if (operation == tk_div) return op_info{ {op_u64_div{}}, type };

        if (operation == tk_eq) return op_info{ {op_u64_eq{}}, bool_type() };
        if (operation == tk_ne) return op_info{ {op_u64_ne{}}, bool_type() };
        if (operation == tk_lt) return op_info{ {op_u64_lt{}}, bool_type() };
        if (operation == tk_le) return op_info{ {op_u64_le{}}, bool_type() };
        if (operation == tk_gt) return op_info{ {op_u64_gt{}}, bool_type() };
        if (operation == tk_ge) return op_info{ {op_u64_ge{}}, bool_type() };
    }
    else if (type == f64_type()) {
        if (operation == tk_add) return op_info{ {op_f64_add{}}, type };
        if (operation == tk_sub) return op_info{ {op_f64_sub{}}, type };
        if (operation == tk_mul) return op_info{ {op_f64_mul{}}, type };
        if (operation == tk_div) return op_info{ {op_f64_div{}}, type };

        if (operation == tk_eq) return op_info{ {op_f64_eq{}}, bool_type() };
        if (operation == tk_ne) return op_info{ {op_f64_ne{}}, bool_type() };
        if (operation == tk_lt) return op_info{ {op_f64_lt{}}, bool_type() };
        if (operation == tk_le) return op_info{ {op_f64_le{}}, bool_type() };
        if (operation == tk_gt) return op_info{ {op_f64_gt{}}, bool_type() };
        if (operation == tk_ge) return op_info{ {op_f64_ge{}}, bool_type() };
    }
    else if (type == bool_type()) {
        if (operation == tk_and) return op_info{ {op_bool_and{}}, type };
        if (operation == tk_or) return op_info{ {op_bool_or{}}, type };
        if (operation == tk_eq) return op_info{ {op_bool_eq{}}, type };
        if (operation == tk_ne) return op_info{ {op_bool_ne{}}, type };
    }
    return std::nullopt;
}

}