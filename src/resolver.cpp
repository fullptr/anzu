#include "resolver.hpp"
#include "program.hpp"

namespace anzu {

auto resolve_operation(
    const type_name& lhs, const type_name& rhs, const std::string& operation
)
    -> std::optional<op_info>
{
    if (lhs != rhs) return std::nullopt;

    const auto& type = lhs;
    if (type == i32_type()) {
        if (operation == "+") return op_info{ {op_i32_add{}}, type };
        if (operation == "-") return op_info{ {op_i32_sub{}}, type };
        if (operation == "*") return op_info{ {op_i32_mul{}}, type };
        if (operation == "/") return op_info{ {op_i32_div{}}, type };

        if (operation == "==") return op_info{ {op_i32_eq{}}, bool_type() };
        if (operation == "!=") return op_info{ {op_i32_ne{}}, bool_type() };
        if (operation == "<")  return op_info{ {op_i32_lt{}}, bool_type() };
        if (operation == "<=") return op_info{ {op_i32_le{}}, bool_type() };
        if (operation == ">")  return op_info{ {op_i32_gt{}}, bool_type() };
        if (operation == ">=") return op_info{ {op_i32_ge{}}, bool_type() };
    }
    else if (type == i64_type()) {
        if (operation == "+") return op_info{ {op_i64_add{}}, type };
        if (operation == "-") return op_info{ {op_i64_sub{}}, type };
        if (operation == "*") return op_info{ {op_i64_mul{}}, type };
        if (operation == "/") return op_info{ {op_i64_div{}}, type };

        if (operation == "==") return op_info{ {op_i64_eq{}}, bool_type() };
        if (operation == "!=") return op_info{ {op_i64_ne{}}, bool_type() };
        if (operation == "<")  return op_info{ {op_i64_lt{}}, bool_type() };
        if (operation == "<=") return op_info{ {op_i64_le{}}, bool_type() };
        if (operation == ">")  return op_info{ {op_i64_gt{}}, bool_type() };
        if (operation == ">=") return op_info{ {op_i64_ge{}}, bool_type() };
    }
    else if (type == u64_type()) {
        if (operation == "+") return op_info{ {op_u64_add{}}, type };
        if (operation == "-") return op_info{ {op_u64_sub{}}, type };
        if (operation == "*") return op_info{ {op_u64_mul{}}, type };
        if (operation == "/") return op_info{ {op_u64_div{}}, type };

        if (operation == "==") return op_info{ {op_u64_eq{}}, bool_type() };
        if (operation == "!=") return op_info{ {op_u64_ne{}}, bool_type() };
        if (operation == "<")  return op_info{ {op_u64_lt{}}, bool_type() };
        if (operation == "<=") return op_info{ {op_u64_le{}}, bool_type() };
        if (operation == ">")  return op_info{ {op_u64_gt{}}, bool_type() };
        if (operation == ">=") return op_info{ {op_u64_ge{}}, bool_type() };
    }
    else if (type == f64_type()) {
        if (operation == "+") return op_info{ {op_f64_add{}}, type };
        if (operation == "-") return op_info{ {op_f64_sub{}}, type };
        if (operation == "*") return op_info{ {op_f64_mul{}}, type };
        if (operation == "/") return op_info{ {op_f64_div{}}, type };

        if (operation == "==") return op_info{ {op_f64_eq{}}, bool_type() };
        if (operation == "!=") return op_info{ {op_f64_ne{}}, bool_type() };
        if (operation == "<")  return op_info{ {op_f64_lt{}}, bool_type() };
        if (operation == "<=") return op_info{ {op_f64_le{}}, bool_type() };
        if (operation == ">")  return op_info{ {op_f64_gt{}}, bool_type() };
        if (operation == ">=") return op_info{ {op_f64_ge{}}, bool_type() };
    }
    else if (type == bool_type()) {
        if (operation == "&&") return op_info{ {op_bool_and{}}, type };
        if (operation == "||") return op_info{ {op_bool_or{}}, type };
        if (operation == "==") return op_info{ {op_bool_eq{}}, type };
        if (operation == "!=") return op_info{ {op_bool_ne{}}, type };
    }
    return std::nullopt;
}

}