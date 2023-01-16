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
    }
    else if (type == i64_type()) {
        if (operation == "+") return op_info{ {op_i64_add{}}, type };
        if (operation == "-") return op_info{ {op_i64_sub{}}, type };
        if (operation == "*") return op_info{ {op_i64_mul{}}, type };
        if (operation == "/") return op_info{ {op_i64_div{}}, type };
    }
    else if (type == u64_type()) {
        if (operation == "+") return op_info{ {op_u64_add{}}, type };
        if (operation == "-") return op_info{ {op_u64_sub{}}, type };
        if (operation == "*") return op_info{ {op_u64_mul{}}, type };
        if (operation == "/") return op_info{ {op_u64_div{}}, type };
    }
    else if (type == f64_type()) {
        if (operation == "+") return op_info{ {op_f64_add{}}, type };
        if (operation == "-") return op_info{ {op_f64_sub{}}, type };
        if (operation == "*") return op_info{ {op_f64_mul{}}, type };
        if (operation == "/") return op_info{ {op_f64_div{}}, type };
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