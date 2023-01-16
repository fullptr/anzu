#include "operators.hpp"
#include "object.hpp"
#include "vocabulary.hpp"
#include "program.hpp"
#include "utility/memory.hpp"

#include <algorithm>
#include <functional>

namespace anzu {
namespace {

template <typename Type, template <typename> typename Op>
auto unary_op(std::vector<std::byte>& mem)
{
    static constexpr auto op = Op<Type>{};
    const auto obj = pop_value<Type>(mem);
    push_value(mem, op(obj));
}

}
auto resolve_unary_op(const unary_op_description& desc) -> std::optional<unary_op_info>
{
    const auto& type = desc.type;
    if (type == i32_type()) {
        if (desc.op == tk_sub) {
            return unary_op_info{ unary_op<std::int32_t, std::negate>, type };
        }
    }
    else if (type == i64_type()) {
        if (desc.op == tk_sub) {
            return unary_op_info{ unary_op<std::int64_t, std::negate>, type };
        }
    }
    else if (type == f64_type()) {
        if (desc.op == tk_sub) {
            return unary_op_info{ unary_op<double, std::negate>, type };
        }
    }
    else if (type == bool_type()) {
        if (desc.op == tk_bang) {
            return unary_op_info{ unary_op<bool, std::logical_not>, type };
        }
    }
    return std::nullopt;
}

}