#include "operators.hpp"
#include "object.hpp"

#include <algorithm>
#include <functional>

namespace anzu {
namespace {

template <typename T>
auto get_back(std::vector<block>& mem, std::size_t index) -> T&
{
    return std::get<std::remove_cvref_t<T>>(mem[mem.size() - index - 1]);
}

auto pop_n(std::vector<block>& mem, std::size_t n) -> void
{
    mem.resize(mem.size() - n);
}

template <typename Type, template <typename> typename Op>
auto bin_op()
{
    return [=](std::vector<block>& mem) {
        const auto op = Op<Type>{};
        const auto rhs = get_back<Type>(mem, 0);
        const auto lhs = get_back<Type>(mem, 1);
        mem.pop_back();
        if constexpr (std::is_same_v<bool, decltype(op(lhs, rhs))>) {
            mem.back().emplace<block_byte>(static_cast<block_byte>(op(lhs, rhs)));
        } else {
            mem.back().emplace<decltype(op(lhs, rhs))>(op(lhs, rhs));
        }
    };
}

template <typename Type, template <typename> typename Op>
auto bin_op_bytes(std::vector<block>& mem) -> void
{
    const auto op = Op<Type>{};
    auto lhs_bytes = std::vector<block>{};
    for (std::size_t i = 0; i != sizeof(Type); ++i) {
        lhs_bytes.push_back(mem[mem.size() - (2 * sizeof(Type)) + i]);
    }
    const auto lhs = from_bytes<Type>(lhs_bytes);
    auto rhs_bytes = std::vector<block>{};
    for (std::size_t i = 0; i != sizeof(Type); ++i) {
        rhs_bytes.push_back(mem[mem.size() - sizeof(Type) + i]);
    }
    const auto rhs = from_bytes<Type>(rhs_bytes);
    pop_n(mem, 2 * sizeof(Type));
    const auto ret = op(lhs, rhs);
    const auto ret_bytes = std::bit_cast<std::array<std::byte, sizeof(ret)>>(ret);
    for (const auto& b : ret_bytes) {
        mem.push_back(b);
    }
}

// TODO: Dedeup this, also appears in runtime
auto push_u64(std::vector<block>& mem, std::uint64_t value) -> void
{
    for (const auto& b : std::bit_cast<std::array<std::byte, sizeof(std::uint64_t)>>(value)) {
        mem.push_back(b);
    }
}

// TODO: Dedeup this, also appears in runtime
auto pop_u64(std::vector<block>& mem) -> std::uint64_t
{
    auto bytes = std::array<std::byte, sizeof(std::uint64_t)>{};
    for (std::size_t i = 0; i != sizeof(std::uint64_t); ++i) {
        bytes[i] = std::get<std::byte>(mem[mem.size() - sizeof(std::uint64_t) + i]);
    }
    for (std::size_t i = 0; i != sizeof(std::uint64_t); ++i) {
        mem.pop_back();
    }
    return std::bit_cast<std::uint64_t>(bytes);
}

// Top of stack: [ptr], [size], [offset]
// offset is popped, size stays the same, ptr is modified
auto ptr_addition(std::vector<block>& mem)
{
    const auto offset = pop_u64(mem);
    const auto size = pop_u64(mem);
    const auto ptr = pop_u64(mem);

    push_u64(mem, ptr + offset);
    push_u64(mem, size);
}

// TODO: use memcmp here when we have just bytes
auto eq_comparison(std::size_t bytes)
{
    return [=](std::vector<block>& mem) {
        auto lhs_bytes = std::vector<block_byte>{};
        for (std::size_t i = 0; i != bytes; ++i) {
            lhs_bytes.push_back(std::get<block_byte>(mem[mem.size() - (2 * bytes) + i]));
        }

        auto rhs_bytes = std::vector<block_byte>{};
        for (std::size_t i = 0; i != bytes; ++i) {
            rhs_bytes.push_back(std::get<block_byte>(mem[mem.size() - bytes + i]));
        }

        const auto result = lhs_bytes == rhs_bytes;
        pop_n(mem, 2 * bytes);
        mem.push_back(block_byte{result});
    };
}

auto ne_comparison(std::size_t bytes)
{
    return [=](std::vector<block>& mem) {
        auto lhs_bytes = std::vector<block_byte>{};
        for (std::size_t i = 0; i != bytes; ++i) {
            lhs_bytes.push_back(std::get<block_byte>(mem[mem.size() - (2 * bytes) + i]));
        }

        auto rhs_bytes = std::vector<block_byte>{};
        for (std::size_t i = 0; i != bytes; ++i) {
            rhs_bytes.push_back(std::get<block_byte>(mem[mem.size() - bytes + i]));
        }

        const auto result = lhs_bytes != rhs_bytes;
        pop_n(mem, 2 * bytes);
        mem.push_back(block_byte{result});
    };
}

template <typename Type, template <typename> typename Op>
auto unary_op(std::vector<block>& mem)
{
    const auto op = Op<Type>{};
    auto& obj = get_back<Type>(mem, 0);
    obj = op(obj);
}

template <typename Type, template <typename> typename Op>
auto unary_op_sized(std::vector<block>& mem)
{
    const auto op = Op<Type>{};
    auto obj_bytes = std::vector<block>{};
    for (std::size_t i = 0; i != sizeof(Type); ++i) {
        obj_bytes.push_back(mem[mem.size() - (2 * sizeof(Type)) + i]);
    }
    const auto obj = from_bytes<Type>(obj_bytes);
    pop_n(mem, sizeof(Type));
    const auto ret = op(obj);
    const auto ret_bytes = std::bit_cast<std::array<std::byte, sizeof(ret)>>(ret);
    for (const auto& b : ret_bytes) {
        mem.push_back(b);
    }
}

auto bool_negate(std::vector<block>& mem)
{
    auto& top = get_back<block_byte>(mem, 0);
    top = (top == block_byte{1}) ? block_byte{0} : block_byte{1};
}

}

auto resolve_binary_op(const binary_op_description& desc) -> std::optional<binary_op_info>
{
    if (is_ptr_type(desc.lhs) && desc.rhs == u64_type()) {
        return binary_op_info{ ptr_addition, desc.lhs };
    }

    if (desc.lhs != desc.rhs) {
        return std::nullopt;
    }

    const auto& type = desc.lhs;
    
    if (is_list_type(type)) { // No support for having these in bin ops.
        return std::nullopt;
    }

    if (type == i32_type()) {
        if (desc.op == tk_add) {
            return binary_op_info{ bin_op_bytes<std::int32_t, std::plus>, type };
        } else if (desc.op == tk_sub) {
            return binary_op_info{ bin_op_bytes<std::int32_t, std::minus>, type };
        } else if (desc.op == tk_mul) {
            return binary_op_info{ bin_op_bytes<std::int32_t, std::multiplies>, type };
        } else if (desc.op == tk_div) {
            return binary_op_info{ bin_op_bytes<std::int32_t, std::divides>, type };
        } else if (desc.op == tk_mod) {
            return binary_op_info{ bin_op_bytes<std::int32_t, std::modulus>, type };
        } else if (desc.op == tk_lt) {
            return binary_op_info{ bin_op_bytes<std::int32_t, std::less>, bool_type() };
        } else if (desc.op == tk_le) {
            return binary_op_info{ bin_op_bytes<std::int32_t, std::less_equal>, bool_type() };
        } else if (desc.op == tk_gt) {
            return binary_op_info{ bin_op_bytes<std::int32_t, std::greater>, bool_type() };
        } else if (desc.op == tk_ge) {
            return binary_op_info{ bin_op_bytes<std::int32_t, std::greater_equal>, bool_type() };
        } else if (desc.op == tk_eq) {
            return binary_op_info{ bin_op_bytes<std::int32_t, std::equal_to>, bool_type() };
        } else if (desc.op == tk_ne) {
            return binary_op_info{ bin_op_bytes<std::int32_t, std::not_equal_to>, bool_type() };
        }
    }
    else if (type == i64_type()) {
        if (desc.op == tk_add) {
            return binary_op_info{ bin_op_bytes<std::int64_t, std::plus>, type };
        } else if (desc.op == tk_sub) {
            return binary_op_info{ bin_op_bytes<std::int64_t, std::minus>, type };
        } else if (desc.op == tk_mul) {
            return binary_op_info{ bin_op_bytes<std::int64_t, std::multiplies>, type };
        } else if (desc.op == tk_div) {
            return binary_op_info{ bin_op_bytes<std::int64_t, std::divides>, type };
        } else if (desc.op == tk_mod) {
            return binary_op_info{ bin_op_bytes<std::int64_t, std::modulus>, type };
        } else if (desc.op == tk_lt) {
            return binary_op_info{ bin_op_bytes<std::int64_t, std::less>, bool_type() };
        } else if (desc.op == tk_le) {
            return binary_op_info{ bin_op_bytes<std::int64_t, std::less_equal>, bool_type() };
        } else if (desc.op == tk_gt) {
            return binary_op_info{ bin_op_bytes<std::int64_t, std::greater>, bool_type() };
        } else if (desc.op == tk_ge) {
            return binary_op_info{ bin_op_bytes<std::int64_t, std::greater_equal>, bool_type() };
        } else if (desc.op == tk_eq) {
            return binary_op_info{ bin_op_bytes<std::int64_t, std::equal_to>, bool_type() };
        } else if (desc.op == tk_ne) {
            return binary_op_info{ bin_op_bytes<std::int64_t, std::not_equal_to>, bool_type() };
        }
    }
    else if (type == u64_type()) {
        if (desc.op == tk_add) {
            return binary_op_info{ bin_op_bytes<std::uint64_t, std::plus>, type };
        } else if (desc.op == tk_sub) {
            return binary_op_info{ bin_op_bytes<std::uint64_t, std::minus>, type };
        } else if (desc.op == tk_mul) {
            return binary_op_info{ bin_op_bytes<std::uint64_t, std::multiplies>, type };
        } else if (desc.op == tk_div) {
            return binary_op_info{ bin_op_bytes<std::uint64_t, std::divides>, type };
        } else if (desc.op == tk_mod) {
            return binary_op_info{ bin_op_bytes<std::uint64_t, std::modulus>, type };
        } else if (desc.op == tk_lt) {
            return binary_op_info{ bin_op_bytes<std::uint64_t, std::less>, bool_type() };
        } else if (desc.op == tk_le) {
            return binary_op_info{ bin_op_bytes<std::uint64_t, std::less_equal>, bool_type() };
        } else if (desc.op == tk_gt) {
            return binary_op_info{ bin_op_bytes<std::uint64_t, std::greater>, bool_type() };
        } else if (desc.op == tk_ge) {
            return binary_op_info{ bin_op_bytes<std::uint64_t, std::greater_equal>, bool_type() };
        } else if (desc.op == tk_eq) {
            return binary_op_info{ bin_op_bytes<std::uint64_t, std::equal_to>, bool_type() };
        } else if (desc.op == tk_ne) {
            return binary_op_info{ bin_op_bytes<std::uint64_t, std::not_equal_to>, bool_type() };
        }
    }
    else if (type == f64_type()) {
        if (desc.op == tk_add) {
            return binary_op_info{ bin_op_bytes<double, std::plus>, type };
        } else if (desc.op == tk_sub) {
            return binary_op_info{ bin_op_bytes<double, std::minus>, type };
        } else if (desc.op == tk_mul) {
            return binary_op_info{ bin_op_bytes<double, std::multiplies>, type };
        } else if (desc.op == tk_div) {
            return binary_op_info{ bin_op_bytes<double, std::divides>, type };
        } else if (desc.op == tk_lt) {
            return binary_op_info{ bin_op_bytes<double, std::less>, bool_type() };
        } else if (desc.op == tk_le) {
            return binary_op_info{ bin_op_bytes<double, std::less_equal>, bool_type() };
        } else if (desc.op == tk_gt) {
            return binary_op_info{ bin_op_bytes<double, std::greater>, bool_type() };
        } else if (desc.op == tk_ge) {
            return binary_op_info{ bin_op_bytes<double, std::greater_equal>, bool_type() };
        } else if (desc.op == tk_eq) {
            return binary_op_info{ bin_op_bytes<double, std::equal_to>, bool_type() };
        } else if (desc.op == tk_ne) {
            return binary_op_info{ bin_op_bytes<double, std::not_equal_to>, bool_type() };
        }
    }
    else if (type == bool_type()) {
        if (desc.op == tk_and) {
            return binary_op_info{ bin_op_bytes<bool, std::logical_and>, type };
        } else if (desc.op == tk_or) {
            return binary_op_info{ bin_op_bytes<bool, std::logical_or>, type };
        } else if (desc.op == tk_eq) {
            return binary_op_info{ bin_op_bytes<bool, std::equal_to>, bool_type() };
        } else if (desc.op == tk_ne) {
            return binary_op_info{ bin_op_bytes<bool, std::not_equal_to>, bool_type() };
        }
    }
    else if (type == char_type()) {
        if (desc.op == tk_eq) {
            return binary_op_info{ bin_op_bytes<char, std::equal_to>, bool_type() };
        } else if (desc.op == tk_ne) {
            return binary_op_info{ bin_op_bytes<char, std::equal_to>, bool_type() };
        }
    }

    return std::nullopt;
}

auto resolve_unary_op(const unary_op_description& desc) -> std::optional<unary_op_info>
{
    const auto& type = desc.type;
    if (type == i32_type()) {
        if (desc.op == tk_sub) {
            return unary_op_info{ unary_op_sized<std::int32_t, std::negate>, type };
        }
    }
    else if (type == i64_type()) {
        if (desc.op == tk_sub) {
            return unary_op_info{ unary_op_sized<std::int64_t, std::negate>, type };
        }
    }
    else if (type == f64_type()) {
        if (desc.op == tk_sub) {
            return unary_op_info{ unary_op_sized<double, std::negate>, type };
        }
    }
    else if (type == bool_type()) {
        if (desc.op == tk_bang) {
            return unary_op_info{ bool_negate, type };
        }
    }
    return std::nullopt;
}

}