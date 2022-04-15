#pragma once
#include <format>
#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "type.hpp"
#include "utility/print.hpp"

namespace anzu {

// Want these to be equivalent since we want uints available in the runtime but we also want
// to use it as indexes into C++ vectors which use size_t.
static_assert(std::is_same_v<std::uint64_t, std::size_t>);

struct object
{
    std::vector<std::byte> data;
    anzu::type_name        type;
};

auto to_string(const object& object) -> std::string;

auto make_i32(std::int32_t val) -> object;
auto make_i64(std::int64_t val) -> object;
auto make_u64(std::uint64_t val) -> object;
auto make_f64(double val) -> object;
auto make_char(char val) -> object;
auto make_bool(bool val) -> object;
auto make_null() -> object;

// Should be elsewhere
auto format_special_chars(const std::string& str) -> std::string;

template <typename T>
inline auto to_bytes(const T& val) -> std::vector<std::byte>
{
    auto ret = std::vector<std::byte>{};
    for (const auto b : std::bit_cast<std::array<std::byte, sizeof(T)>>(val)) {
        ret.push_back(b);
    }
    return ret;
}

template <typename T>
inline auto from_bytes(const std::vector<std::byte>& val) -> T
{
    if (val.size() != sizeof(T)) {
        print("oh no, size = {}\n", val.size());
        std::exit(1);
    }
    auto bytes = std::array<std::byte, sizeof(T)>{};
    for (std::size_t i = 0; i != sizeof(T); ++i) {
        bytes[i] = val[i];
    }
    return std::bit_cast<T>(bytes);
}

}

template <> struct std::formatter<std::byte> : std::formatter<std::string> {
    auto format(std::byte b, auto& ctx) {
        const auto str = std::format("{:X}", static_cast<unsigned char>(b));
        return std::formatter<std::string>::format(str, ctx);
    }
};

template <> struct std::formatter<anzu::object> : std::formatter<std::string> {
    auto format(const anzu::object& obj, auto& ctx) {
        return std::formatter<std::string>::format(to_string(obj), ctx);
    }
};