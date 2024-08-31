#include <vector>
#include <utility>
#include <concepts>
#include <cstddef>

namespace anzu {

template <typename T>
concept has_member_hash = requires(T t) {
    { t.to_hash() } -> std::convertible_to<std::size_t>;
};

auto simple_hash(auto&& obj) -> std::size_t
{
    using T = std::remove_cvref_t<decltype(obj)>;
    if constexpr (has_member_hash<T>) {
        return obj.to_hash();
    } else {
        return std::hash<T>{}(obj);
    }
}

template <typename T>
auto simple_hash(const std::vector<T>& objs) -> std::size_t
{
    auto hash = std::size_t{0};
    for (const auto& obj : objs) {
        hash ^= simple_hash(obj);
    }
    return hash;
}

template <typename... Args>
auto var_hash(Args&&... args) -> std::size_t
{
    return (simple_hash(args) ^ ...);
}

}

template<anzu::has_member_hash T>
struct std::hash<T>
{
    auto operator()(const T& obj) const -> std::size_t
    {
        return obj.to_hash();
    }
};