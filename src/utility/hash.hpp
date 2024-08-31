#include <vector>
#include <utility>
#include <concepts>
#include <cstddef>

namespace anzu {

template <typename T>
concept has_member_hash = requires(T t) {
    { t.to_hash() } -> std::convertible_to<std::size_t>;
};

auto hash(auto&& obj) -> std::size_t
{
    using T = std::remove_cvref_t<decltype(obj)>;
    if constexpr (has_member_hash<T>) {
        return obj.to_hash();
    } else {
        return std::hash<T>{}(obj);
    }
}

template <typename T>
auto hash(const std::vector<T>& objs) -> std::size_t
{
    auto val = std::size_t{0};
    for (const auto& obj : objs) {
        val ^= hash(obj);
    }
    return val;
}

template <typename First, typename Second, typename... Args>
auto hash(First&& first, Second&& second, Args&&... args) -> std::size_t
{
    return ((hash(first) ^ hash(second)) ^ ... ^ hash(args));
}

}

template <anzu::has_member_hash T>
struct std::hash<T>
{
    auto operator()(const T& obj) const -> std::size_t
    {
        return obj.to_hash();
    }
};