#pragma once
#include <ranges>

namespace anzu {

template <typename R1, typename R2>
auto zip(const R1& r1, const R2& r2)
{
    const auto size = static_cast<int>(std::min(r1.size(), r2.size()));
    return std::views::iota(0, size)
         | std::views::transform([&](int idx) { return std::tie(r1.at(idx), r2.at(idx)); });
}

}