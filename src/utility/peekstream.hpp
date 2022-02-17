#pragma once
#include <ranges>

namespace anzu {

template <std::ranges::range T>
class peekstream
{
    T::const_iterator d_begin;
    T::const_iterator d_curr;
    T::const_iterator d_end;

    using value_type = typename T::value_type;

public:
    peekstream(const T& range)
        : d_begin(std::ranges::begin(range))
        , d_curr(std::ranges::begin(range))
        , d_end(std::ranges::end(range))
    {}

    auto valid() const -> bool { return d_curr != d_end; }
    auto has_next() const -> bool { return std::next(d_curr) != d_end; }

    auto curr() const -> const value_type& { return *d_curr; }
    auto next() const -> const value_type& { return *std::next(d_curr); }
    auto position() const -> std::int64_t { return std::distance(d_begin, d_curr) + 1; }

    auto consume() -> value_type
    {
        auto ret = curr();
        ++d_curr;
        return ret;
    }

    auto consume_maybe(const value_type& val) -> bool
    {
        if (curr() == val) {
            consume();
            return true;
        }
        return false;
    };
};

}