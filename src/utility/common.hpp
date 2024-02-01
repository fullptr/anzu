#pragma once
#include <ranges>
#include <format>
#include <source_location>
#include <chrono>
#include <iostream>
#include <print>

namespace anzu {

template <typename T, typename Transform>
auto format_comma_separated(const std::vector<T>& values, Transform&& transform) -> std::string
{
    std::string ret;
    if (!values.empty()) {
        ret += std::format("{}", transform(values.front()));
        for (const auto& value : values | std::views::drop(1)) {
            ret += std::format(", {}", transform(value));
        }
    }
    return ret;
}

template <typename T>
auto format_comma_separated(const std::vector<T>& values) -> std::string
{
    return format_comma_separated(values, [](auto&& x) { return std::format("{}", x); });
}

template <typename T, typename Transform>
auto print_comma_separated(const std::vector<T>& values, Transform&& transform) -> void
{
    std::cout << format_comma_separated(values, std::forward<Transform>(transform));
}
    
template <class... Args>
struct panic_format {
    template <class FormatStr>
    consteval panic_format(
        const FormatStr &s,
        std::source_location loc = std::source_location::current()
    ) noexcept
        : fmt{s}
        , loc{loc}
    {}

    std::format_string<Args...> fmt;
    std::source_location        loc;
};

template <class... Args>
[[noreturn]] auto panic(
    panic_format<std::type_identity_t<Args>...> fmt,
    Args&&... args) noexcept -> void
{
    print(
        "{}:{} panic: {}\n",
        fmt.loc.file_name(),
        fmt.loc.line(),
        std::format(fmt.fmt, std::forward<Args>(args)...)
    );
    std::exit(1);
}

template <class... Args>
auto panic_if(
    bool condition,
    panic_format<std::type_identity_t<Args>...> fmt,
    Args&&... args) noexcept -> void
{
    if (condition) {
        panic(fmt, std::forward<Args>(args)...);
    }
}

inline auto range(std::size_t max) {
    return std::views::iota(std::size_t{0}, max);
}

template <typename R1, typename R2>
auto zip(const R1& r1, const R2& r2)
{
    const auto size = static_cast<int>(std::min(r1.size(), r2.size()));
    return std::views::iota(0, size)
         | std::views::transform([&](int idx) { return std::tie(r1.at(idx), r2.at(idx)); });
}

struct scope_timer
{
    using clock_type = std::chrono::steady_clock;
	using second_type = std::chrono::duration<double, std::ratio<1>>;
    
    clock_type::time_point start;

    scope_timer() : start(clock_type::now()) {}
    ~scope_timer()
    {
        const auto duration = std::chrono::steady_clock::now() - start;
        const auto time_elapsed = std::chrono::duration_cast<second_type>(duration).count();
        std::print("\n -> Program took {} seconds\n", time_elapsed);
    }
};

template <typename Callable>
class scope_exit
{
    scope_exit(const scope_exit&) = delete;
    scope_exit& operator=(const scope_exit&) = delete;

    Callable d_callable;

public:
    scope_exit(const Callable& callable) : d_callable{callable} {}
    ~scope_exit() { d_callable(); }
};

template <typename... Ts>
struct overloaded : Ts...
{
    using Ts::operator()...;
};

}