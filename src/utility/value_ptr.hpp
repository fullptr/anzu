#pragma once
#include <memory>

namespace anzu {

template <typename T>
class value_ptr
{
    std::unique_ptr<T> d_value;

public:
    value_ptr() : d_value{nullptr} {}
    value_ptr(T* value) : d_value(std::unique_ptr<T>(value)) {}
    value_ptr(const value_ptr& other) : d_value(std::make_unique<T>(*other)) {}
    value_ptr(const T& val) : d_value(std::make_unique<T>(val)) {}

    auto operator=(const value_ptr& other) -> value_ptr&
    {
        *d_value = *other.d_value;
        return *this;
    }

    auto operator=(value_ptr&& other) -> value_ptr&
    {
        d_value = std::move(other.d_value);
        return *this;
    }

    auto operator*() -> T& { return *d_value; }
    auto operator*() const -> const T& { return *d_value; }

    auto operator->() -> T* { return d_value.get(); }
    auto operator->() const -> const T* { return d_value.get(); }

    auto get() -> T* { return d_value.get(); }
    auto get() const -> const T* { return d_value.get(); }

    void reset(T* ptr = nullptr) { d_value.reset(ptr); }

    auto operator==(const value_ptr& other) const -> bool
    {
        return *d_value == *other.d_value;
    }

    auto to_hash() const -> std::size_t { return var_hash(*d_value); }
};

template <typename T, typename... Args>
auto make_value(Args&&... args) -> value_ptr<T>
{
    return value_ptr(new T{std::forward<Args>(args)...});
}

}