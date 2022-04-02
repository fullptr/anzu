#pragma once
#include <memory>

namespace anzu {

template <typename T>
class value_ptr
{
    std::unique_ptr<T> d_value;

public:
    value_ptr(T* value)
        : d_value(std::unique_ptr<T>(value))
    {
    }

    value_ptr(const value_ptr& other)
        : d_value(std::make_unique<T>(*other))
    {
    }

    value_ptr& operator=(const value_ptr& other)
    {
        *d_value = *other.d_value;
        return *this;
    }

    value_ptr(const T& val)
        : d_value(std::make_unique<T>(val))
    {
    }

    value_ptr& operator=(value_ptr&& other)
    {
        d_value = std::move(other.d_value);
        other.d_value.reset();
        return *this;
    }

    T& operator*() { return *d_value; }
    const T& operator*() const { return *d_value; }

    T* operator->() { return d_value.get(); }
    const T* operator->() const { return d_value.get(); }

    T* get() { return d_value.get(); }
    const T* get() const { return d_value.get(); }

    void reset() { d_value = nullptr; }

    bool operator==(const value_ptr& other) const
    {
        return *d_value == *other.d_value;
    }
};

template <typename T, typename... Args>
auto make_value(Args&&... args) -> value_ptr<T>
{
    return value_ptr(new T{std::forward<Args>(args)...});
}

}