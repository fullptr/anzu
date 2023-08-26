#pragma once

namespace anzu {

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

}