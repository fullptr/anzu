#pragma once

namespace anzu {

template <typename... Ts>
struct overloaded : Ts...
{
    using Ts::operator()...;
};

}