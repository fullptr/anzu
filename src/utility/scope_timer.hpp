#pragma once
#include "utility/print.hpp"

#include <chrono>

namespace anzu {

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
        anzu::print("\n -> Program took {} seconds\n", time_elapsed);
    }
};

}