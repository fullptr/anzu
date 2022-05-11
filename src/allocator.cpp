#include "allocator.hpp"
#include "utility/print.hpp"

#include <limits>

namespace anzu {
namespace {

using map = std::map<std::size_t, std::size_t>;
using iter = typename map::iterator;
auto try_merge_left(map& pool, iter lhs, iter rhs) -> iter
{
    auto& [lhs_ptr, lhs_size] = *lhs;
    auto& [rhs_ptr, rhs_size] = *rhs;
    if (lhs_ptr + lhs_size == rhs_ptr) {
        lhs_size += rhs_size;
        pool.erase(rhs);
        return lhs;
    }
    return rhs;
}

}

auto memory_allocator::allocate(std::size_t size) -> std::size_t
{
    d_bytes_allocated += size;

    for (auto it = d_pools.begin(); it != d_pools.end(); ++it) {
        auto& [pool_ptr, pool_size] = *it;
        if (size <= pool_size) { // Can fit in this block pool.
            const auto ptr = pool_ptr + pool_size - size;
            pool_size -= size; // Put at the end of the pool and shrink it.
            if (pool_size == 0) {
                d_pools.erase(it);
            }
            return ptr;
        }
    }

    // If there is a pool at the end and the block we want to allocate is larger than
    // it, use up the pool and shrink it down
    if (!d_pools.empty()) {
        const auto last = std::prev(d_pools.end());
        const auto [last_ptr, last_size] = *last;
        if (last_ptr + last_size == d_memory->size()) {
            // We already know this pool is too small (since we would have used it in the
            // above code) so size - last_size is definitely positive.
            for (std::size_t i = 0; i != size - last_size; ++i) {
                d_memory->emplace_back();
            }
            d_pools.erase(last);
            return last_ptr;
        }
    }

    // Otherwise, append the end of vector.
    const auto ptr = d_memory->size();
    for (std::size_t i = 0; i != size; ++i) {
        d_memory->emplace_back();
    }
    return ptr;
}

auto memory_allocator::deallocate(std::size_t ptr, std::size_t size) -> void
{
    d_bytes_allocated -= size;
    
    auto [it, success] = d_pools.emplace(ptr, size);
    if (!success) {
        print("logic error, double deallocation of ptr={}\n", ptr);
        std::exit(1);
    }

    if (it != d_pools.begin()) { // Try to merge into the one before
        it = try_merge_left(d_pools, std::prev(it), it);
    }
    if (std::next(it) != d_pools.end()) {
        try_merge_left(d_pools, it, std::next(it));
    }
}

auto memory_allocator::bytes_allocated() const -> std::size_t
{
    return d_bytes_allocated;
}

}