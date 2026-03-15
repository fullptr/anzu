#include "debugger.hpp"
#include "runtime.hpp"
#include "bytecode.hpp"

#include <algorithm>
#include <fstream>
#include <iostream>
#include <print>
#include <string>
#include <unordered_set>
#include <vector>

namespace anzu {
namespace {

auto load_source_lines(const std::filesystem::path& path) -> std::vector<std::string>
{
    std::vector<std::string> lines;
    auto file = std::ifstream{path};
    for (std::string line; std::getline(file, line); ) {
        lines.push_back(line);
    }
    return lines;
}

// Returns the source line for a given bytecode offset in a function, or 0 if unknown.
auto get_current_line(const bytecode_function& func, std::size_t offset) -> std::size_t
{
    if (func.source_map.empty()) return 0;
    // source_map is sorted by offset; find the last entry with offset <= current
    auto it = std::upper_bound(
        func.source_map.begin(), func.source_map.end(), offset,
        [](std::size_t val, const source_location& sl) { return val < sl.offset; }
    );
    if (it == func.source_map.begin()) return 0;
    --it;
    return it->line;
}

auto print_source_context(const std::vector<std::string>& lines, std::size_t line, int ctx_size = 2) -> void
{
    if (line == 0 || lines.empty()) return;
    const auto first = line > static_cast<std::size_t>(ctx_size) ? line - ctx_size : std::size_t{1};
    const auto last  = std::min(line + static_cast<std::size_t>(ctx_size), lines.size());
    for (auto i = first; i <= last; ++i) {
        std::print("{} {:4} | {}\n", i == line ? ">" : " ", i, lines[i - 1]);
    }
}

auto print_variables(bytecode_context& ctx, const bytecode_function& func, std::size_t offset) -> void
{
    const auto& frame = ctx.frames.back();
    bool any = false;
    for (const auto& var : func.dbg_vars) {
        if (var.name.starts_with('$')) continue; // skip compiler-generated vars
        if (var.live_from > offset) continue;    // not yet declared

        // Check the variable's stack space is still allocated
        const auto base    = var.is_global ? std::size_t{0} : frame.base_ptr;
        const auto abs_end = base + var.location + var.size;
        if (abs_end > ctx.stack.size()) continue; // popped out of scope

        any = true;
        std::print("  {:20} : {:12} = ", var.name, var.type_str);
        const auto addr = base + var.location;

        if      (var.type_str == "i32"  && var.size == 4) { std::int32_t  v; std::memcpy(&v, &ctx.stack.at(addr), 4); std::print("{}\n", v); }
        else if (var.type_str == "i64"  && var.size == 8) { std::int64_t  v; std::memcpy(&v, &ctx.stack.at(addr), 8); std::print("{}\n", v); }
        else if (var.type_str == "u64"  && var.size == 8) { std::uint64_t v; std::memcpy(&v, &ctx.stack.at(addr), 8); std::print("{}\n", v); }
        else if (var.type_str == "f64"  && var.size == 8) { double        v; std::memcpy(&v, &ctx.stack.at(addr), 8); std::print("{}\n", v); }
        else if (var.type_str == "bool" && var.size == 1) { bool          v; std::memcpy(&v, &ctx.stack.at(addr), 1); std::print("{}\n", v ? "true" : "false"); }
        else if (var.type_str == "char" && var.size == 1) { char          v; std::memcpy(&v, &ctx.stack.at(addr), 1); std::print("'{}'\n", v); }
        else { std::print("<{} bytes>\n", var.size); }
    }
    if (!any) std::print("  (no locals in scope)\n");
}

auto print_backtrace(bytecode_context& ctx) -> void
{
    for (int i = static_cast<int>(ctx.frames.size()) - 1; i >= 0; --i) {
        const auto& frame = ctx.frames[i];
        const auto& func  = ctx.functions[frame.function_id];
        const auto  off   = static_cast<std::size_t>(frame.ip - frame.code);
        const auto  line  = get_current_line(func, off);
        std::print("  #{} {} (line {})\n", ctx.frames.size() - 1 - i, func.name, line);
    }
}

auto print_debugger_help() -> void
{
    std::print("Debugger commands:\n");
    std::print("  s / step          - step to next source line\n");
    std::print("  c / continue      - continue until next breakpoint\n");
    std::print("  b <n> / break <n> - add breakpoint at line n\n");
    std::print("  d <n> / del <n>   - remove breakpoint at line n\n");
    std::print("  bl / breakpoints  - list all breakpoints\n");
    std::print("  v / vars          - show current local variables\n");
    std::print("  bt / backtrace    - show call stack\n");
    std::print("  l / list          - show current source context\n");
    std::print("  q / quit          - exit the program\n");
    std::print("  h / help          - show this help\n");
}

} // anonymous namespace

auto run_program_interactive(const bytecode_program& prog, const std::filesystem::path& source_file) -> void
{
    const auto source_lines = load_source_lines(source_file);

    std::unordered_set<std::size_t> breakpoints;
    bool        stepping  = true; // start paused at first source line
    std::size_t last_line = 0;

    auto hook = [&](bytecode_context& ctx) {
        const auto& frame  = ctx.frames.back();
        const auto& func   = ctx.functions[frame.function_id];
        const auto  offset = static_cast<std::size_t>(frame.ip - frame.code);
        const auto  line   = get_current_line(func, offset);

        if (line == 0) return; // no source info for this instruction

        const bool at_breakpoint = breakpoints.count(line) > 0;
        const bool new_line      = stepping && line != last_line;
        if (!at_breakpoint && !new_line) return;

        last_line = line;
        stepping  = true; // hitting a breakpoint re-enables step mode

        std::print("\n=== {} : line {} ===\n", func.name, line);
        print_source_context(source_lines, line);
        std::print("\n");

        // Interactive REPL
        while (true) {
            std::print("(dbg) ");
            std::string cmd;
            if (!std::getline(std::cin, cmd)) { std::exit(0); }

            if (cmd.empty() || cmd == "s" || cmd == "step") {
                stepping = true;
                break;
            }
            else if (cmd == "c" || cmd == "continue") {
                stepping = false;
                break;
            }
            else if (cmd.starts_with("b ") || cmd.starts_with("break ")) {
                const auto n  = cmd.substr(cmd.find(' ') + 1);
                const auto bp = std::stoul(n);
                breakpoints.insert(bp);
                std::print("Breakpoint set at line {}\n", bp);
            }
            else if (cmd.starts_with("d ") || cmd.starts_with("del ")) {
                const auto n  = cmd.substr(cmd.find(' ') + 1);
                const auto bp = std::stoul(n);
                breakpoints.erase(bp);
                std::print("Breakpoint at line {} removed\n", bp);
            }
            else if (cmd == "bl" || cmd == "breakpoints") {
                if (breakpoints.empty()) { std::print("  (no breakpoints)\n"); }
                else { for (auto bp : breakpoints) std::print("  line {}\n", bp); }
            }
            else if (cmd == "v" || cmd == "vars") {
                print_variables(ctx, func, offset);
            }
            else if (cmd == "bt" || cmd == "backtrace") {
                print_backtrace(ctx);
            }
            else if (cmd == "l" || cmd == "list") {
                print_source_context(source_lines, line, 5);
            }
            else if (cmd == "q" || cmd == "quit") {
                std::exit(0);
            }
            else if (cmd == "h" || cmd == "help") {
                print_debugger_help();
            }
            else {
                std::print("Unknown command '{}'. Type 'h' for help.\n", cmd);
            }
        }
    };

    run_program_with_hook(prog, hook);
}

}
