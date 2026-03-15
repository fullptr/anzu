#pragma once
#include "bytecode.hpp"
#include <filesystem>

namespace anzu {

auto run_program_interactive(const bytecode_program& prog, const std::filesystem::path& source_file) -> void;

}
