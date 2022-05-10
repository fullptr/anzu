cmake_minimum_required(VERSION 3.13)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS}")
set(CMAKE_STATIC_LINKER_FLAGS "${CMAKE_STATIC_LINKER_FLAGS}")

add_executable(
    anzu
    anzu.m.cpp
    lexer.cpp
    token.cpp
    parser.cpp
    ast.cpp
    compiler.cpp
    program.cpp
    runtime.cpp
    memory.cpp
    object.cpp
    functions.cpp
    operators.cpp
    vocabulary.cpp
)

target_include_directories(anzu PRIVATE .)