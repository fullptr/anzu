cmake_minimum_required(VERSION 3.13)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS}")
set(CMAKE_STATIC_LINKER_FLAGS "${CMAKE_STATIC_LINKER_FLAGS}")
set(CMAKE_COMPILE_WARNING_AS_ERROR ON)

add_executable(
    anzu
    anzu.m.cpp
    lexer.cpp
    token.cpp
    parser.cpp
    ast.cpp
    compiler.cpp
    object.cpp
    functions.cpp
    bytecode.cpp
    runtime.cpp
    names.cpp

    compilation/type_manager.cpp
    compilation/variable_manager.cpp
)

target_include_directories(anzu PRIVATE .)