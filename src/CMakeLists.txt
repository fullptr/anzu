cmake_minimum_required(VERSION 3.13)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS}")
set(CMAKE_STATIC_LINKER_FLAGS "${CMAKE_STATIC_LINKER_FLAGS}")

add_executable(anzu
               anzu.m.cpp
               ast.cpp
               lexer.cpp
               parser.cpp
               compiler.cpp
               stack_frame.cpp
               op_codes.cpp
               object.cpp
               functions.cpp
               vocabulary.cpp)

target_include_directories(anzu PRIVATE .)