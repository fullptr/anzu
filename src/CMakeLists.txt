cmake_minimum_required(VERSION 3.13)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS}")
set(CMAKE_STATIC_LINKER_FLAGS "${CMAKE_STATIC_LINKER_FLAGS}")

find_package(fmt CONFIG REQUIRED)

add_executable(anzu
               anzu.m.cpp
               stack_frame.cpp
               op_codes.cpp
               parser.cpp
               object.cpp)

target_link_libraries(anzu PRIVATE fmt::fmt)
target_include_directories(anzu PRIVATE .)