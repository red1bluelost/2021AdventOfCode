cmake_minimum_required(VERSION 3.22)
project(Cpp)

set(CMAKE_CXX_STANDARD 20)

add_compile_options(-Wall -Wextra -Wpedantic -O3 -g0)

add_executable(Cpp main.cpp)
