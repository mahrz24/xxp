cmake_minimum_required (VERSION 2.6)
project(xxpexample)

include_directories("../../adaptors/cpp/include")

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
add_definitions(-DXXP_WITH_CPP11)

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -DXXP_DEBUG")

add_executable(test1 main.cpp)