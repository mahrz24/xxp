cmake_minimum_required (VERSION 2.6)
project(xxpexample)

# Required to use xxp and link against the zeromq library
set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/../../core/cmake")

find_package(Boost)
include_directories(${Boost_INCLUDE_DIR})

find_package(ZMQ)
include_directories(${ZMQ_INCLUDE_DIRS})

include_directories("../../core/adaptors/cpp/include")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -DXXP_DEBUG")

# Actual configuration starts here
add_executable(test1 main.cpp)
target_link_libraries(test1 ${ZMQ_LIBRARIES})

add_executable(gen main_gen.cpp)
target_link_libraries(gen ${ZMQ_LIBRARIES})