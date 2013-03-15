cmake_minimum_required (VERSION 2.6)
project(xxpexample)

FIND_PACKAGE(Boost COMPONENTS system REQUIRED)
IF (Boost_FOUND)
    INCLUDE_DIRECTORIES(${Boost_INCLUDE_DIR})
ENDIF()

include_directories("../../adaptors/cpp/include")

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -DXXP_DEBUG")

add_executable(test1 main.cpp)
target_link_libraries(test1 ${Boost_LIBRARIES} zmq)