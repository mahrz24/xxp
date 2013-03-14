cmake_minimum_required (VERSION 2.6)
project(mpibridge)

add_executable(mpibridge mpibridge.cpp)

find_package(MPI REQUIRED)
include_directories(${MPI_INCLUDE_PATH})

find_package(Boost COMPONENTS system REQUIRED)
include_directories(${Boost_INCLUDE_DIR})

set(CMAKE_CXX_COMPILE_FLAGS ${CMAKE_CXX_COMPILE_FLAGS} ${MPI_COMPILE_FLAGS})
set(CMAKE_CXX_LINK_FLAGS ${CMAKE_CXX_LINK_FLAGS} ${MPI_LINK_FLAGS})

target_link_libraries(mpibridge ${MPI_LIBRARIES} ${Boost_LIBRARIES}) 
