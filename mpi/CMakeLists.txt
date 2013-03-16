cmake_minimum_required (VERSION 2.6)
project(mpibridge)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/../cmake")

add_executable(mpibridge mpibridge.cpp)

find_package(MPI REQUIRED)
include_directories(${MPI_INCLUDE_PATH})

find_package(Boost COMPONENTS system filesystem REQUIRED)
include_directories(${Boost_INCLUDE_DIR})

find_package(ZMQ)
include_directories(${ZMQ_INCLUDE_DIRS})

include_directories(../adaptors/cpp/include/)

set(CMAKE_CXX_COMPILE_FLAGS ${CMAKE_CXX_COMPILE_FLAGS} ${MPI_COMPILE_FLAGS})
set(CMAKE_CXX_LINK_FLAGS ${CMAKE_CXX_LINK_FLAGS} ${MPI_LINK_FLAGS})

target_link_libraries(mpibridge ${MPI_LIBRARIES} ${Boost_LIBRARIES} ${ZMQ_LIBRARIES})

install(TARGETS mpibridge DESTINATION /usr/local/bin COMPONENT mpibridge)
