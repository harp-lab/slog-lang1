cmake_minimum_required (VERSION 2.6)

project (target)

add_compile_options(--std=c++17 -Wno-strict-aliasing)

find_package(MPI REQUIRED)
find_package(OpenMP)
if (OPENMP_FOUND)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
    set (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_EXE_LINKER_FLAGS}")
endif()

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -std=c++17")
set (source_dir "${PROJECT_SOURCE_DIR}")
set (ra_source_dir "../../parallel-RA/src")

ADD_LIBRARY(parallel_RA "../../parallel-RA/build")

file (GLOB source_files "*.cpp")

add_executable(target ${source_files})
INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})
INCLUDE_DIRECTORIES("/Users/krismicinski/projects/parallel-RA/src")
TARGET_LINK_LIBRARIES(target ${MPI_LIBRARIES} parallel_RA)
