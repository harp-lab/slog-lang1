cmake_minimum_required (VERSION 2.6)

project (target)

add_compile_options(--std=c++17 -Wno-strict-aliasing -fpermissive)

if(APPLE)
    set(CMAKE_C_COMPILER clang)
    set(CMAKE_CXX_COMPILER clang++)
    if(CMAKE_C_COMPILER_ID MATCHES "Clang\$")
        set(OpenMP_C_FLAGS "-Xpreprocessor -fopenmp")
        set(OpenMP_C_LIB_NAMES "omp")
        set(OpenMP_omp_LIBRARY omp)
    endif()
    if(CMAKE_CXX_COMPILER_ID MATCHES "Clang\$")
        set(OpenMP_CXX_FLAGS "-Xpreprocessor -fopenmp")
        set(OpenMP_CXX_LIB_NAMES "omp")
        set(OpenMP_omp_LIBRARY omp)
    endif()
endif()


find_package(MPI REQUIRED)
find_package(OpenMP)
if (OPENMP_FOUND)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
    set (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_EXE_LINKER_FLAGS}")
endif()

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -std=c++17")
set (source_dir "${PROJECT_SOURCE_DIR}")
set (ra_source_dir "../../../backend/src")

file (GLOB source_files "*.cpp")

add_executable(target ${source_files})
INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})
TARGET_INCLUDE_DIRECTORIES(target PUBLIC "../../../backend/src")
TARGET_LINK_DIRECTORIES(target PUBLIC "../../../backend/build")
TARGET_LINK_LIBRARIES(target parallel_RA ${MPI_LIBRARIES})
