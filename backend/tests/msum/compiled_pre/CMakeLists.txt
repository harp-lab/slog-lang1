cmake_minimum_required (VERSION 3.9)

project (msum)

add_compile_options(--std=c++17 -lstdc++fs -Wno-strict-aliasing -Werror=class-memaccess -fpermissive)

link_libraries(stdc++fs)

find_package(MPI REQUIRED)
# find_package(OpenMP)
# if (OPENMP_FOUND)
#     set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
#     set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
#     set (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_EXE_LINKER_FLAGS}")
# endif()

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -std=c++17 -lstdc++fs -Werror=class-memaccess -fpermissive")
# set (base_dir "${PROJECT_SOURCE_DIR}/../backend")
set (source_dir "${PROJECT_SOURCE_DIR}/../../../src")

file (GLOB source_files_parallel_RA "${source_dir}/parallel_RA_inc.h" "${source_dir}/log/logger.cpp" "${source_dir}/hash/hash.h" "${source_dir}/buffer/vector_buffer.cpp" "${source_dir}/comm/comm.cpp" "${source_dir}/relation/shmap_relation_exp.cpp" "${source_dir}/comm/all_to_all_comm.cpp" "${source_dir}/comm/all_to_allv_comm.cpp" "${source_dir}/IO/parallel_io.cpp" "${source_dir}/RA/parallel_join.cpp" "${source_dir}/RA/parallel_agg.cpp" "${source_dir}/comm/intra_bucket_comm.cpp" "${source_dir}/RA/parallel_copy.cpp" "${source_dir}/RA/parallel_copy_filter.cpp" "${source_dir}/RA/parallel_copy_generate.cpp" "${source_dir}/RA/parallel_RA.h" "${source_dir}/RA/parallel_acopy.cpp" "${source_dir}/relation/balanced_hash_relation.cpp" "${source_dir}/relation/relation_load_balancer.cpp" "${source_dir}/RAM/RA_tasks.cpp" "${source_dir}/lie/lie.cpp")
file (GLOB source_files_msum "${PROJECT_SOURCE_DIR}/msum.cpp")

ADD_LIBRARY(parallel_RA "${source_files_parallel_RA}")

add_executable(msum ${source_files_msum})
INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})
TARGET_LINK_LIBRARIES(msum parallel_RA ${MPI_LIBRARIES})
