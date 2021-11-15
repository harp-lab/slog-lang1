cmake_minimum_required (VERSION 3.9)

project (BTree)

add_compile_options(--std=c++17 -Wno-strict-aliasing -Werror=class-memaccess -fpermissive)

find_package(MPI REQUIRED)
find_package(OpenMP)
if (OPENMP_FOUND)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
    set (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_EXE_LINKER_FLAGS}")
endif()

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -std=c++17 -Werror=class-memaccess -fpermissive")
set (source_dir "${PROJECT_SOURCE_DIR}/src")
set (tests_dir "${PROJECT_SOURCE_DIR}/tests")
set (data_dir "${PROJECT_SOURCE_DIR}/data")
set (utility_dir "${PROJECT_SOURCE_DIR}/utility")

file (GLOB source_files_parallel_RA "${source_dir}/parallel_RA_inc.h" "${source_dir}/log/logger.cpp" "${source_dir}/hash/hash.h" "${source_dir}/buffer/vector_buffer.cpp" "${source_dir}/comm/comm.cpp" "${source_dir}/relation/google_btree_relation.cpp" "${source_dir}/relation/shmap_relation_exp.cpp" "${source_dir}/comm/all_to_all_comm.cpp" "${source_dir}/comm/all_to_allv_comm.cpp" "${source_dir}/IO/parallel_io.cpp" "${source_dir}/RA/parallel_join.cpp" "${source_dir}/RA/parallel_agg.cpp" "${source_dir}/comm/intra_bucket_comm.cpp" "${source_dir}/RA/parallel_copy.cpp" "${source_dir}/RA/parallel_copy_filter.cpp" "${source_dir}/RA/parallel_copy_generate.cpp" "${source_dir}/RA/parallel_RA.h" "${source_dir}/RA/parallel_acopy.cpp" "${source_dir}/relation/balanced_hash_relation.cpp" "${source_dir}/relation/relation_load_balancer.cpp" "${source_dir}/RAM/RA_tasks.cpp" "${source_dir}/lie/lie.cpp")
file (GLOB source_files_ata "${tests_dir}/all_to_all_benchmark.cpp")
file (GLOB source_files_tc "${tests_dir}/transitive_closure.cpp")
#file (GLOB source_files_builtin "${tests_dir}/builtin.cpp")
#file (GLOB source_files_prim_32_3 "${tests_dir}/prim-32-3.cpp")
file (GLOB source_files_fact "${tests_dir}/fact.cpp")
file (GLOB source_files_out "${tests_dir}/out.cpp")
file (GLOB source_files_wc110_4 "${tests_dir}/worstcase-110-terms-4-m.cpp")
file (GLOB source_files_wc110_5 "${tests_dir}/worstcase-110-terms-5-m.cpp")
file (GLOB source_files_wc110_6 "${tests_dir}/worstcase-110-terms-6-m.cpp")
file (GLOB source_files_wc110_7 "${tests_dir}/worstcase-110-terms-7-m.cpp")
file (GLOB source_files_wc110_8 "${tests_dir}/worstcase-110-terms-8-m.cpp")
file (GLOB source_files_wc110_9 "${tests_dir}/worstcase-110-terms-9-m.cpp")
file (GLOB source_files_wc110_10 "${tests_dir}/worstcase-110-terms-10-m.cpp")
file (GLOB source_files_fv "${tests_dir}/freevars.cpp")
file (GLOB source_files_kcfa "${tests_dir}/kcfa-tiny.cpp")

ADD_LIBRARY(parallel_RA "${source_files_parallel_RA}")

add_executable(ata ${source_files_ata})
INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})
TARGET_LINK_LIBRARIES(ata parallel_RA ${MPI_LIBRARIES})

add_executable(wc1104 ${source_files_wc110_4})
INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})
TARGET_LINK_LIBRARIES(wc1104 parallel_RA ${MPI_LIBRARIES})

add_executable(wc1105 ${source_files_wc110_5})
INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})
TARGET_LINK_LIBRARIES(wc1105 parallel_RA ${MPI_LIBRARIES})

add_executable(wc1106 ${source_files_wc110_6})
INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})
TARGET_LINK_LIBRARIES(wc1106 parallel_RA ${MPI_LIBRARIES})

add_executable(wc1107 ${source_files_wc110_7})
INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})
TARGET_LINK_LIBRARIES(wc1107 parallel_RA ${MPI_LIBRARIES})

add_executable(wc1108 ${source_files_wc110_8})
INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})
TARGET_LINK_LIBRARIES(wc1108 parallel_RA ${MPI_LIBRARIES})

add_executable(wc1109 ${source_files_wc110_9})
INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})
TARGET_LINK_LIBRARIES(wc1109 parallel_RA ${MPI_LIBRARIES})

add_executable(wc11010 ${source_files_wc110_10})
INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})
TARGET_LINK_LIBRARIES(wc11010 parallel_RA ${MPI_LIBRARIES})

add_executable(fact ${source_files_fact})
INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})
TARGET_LINK_LIBRARIES(fact parallel_RA ${MPI_LIBRARIES})

# add_executable(out ${source_files_out})
# INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})
# TARGET_LINK_LIBRARIES(out parallel_RA ${MPI_LIBRARIES})

#add_executable(prim_32_3 ${source_files_prim_32_3})
#INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})
#TARGET_LINK_LIBRARIES(prim_32_3 parallel_RA ${MPI_LIBRARIES})

add_executable(TC ${source_files_tc})
INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})
TARGET_LINK_LIBRARIES(TC parallel_RA ${MPI_LIBRARIES})

#add_executable(builtin ${source_files_builtin})
#INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})
#TARGET_LINK_LIBRARIES(builtin parallel_RA ${MPI_LIBRARIES})

add_executable(freevars ${source_files_fv})
INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})
TARGET_LINK_LIBRARIES(freevars parallel_RA ${MPI_LIBRARIES})

add_executable(kcfa ${source_files_kcfa})
INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})
TARGET_LINK_LIBRARIES(kcfa parallel_RA ${MPI_LIBRARIES})

add_executable(bin_parser "${utility_dir}/binary_parser.cpp")

add_executable(tsv_to_bin "${utility_dir}/tsv_to_bin.cpp")
INCLUDE_DIRECTORIES(${MPI_INCLUDE_PATH})
TARGET_LINK_LIBRARIES(tsv_to_bin parallel_RA ${MPI_LIBRARIES})
