/*
 * join
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#include "../parallel_RA_inc.h"
#include "mpi.h"
#include <cstddef>
#include <iostream>
#include <vector>


bool parallel_join::local_join(int threshold, int* offset,
                               int join_order,
                               u32 buckets,
                               shmap_relation *input0,
                               int input0_buffer_size, int input0_buffer_width, u64 *input0_buffer,
                               shmap_relation *input1, u32 i1_size, int input1_buffer_width,
                               std::vector<int> reorder_map_array,
                               relation* output,
                               relation* input0_rel,
                               relation* input1_rel,
                               all_to_allv_buffer& join_buffer,
                               int counter,
                               int join_column_count,
                               u32* global_join_duplicates,
                               u32* global_join_inserts,
                               std::vector<double>& time_stat)
{
    join_buffer.width[counter] = reorder_map_array.size();

    shmap_relation deduplicate(join_column_count, false);
    auto out_dep_cols = output->get_dependent_column();
    if (out_dep_cols.size() != 0) {
        for (size_t i = 0; i < out_dep_cols.size() - 1; i++) {
            deduplicate.dependent_column_indices.push_back(out_dep_cols[i]);
        }
        deduplicate.update_compare_func = output->get_update_compare_func();
    }
    u32* output_sub_bucket_count = output->get_sub_bucket_per_bucket_count();
    u32** output_sub_bucket_rank = output->get_sub_bucket_rank();
    // std::cout << "wwwwwwwww " << input0_buffer_size << " " << input0_buffer_size << " " << i1_size << std::endl;

    if (*offset > input0_buffer_size || input0_buffer_size == 0 || i1_size == 0) {
        time_stat.push_back(0);
        return true;
    }

    double join_time_total = 0;
    int local_join_count=0;
    if (join_order == LEFT)
    {
        for (int k1 = *offset; k1 < input0_buffer_size; k1 = k1 + input0_buffer_width)
        {
            std::vector<u64> prefix;
            for (int jc=0; jc < join_column_count; jc++)
            {
                prefix.push_back(input0_buffer[k1 + jc]);
                //std::cout << "PREFIX " << input0_buffer[k1 + jc] << std::endl;
            }

            // u64 bucket_id = tuple_hash(input0_buffer + k1, join_column_count) % buckets;
            
            auto before_actual_join = MPI_Wtime();
            for (u32 bucket_id = 0; bucket_id < buckets; bucket_id++) {
                input1[bucket_id].as_all_to_allv_left_join_buffer(
                    prefix, join_buffer,
                    input0_buffer + k1,input0_buffer_width,
                    input1_buffer_width, counter,
                    buckets, output_sub_bucket_count,
                    output_sub_bucket_rank, reorder_map_array,
                    join_column_count, deduplicate,
                    &local_join_count, global_join_duplicates,
                    global_join_inserts, output->get_join_column_count(),
                    output->get_is_canonical(),
                    generator_mode, generator_func);
            }
            auto after_actual_join = MPI_Wtime();
            join_time_total += after_actual_join - before_actual_join;

            // std::cout << "local_join_count " << local_join_count << " Threshold " << threshold << " k1 " << k1 << " offset " << *offset << " " << input0_buffer_width << std::endl;
            if (local_join_count > threshold)
            {
                *offset = k1 + input0_buffer_width;
                deduplicate.remove_tuple();
                return false;
            }
        }
    }

    else if (join_order == RIGHT)
    {
        if (input0->dependent_column_indices.size() > 0 && generator_mode) {
            // right lattice join
            std::vector<std::vector<u64>> input_ts;
            std::vector<u64> prev_non_dependent_columns;
            for (int k1 = *offset; k1 < input0_buffer_size; k1 = k1 + input0_buffer_width) {
                std::vector<u64> cur_non_dependent_columns(
                    input0_buffer+k1,
                    input0_buffer+k1+input0_buffer_width-input0->dependent_column_indices.size()
                );
                // std::vector<u64> prefix;
                // for (int jc=0; jc < join_column_count; jc++)
                //     prefix.push_back(input0_buffer[k1 + jc]);
                
                std::vector<u64> input_t(input0_buffer+k1, input0_buffer+k1+input0_buffer_width);
                // std::cout << "LT >>> ";
                // for (auto c: input_t) {
                //     std::cout << c << " ";
                // }
                // std::cout << std::endl;
                if (cur_non_dependent_columns == prev_non_dependent_columns) {
                    input_ts.push_back(input_t);
                } else {
                    if (input_ts.size() != 0) {
                        auto before_actual_join = MPI_Wtime();
                        // u64 bucket_id = tuple_hash(input0_buffer + k1, join_column_count) % buckets;
                        for (u32 bucket_id = 0; bucket_id < buckets; bucket_id++) {
                            input1[bucket_id].as_all_to_allv_right_join_buffer(
                                std::vector<u64>(prev_non_dependent_columns.begin(),
                                                prev_non_dependent_columns.begin()+join_column_count),
                                join_buffer,
                                input_ts,
                                input1_buffer_width, counter,
                                buckets, output_sub_bucket_count,
                                output_sub_bucket_rank, reorder_map_array,
                                join_column_count, deduplicate,
                                &local_join_count, global_join_duplicates,
                                global_join_inserts,
                                output->get_join_column_count(),output->get_is_canonical(),
                                generator_mode, generator_func);
                        }
                        auto after_actual_join = MPI_Wtime();
                        join_time_total += after_actual_join - before_actual_join;
                        input_ts.clear();
                    }
                    prev_non_dependent_columns = cur_non_dependent_columns;
                    input_ts.push_back(input_t);
                }
            }
            if (input_ts.size() != 0) {
                // u64 bucket_id = tuple_hash(prev_non_dependent_columns.data(), join_column_count) % buckets;
                auto before_actual_join = MPI_Wtime();
                for (u32 bucket_id = 0; bucket_id < buckets; bucket_id++) {
                    input1[bucket_id].as_all_to_allv_right_join_buffer(
                        std::vector<u64>(prev_non_dependent_columns.begin(),
                                        prev_non_dependent_columns.begin()+join_column_count),
                        join_buffer,
                        input_ts,
                        input1_buffer_width, counter,
                        buckets, output_sub_bucket_count,
                        output_sub_bucket_rank, reorder_map_array,
                        join_column_count, deduplicate,
                        &local_join_count, global_join_duplicates,
                        global_join_inserts,
                        output->get_join_column_count(),output->get_is_canonical(),
                        generator_mode, generator_func);
                }
                auto after_actual_join = MPI_Wtime();
                join_time_total += after_actual_join - before_actual_join;
                input_ts.clear();
            }
        } else {
        // original code    
        for (int k1 = *offset; k1 < input0_buffer_size; k1 = k1 + input0_buffer_width)
        {
            std::vector<u64> prefix;
            for (int jc=0; jc < join_column_count; jc++)
                prefix.push_back(input0_buffer[k1 + jc]);

            // u64 bucket_id = tuple_hash(input0_buffer + k1, join_column_count) % buckets;
            std::vector<std::vector<u64>> input_ts;
            input_ts.push_back(std::vector<u64>(input0_buffer+k1, input0_buffer+k1+input0_buffer_width));
            auto before_actual_join = MPI_Wtime();
            for (u32 bucket_id = 0; bucket_id < buckets; bucket_id++) {
                input1[bucket_id].as_all_to_allv_right_join_buffer(
                    prefix, join_buffer,
                    // input0_buffer + k1, input0_buffer_width,
                    input_ts,
                    input1_buffer_width, counter,
                    buckets, output_sub_bucket_count,
                    output_sub_bucket_rank, reorder_map_array,
                    join_column_count, deduplicate,
                    &local_join_count, global_join_duplicates,
                    global_join_inserts,
                    output->get_join_column_count(),output->get_is_canonical(),
                    generator_mode, generator_func);
            }
            auto after_actual_join = MPI_Wtime();
            join_time_total += after_actual_join - before_actual_join;

            // std::cout << "local_join_count " << local_join_count << " Threshold " << threshold << " k1 " << k1 << " offset " << *offset << " " << input0_buffer_width << std::endl;
            if (local_join_count > threshold)
            {
                *offset = k1 + input0_buffer_width;
                //std::cout << "Setting offset " << *offset << std::endl;
                deduplicate.remove_tuple();
                return false;
            }
        }

        }
    }

    time_stat.push_back(join_time_total);
    deduplicate.remove_tuple();
    return true;
}
