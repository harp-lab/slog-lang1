/*
 * join
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#include "../parallel_RA_inc.h"


bool parallel_join::local_join(int threshold, int* offset,
                               int join_order,
                               u32 buckets,
                               int input0_buffer_size, int input0_buffer_width, u64 *input0_buffer,
                               shmap_relation *input1, u32 i1_size, int input1_buffer_width,
                               std::vector<int> reorder_map_array,
                               relation* output,
                               all_to_allv_buffer& join_buffer,
                               int counter,
                               int join_column_count,
                               u32* global_join_duplicates,
                               u32* global_join_inserts)
{
    join_buffer.width[counter] = reorder_map_array.size();

    shmap_relation deduplicate(join_column_count, false);
    u32* output_sub_bucket_count = output->get_sub_bucket_per_bucket_count();
    u32** output_sub_bucket_rank = output->get_sub_bucket_rank();

    if (*offset > input0_buffer_size || input0_buffer_size == 0 || i1_size == 0)
        return true;

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

            u64 bucket_id = tuple_hash(input0_buffer + k1, join_column_count) % buckets;

            // for (auto& t: input1[bucket_id]) 
            // //for (const t_tuple &cur_path : input1[bucket_id])
            // {
            //     std::cout << "TupleXXXXXXX: " << t[0] << std::endl;
            // }
            if (input1[bucket_id].size() == 0) {
                continue;
            }
            // input1[bucket_id].as_all_to_allv_left_join_buffer(
            //     prefix, join_buffer,
            //     input0_buffer + k1,input0_buffer_width,
            //     input1_buffer_width, counter,
            //     buckets, output_sub_bucket_count,
            //     output_sub_bucket_rank, reorder_map_array,
            //     join_column_count, deduplicate,
            //     &local_join_count, global_join_duplicates,
            //     global_join_inserts, output->get_join_column_count(),
            //     output->get_is_canonical());
            bool canonical = output->get_is_canonical();
            int head_rel_hash_col_count = output->get_join_column_count();
            shmap_relation::t_tuple upper_bound(input1[bucket_id].arity+1, std::numeric_limits<u64>::max());
            shmap_relation::t_tuple lower_bound(input1[bucket_id].arity+1, std::numeric_limits<u64>::min());
            for(size_t i = 0; i < prefix.size(); i++)
            {
                upper_bound[i] = prefix[i];
                lower_bound[i] = prefix[i];
            }
            auto joined_range = input1[bucket_id].lowerUpperRange(lower_bound, upper_bound);
            for(auto it = joined_range.first; it != joined_range.second && it != input1[bucket_id].end(); ++it)
            {
                auto cur_path = *it;
                u64 projected_path[join_buffer.width[counter]];
                u64 reordered_cur_path[input0_buffer_width + input1_buffer_width - join_column_count];
                for (int i = 0; i < input0_buffer_width; i++)
                    reordered_cur_path[i] = (input0_buffer + k1)[i];

                for (int i = join_column_count; i < input1_buffer_width; i++)
                    reordered_cur_path[input0_buffer_width + (i - join_column_count)] = cur_path[i];

                for (int i =0; i < join_buffer.width[counter]; i++)
                    projected_path[i] = reordered_cur_path[reorder_map_array[i]];

                //std::cout << "NT " << projected_path[0] << " " << projected_path[1] << std::endl;
                if (deduplicate.insert_tuple_from_array(projected_path, join_buffer.width[counter]) == true)
                {
                    uint64_t bucket_id = tuple_hash(projected_path, head_rel_hash_col_count) % buckets;
                    uint64_t sub_bucket_id=0;
                    if (canonical == false)
                        sub_bucket_id = tuple_hash(projected_path + head_rel_hash_col_count, join_buffer.width[counter]-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

                    int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];

                    join_buffer.local_compute_output_size_rel[counter] = join_buffer.local_compute_output_size_rel[counter] + join_buffer.width[counter];
                    join_buffer.local_compute_output_size_total = join_buffer.local_compute_output_size_total + join_buffer.width[counter];
                    join_buffer.local_compute_output_size_flat[index*join_buffer.ra_count + counter] = join_buffer.local_compute_output_size_flat[index*join_buffer.ra_count + counter] + join_buffer.width[counter];
                    join_buffer.local_compute_output_count_flat[index * join_buffer.ra_count + counter] ++;

                    join_buffer.local_compute_output_size[counter][index] = join_buffer.local_compute_output_size[counter][index] + join_buffer.width[counter];
                    join_buffer.cumulative_tuple_process_map[index] = join_buffer.cumulative_tuple_process_map[index] + join_buffer.width[counter];
                    join_buffer.local_compute_output[counter][index].vector_buffer_append((const unsigned char*)projected_path, sizeof(u64)*join_buffer.width[counter]);
                    (*global_join_inserts)++;
                    local_join_count++;
                }
                else {
                    (*global_join_duplicates)++;
                }
            }

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
        for (int k1 = *offset; k1 < input0_buffer_size; k1 = k1 + input0_buffer_width)
        {
            std::vector<u64> prefix;
            for (int jc=0; jc < join_column_count; jc++)
                prefix.push_back(input0_buffer[k1 + jc]);

            u64 bucket_id = tuple_hash(input0_buffer + k1, join_column_count) % buckets;

            if (input1[bucket_id].size() == 0) {
                continue;
            }
            int arity = input1[bucket_id].arity;
            shmap_relation::t_tuple upper_bound(arity+1, std::numeric_limits<u64>::max());
            shmap_relation::t_tuple lower_bound(arity+1, std::numeric_limits<u64>::min());
            for(size_t i = 0; i < prefix.size(); i++)
            {
                upper_bound[i] = prefix[i];
                lower_bound[i] = prefix[i];
            }
            auto joined_range = input1[bucket_id].lowerUpperRange(lower_bound, upper_bound);
            bool canonical = output->get_is_canonical();
            int head_rel_hash_col_count = output->get_join_column_count();
            for(auto it = joined_range.first; it != joined_range.second && it != input1[bucket_id].end(); ++it)
            {
                auto cur_path = *it;
                u64 projected_path[join_buffer.width[counter]];
                u64 reordered_cur_path[input0_buffer_width + input1_buffer_width - join_column_count];
                for (int i = 0; i < input1_buffer_width; i++)
                    reordered_cur_path[i] = cur_path[i];

                for (int i = join_column_count; i < input0_buffer_width; i++)
                    reordered_cur_path[input1_buffer_width + (i - join_column_count)] = (input0_buffer+k1)[i];

                for (int i =0; i < join_buffer.width[counter]; i++)
                    projected_path[i] = reordered_cur_path[reorder_map_array[i]];

                if (deduplicate.insert_tuple_from_array(projected_path, join_buffer.width[counter]) == true)
                {
                    uint64_t bucket_id = tuple_hash(projected_path, head_rel_hash_col_count) % buckets;
                    uint64_t sub_bucket_id=0;
                    if (canonical == false)
                        sub_bucket_id = tuple_hash(projected_path + head_rel_hash_col_count, join_buffer.width[counter]-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

                    int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];

                    join_buffer.local_compute_output_size_rel[counter] = join_buffer.local_compute_output_size_rel[counter] + join_buffer.width[counter];
                    join_buffer.local_compute_output_size_total = join_buffer.local_compute_output_size_total + join_buffer.width[counter];
                    join_buffer.local_compute_output_size_flat[index*join_buffer.ra_count + counter] = join_buffer.local_compute_output_size_flat[index*join_buffer.ra_count + counter] + join_buffer.width[counter];
                    join_buffer.local_compute_output_count_flat[index * join_buffer.ra_count + counter] ++;
                    join_buffer.local_compute_output_size[counter][index] = join_buffer.local_compute_output_size[counter][index] + join_buffer.width[counter];

                    join_buffer.cumulative_tuple_process_map[index] = join_buffer.cumulative_tuple_process_map[index] + join_buffer.width[counter];
                    join_buffer.local_compute_output[counter][index].vector_buffer_append((const unsigned char*)projected_path, sizeof(u64)*join_buffer.width[counter]);
                    (*global_join_inserts)++;
                    local_join_count++;
                }
                else {
                    (*global_join_duplicates)++;
                }
            }

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

    deduplicate.remove_tuple();
    return true;
}
