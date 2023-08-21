/*
 * copy
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#include "../parallel_RA_inc.h"

void parallel_copy_filter::local_copy_filter(u32 buckets, shmap_relation* input, u32* input_bucket_map, relation* output, std::vector<int> reorder_map, u32 arity, u32 join_column_count, all_to_allv_buffer& copy_filter_buffer, int ra_counter)
{
    u32* output_sub_bucket_count = output->get_sub_bucket_per_bucket_count();
    u32** output_sub_bucket_rank = output->get_sub_bucket_rank();


    copy_filter_buffer.width[ra_counter] = reorder_map.size();
    assert(copy_filter_buffer.width[ra_counter] == (int)output->get_arity());


    for (u32 i = 0; i < buckets; i++)
        if (input_bucket_map[i] == 1) {
            if (input[i].size() == 0) {
                continue;
            }
            bool canonical = output->get_is_canonical();
            int head_rel_hash_col_count = output->get_join_column_count();
            for (const shmap_relation::t_tuple &cur_path : input[i])
            {
                u64 reordered_cur_path[copy_filter_buffer.width[ra_counter]];
                u64 cur_path_array[cur_path.size()];
                cur_path_array[0] = cur_path[0];
                cur_path_array[1] = cur_path[1];
                if (lambda(cur_path_array) == true)
                {
                    for (u32 j =0; j < reorder_map.size(); j++)
                        reordered_cur_path[j] = cur_path[reorder_map[j]];

                    uint64_t bucket_id = tuple_hash(reordered_cur_path, head_rel_hash_col_count) % buckets;
                    uint64_t sub_bucket_id=0;
                    if (canonical == false && arity != 0 && arity >= head_rel_hash_col_count)
                        sub_bucket_id = tuple_hash(reordered_cur_path + head_rel_hash_col_count, arity-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

                    int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];
                    copy_filter_buffer.local_compute_output_size_rel[ra_counter] = copy_filter_buffer.local_compute_output_size_rel[ra_counter] + copy_filter_buffer.width[ra_counter];
                    copy_filter_buffer.local_compute_output_size_total = copy_filter_buffer.local_compute_output_size_total + copy_filter_buffer.width[ra_counter];
                    copy_filter_buffer.local_compute_output_size_flat[index * copy_filter_buffer.ra_count + ra_counter] = copy_filter_buffer.local_compute_output_size_flat[index * copy_filter_buffer.ra_count + ra_counter] + copy_filter_buffer.width[ra_counter];
                    copy_filter_buffer.local_compute_output_count_flat[index * copy_filter_buffer.ra_count + ra_counter] ++;

                    copy_filter_buffer.local_compute_output_size[ra_counter][index] = copy_filter_buffer.local_compute_output_size[ra_counter][index] + copy_filter_buffer.width[ra_counter];
                    copy_filter_buffer.cumulative_tuple_process_map[index] = copy_filter_buffer.cumulative_tuple_process_map[index] + copy_filter_buffer.width[ra_counter];
                    copy_filter_buffer.local_compute_output[ra_counter][index].vector_buffer_append((const unsigned char*)reordered_cur_path, sizeof(u64)*copy_filter_buffer.width[ra_counter]);
                }
            }
        }
}