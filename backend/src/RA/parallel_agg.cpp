
#include "../parallel_RA_inc.h"
#include <iostream>
#include <ostream>

bool parallel_join_negate::local_negation(
    int threshold, int* offset, int join_order, u32 buckets,
    int input0_buffer_size,
    int input0_buffer_width, u64 *input0_buffer,
    shmap_relation *input1,
    u32 i1_size, int input1_buffer_width,
    std::vector<int> &reorder_map_array,
    relation* output, all_to_allv_buffer& join_buffer,
    int counter, int join_column_count)
{
    join_buffer.width[counter] = reorder_map_array.size();

    // shmap_relation* negated_target = NULL;
    u32* output_sub_bucket_count = output->get_sub_bucket_per_bucket_count();
    u32** output_sub_bucket_rank = output->get_sub_bucket_rank();
 
    if (*offset > input0_buffer_size || i1_size == 0)
    {
        // std::cout << "buffer size invalid ... " << input0_buffer_size << "  " << i1_size << std::endl;
        return true;
    }
    if (join_order == RIGHT)
    {
        for (u32 bucket_id = 0; bucket_id < buckets; bucket_id++)
        {
            input1[bucket_id].as_all_to_allv_right_outer_join_buffer(
                // negated_target,
                input0_buffer, input0_buffer_size, input0_buffer_width, offset,
                join_buffer,
                counter, buckets, output_sub_bucket_count,
                output_sub_bucket_rank, reorder_map_array, join_column_count,
                output->get_arity(),
                output->get_join_column_count(), output->get_is_canonical());
        }
    }
    return true;
}

void parallel_join_negate::local_copy(
    u32 buckets, shmap_relation *input,
    u32 *input_bucket_map, relation *output,
    std::vector<int> reorder_map, u32 arity,
    u32 join_column_count, all_to_allv_buffer &copy_buffer,
    int ra_counter)
{
    u32* output_sub_bucket_count = output->get_sub_bucket_per_bucket_count();
    u32** output_sub_bucket_rank = output->get_sub_bucket_rank();

    copy_buffer.width[ra_counter] = reorder_map.size();
    assert(copy_buffer.width[ra_counter] == (int)output->get_arity());

    for (u32 i = 0; i < buckets; i++)
    {
        if (input_bucket_map[i] == 1)
        {
            input[i].as_all_to_allv_copy_buffer(copy_buffer, {}, reorder_map, ra_counter, buckets, output_sub_bucket_count, output_sub_bucket_rank, arity, join_column_count, output->get_join_column_count(), output->get_is_canonical());
        }
    }
}
