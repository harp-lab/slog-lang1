
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

    shmap_relation* negated_target = NULL;
    u32* output_sub_bucket_count = output->get_sub_bucket_per_bucket_count();
    u32** output_sub_bucket_rank = output->get_sub_bucket_rank();
 
    if (*offset > input0_buffer_size || i1_size == 0)
    {
        // std::cout << "buffer size invalid ... " << input0_buffer_size << "  " << i1_size << std::endl;
        return true;
    }
    if (join_order == RIGHT)
    {
        if (input0_buffer_size != 0)
        {
            negated_target = new shmap_relation;
            for (int k1 = *offset; k1 < input0_buffer_size; k1 = k1 + input0_buffer_width)
            {
                u64 prefix[join_column_count];
                // std::cout << "NEG PREFIX  ";
                for (int jc=0; jc < join_column_count; jc++)
                {
                    prefix[jc] = input0_buffer[k1 + jc];
                    // std::cout << input0_buffer[k1 + jc] << " ";
                }
                // std::cout << std::endl;
                negated_target->insert_tuple_from_array(prefix, join_column_count);
            }
        }
        else {
            // should fail here !!!
            std::cout << "shouldn't be here..." << std::endl;
            return false;
        }
        
        for (int k1 = *offset; k1 < input0_buffer_size; k1 = k1 + input0_buffer_width)
        {
            u64 bucket_id = tuple_hash(input0_buffer + k1, join_column_count) % buckets;
            input1[bucket_id].as_all_to_allv_right_outer_join_buffer(
                negated_target, join_buffer, input0_buffer + k1, input0_buffer_width,
                input1_buffer_width, counter, buckets, output_sub_bucket_count,
                output_sub_bucket_rank, reorder_map_array, join_column_count,
                output->get_arity(),
                output->get_join_column_count(), output->get_is_canonical());
        }
    }
    if (negated_target != NULL)
        delete negated_target;
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
