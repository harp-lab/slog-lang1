
#include "../parallel_RA_inc.h"

bool parallel_join_negate::local_negation(
    int threshold, int* offset, int join_order, u32 buckets,
    int input0_buffer_size,
    int input0_buffer_width, u64 *input0_buffer,
    shmap_relation *input0,
    shmap_relation *input1,
    u32 i1_size, int input1_buffer_width,
    std::vector<int> reorder_map_array,
    relation* output, all_to_allv_buffer& join_buffer,
    int counter, int join_column_count, u32* global_join_duplicates,
    u32* global_join_inserts)
{
    join_buffer.width[counter] = reorder_map_array.size();

    shmap_relation deduplicate;
    u32* output_sub_bucket_count = output->get_sub_bucket_per_bucket_count();
    u32** output_sub_bucket_rank = output->get_sub_bucket_rank();

    if (*offset > input0_buffer_size || input0_buffer_size == 0)
    {
        // std::cout << "buffer size invalid ... " << input0_buffer_size << "  " << i1_size << std::endl;
        return true;
    }
    // if (i1_size == 0)
    // {
    //     // if the target relation is empty set, just copy all input into output
    //     for (u32 i = 0; i < buckets; i++)
    //     {
            
    //     }
    // }
    int local_join_count=0;
    if (join_order == LEFT)
    {

        for (int k1 = *offset; k1 < input0_buffer_size; k1 = k1 + input0_buffer_width)
        {
            std::vector<u64> prefix;
            for (int jc=0; jc < join_column_count; jc++)
            {
                prefix.push_back(input0_buffer[k1 + jc]);
                // std::cout << "PREFIX " << input0_buffer[k1 + jc] << std::endl;
            }

            u64 bucket_id = tuple_hash(input0_buffer + k1, join_column_count) % buckets;

            // std::cout << "join buffer size before local join : "
            //           <<  join_buffer.width << std::endl;
            input1[bucket_id].as_all_to_allv_left_outer_join_buffer(
                input0,
                prefix, join_buffer, input0_buffer + k1, input0_buffer_width,
                input1_buffer_width, counter, buckets, output_sub_bucket_count,
                output_sub_bucket_rank, reorder_map_array, join_column_count,
                deduplicate, &local_join_count,
                global_join_duplicates, global_join_inserts,
                output->get_join_column_count(), output->get_is_canonical());
            // std::cout << "join buffer size after local join : "
            //           <<  join_buffer.width << std::endl;
            // std::cout << "local_negation_count " << local_join_count << " Threshold " << threshold << " k1 " << k1 << " offset " << *offset << " " << input0_buffer_width << std::endl;
            if (local_join_count > threshold)
            {
                *offset = k1 + input0_buffer_width;
                deduplicate.remove_tuple();
                return false;
            }
        }
    }
    else {
        // RIGHT
        
    }
    deduplicate.remove_tuple();
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
