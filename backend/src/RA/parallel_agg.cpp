
#include "../parallel_RA_inc.h"

bool parallel_join_negate::local_negation(
    int threshold, int* offset, int join_order, u32 buckets,
    int input0_buffer_size, int input0_buffer_width, u64 *input0_buffer,
    shmap_relation *input0,
    shmap_relation *input1, u32 i1_size, int input1_buffer_width,
    std::vector<int> reorder_map_array,
    relation* output, all_to_allv_buffer& join_buffer,
    int counter, int join_column_count, u32* global_join_duplicates,
    u32* global_join_inserts)
{
    join_buffer.width[counter] = reorder_map_array.size();

    shmap_relation deduplicate;
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
                std::cout << "PREFIX " << input0_buffer[k1 + jc] << std::endl;
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
            std::cout << "local_negation_count " << local_join_count << " Threshold " << threshold << " k1 " << k1 << " offset " << *offset << " " << input0_buffer_width << std::endl;
            if (local_join_count > threshold)
            {
                *offset = k1 + input0_buffer_width;
                deduplicate.remove_tuple();
                return false;
            }
        }
    }
    deduplicate.remove_tuple();
    return true;
}