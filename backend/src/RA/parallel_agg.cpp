
#include "../parallel_RA_inc.h"
#include "parallel_agg.h"
#include <array>
#include <cassert>
#include <iostream>
#include <ostream>
#include <vector>

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
                join_negation_target_table->get_full() + mcomm.get_rank(),
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

void parallel_join_aggregate::local_aggregate(
    u32 buckets, int *offset,
    int input0_buffer_size, u64 *input0_buffer,
    all_to_allv_buffer &agg_buffer, int ra_counter
    ) {

    relation* input = this->join_aggregate_input_table;
    relation* target = this->join_aggregate_target_table;
    relation* output = this->join_aggregate_output_table;
    int input0_buffer_width = target->get_arity() + 1;

    u32* output_sub_bucket_count = output->get_sub_bucket_per_bucket_count();
    u32** output_sub_bucket_rank = output->get_sub_bucket_rank();
    u32 real_join_count = output->get_join_column_count();
    agg_buffer.width[ra_counter] = output->get_arity();

    shmap_relation* agg_target;
    if (*(target->get_sub_bucket_per_bucket_count()) == 1) {
        agg_target = target->get_full() + mcomm.get_rank();
    } else {
        agg_target = new shmap_relation(target->get_arity()+1, false);
        for (int k1 = *offset; k1 < input0_buffer_size; k1 = k1 + input0_buffer_width)
        {
            agg_target->insert_tuple_from_array(input0_buffer+k1, target->get_arity()+1);
        }
    }

    btree::btree_map<std::vector<u64>, u64, shmap_relation::t_comparator> res_map;
    for (u32 bucket=0; bucket < buckets; bucket ++) {
        for (auto tuple: input->get_full()[bucket]) {
            std::vector<u64> data_v(tuple.begin(), tuple.begin()+target->get_join_column_count());
            // std::cout << "On rank " << mcomm.get_rank() << " bucket " << *(target->get_sub_bucket_per_bucket_count()) << std::endl;
            auto joined_range = agg_target->prefix_range(data_v);
            auto agg_data = local_func(joined_range);
            if (*(target->get_sub_bucket_per_bucket_count()) != 1 &&
                res_map.find(data_v) != res_map.end()) {
                // std::cout << "reduce"
                res_map[data_v] = reduce_func(res_map[data_v], agg_data);
            } else {
                res_map[data_v] = agg_data;
            }
        }
    }

    for (u32 bucket=0; bucket < buckets; bucket ++) {
        for (auto input_tuple: input->get_full()[bucket]) {
            std::vector<u64> joined_input_tuple(input_tuple.begin(), input_tuple.begin()+input->get_join_column_count());
            auto agg_res = res_map[joined_input_tuple];
            std::vector<u64> tuple(reorder_mapping.size(), 0);
            int reorder_agg_index = input->get_arity() + 1;
            for (long unsigned int j = 0; j < reorder_mapping.size(); j++) {
              if (reorder_mapping[j] == reorder_agg_index) {
                tuple[j] = agg_res;
              } else {
                tuple[j] = input_tuple[reorder_mapping[j]];
              }
            }

            uint64_t bucket_id = tuple_hash(tuple.data(), output->get_join_column_count()) % buckets;
            uint64_t sub_bucket_id = 0;
            if (input->get_is_canonical() == false && output->get_arity() != 0 && output->get_arity() >= real_join_count) {

                sub_bucket_id = tuple_hash(tuple.data()+real_join_count, output->get_arity()-real_join_count) % output_sub_bucket_count[bucket_id];
            }
            int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];
            // std::cout << "index : " << index << std::endl;
            agg_buffer.local_compute_output_size_rel[ra_counter] = agg_buffer.local_compute_output_size_rel[ra_counter] + agg_buffer.width[ra_counter];
            agg_buffer.local_compute_output_size_total = agg_buffer.local_compute_output_size_total+agg_buffer.width[ra_counter];
            agg_buffer.local_compute_output_size_flat[index*agg_buffer.ra_count+ra_counter] = agg_buffer.local_compute_output_size_flat[index*agg_buffer.ra_count + ra_counter] + agg_buffer.width[ra_counter];
            agg_buffer.local_compute_output_count_flat[index*agg_buffer.ra_count+ra_counter]++;
            agg_buffer.local_compute_output_size[ra_counter][index] = agg_buffer.local_compute_output_size[ra_counter][index]+agg_buffer.width[ra_counter];
            agg_buffer.cumulative_tuple_process_map[index] = agg_buffer.cumulative_tuple_process_map[index] + agg_buffer.width[ra_counter];
            agg_buffer.local_compute_output[ra_counter][index].vector_buffer_append((const unsigned char*)tuple.data(), sizeof(u64)*agg_buffer.width[ra_counter]);
        }
    }
    if (*(target->get_sub_bucket_per_bucket_count()) != 1) {
        agg_target->remove_tuple();
        delete agg_target;
    }
    res_map.clear();
}
