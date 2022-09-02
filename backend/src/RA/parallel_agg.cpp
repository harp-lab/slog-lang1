
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

void parallel_copy_aggregate::local_aggregate(
    u32 buckets, int *offset,
    int input0_buffer_size, u64 *input0_buffer,
    all_to_allv_buffer &agg_buffer, int ra_counter
    ) {
    relation* input = this->copy_aggregate_input_table;
    relation* target = this->copy_aggregate_target_table;
    relation* output = this->copy_aggregate_output_table;
    int input0_buffer_width = target->get_arity() + 1;

    u32* output_sub_bucket_count = output->get_sub_bucket_per_bucket_count();
    u32** output_sub_bucket_rank = output->get_sub_bucket_rank();
    int real_join_count = output->get_join_column_count() - 1;
    agg_buffer.width[ra_counter] = real_join_count + 1;
    int agg_count = 0;
    std::vector<std::vector<u64>> res_tuples;
    if (agg_type ==  SpecialAggregator::recusive) {
        // recursive aggregation, input is output
        shmap_relation result_tree;
        for (auto tuple: output->get_full()[mcomm.get_rank()]) {
            result_tree.insert(tuple);
        }
        // run util fixpoint
        u64 old_count = 0;
        u64 new_count = result_tree.count();
        while (old_count != new_count) {
            for (auto tuple: result_tree) {
                u64 res_v[real_join_count+2];
                auto agg_data = local_func(target->get_full()[mcomm.get_rank()], tuple, output->get_arity()-1);
                agg_count += global_func(tuple.data(), agg_data, agg_count, res_v);
                res_tuples.push_back(std::vector<u64>(res_v, res_v+real_join_count+2));
            }
        }
    } else {
        std::vector<u64> data_v(target->get_join_column_count(), 0);
        for (auto tuple: input->get_full()[mcomm.get_rank()]) {
            u64 res_v[real_join_count+2];
            for (int j=0; j < input->get_join_column_count(); j++) {
                data_v[j] = tuple[j];
            }

            auto agg_data = local_func(target->get_full()[mcomm.get_rank()], data_v, input->get_join_column_count());

            agg_count += global_func(data_v.data(), agg_data, agg_count, res_v);

            res_tuples.push_back(std::vector<u64>(res_v, res_v+real_join_count+2));
        }
    }

    for (auto tuple: res_tuples) {

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
