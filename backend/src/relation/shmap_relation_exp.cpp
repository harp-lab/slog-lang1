/**
 * @file btree_relation.cpp
 * @author Yihao Sun (ysun67@syr.edu)
 * @brief the implmentation of slog relation using souffle's btree,
 *        reload all fucntion in original shmap, just to keep other part code working...
 * @version 0.1
 * @date 2021-12-15
 * 
 * @copyright Yihao Sun Copyright (c) 2021
 * 
 */

#include "../parallel_RA_inc.h"
#include "shmap_relation.h"
#include <cstddef>
#include <iostream>



shmap_relation::shmap_relation(int arity, bool id_flag)
{
    this->arity = arity;
    // ind = new t_ind(t_comparator(id_flag));
    this->id_flag = id_flag;
}

bool shmap_relation::insert_tuple_from_array(u64 *t, int width)
{
    t_tuple tp(t, t+width);

    return insert(tp);
}

std::pair<shmap_relation::iterator, shmap_relation::iterator>
shmap_relation::prefix_range(std::vector<u64> &prefix)
{
    if (prefix.size() >= arity+1)
        return std::make_pair(end(), end());
    t_tuple upper_bound(arity+1, std::numeric_limits<u64>::max());
    t_tuple lower_bound(arity+1, std::numeric_limits<u64>::min());
    for(size_t i = 0; i < prefix.size(); i++)
    {
        upper_bound[i] = prefix[i];
        lower_bound[i] = prefix[i];
    }
    return lowerUpperRange(lower_bound, upper_bound);
}


int shmap_relation::count()
{
    return size();
}

void shmap_relation::remove_tuple()
{
    this->purge();
}

bool shmap_relation::find_tuple_from_array(u64 *t, int width)
{
    // t_tuple upper_bound(arity+1, std::numeric_limits<u64>::max());
    // t_tuple lower_bound(arity+1, std::numeric_limits<u64>::min());
    // for(size_t i = 0; i < width; i++)
    // {
    //     upper_bound[i] = t[i];
    //     lower_bound[i] = t[i];
    // }
    t_tuple tp(t, t+width);
    auto joined_range = prefix_range(tp);
    if (joined_range.first == ind.end()) {
        return false;
    }

    return true;
}

// NOTE: prefix in this function is useless and also actually never use in other code
void shmap_relation::as_vector_buffer_recursive(vector_buffer *vb, std::vector<u64> prefix)
{
    if (size() == 0)
    {
        return;
    }
    for (const auto &cur_path : ind)
    {
        u64 path[cur_path.size()];
        for (u32 i = 0; i < cur_path.size(); i++)
        {
            path[i] = cur_path[i];
        }
        vb->vector_buffer_append((const unsigned char*)path, sizeof(u64)*cur_path.size());
    }
}

// NOTE: prefix in this function is useless and also actually never use in other code
void shmap_relation::as_all_to_allv_acopy_buffer(
    all_to_allv_buffer &buffer,
    std::vector<u64> prefix, // useless
    std::vector<int> reorder_map,
    int ra_id, u32 buckets,
    u32 *output_sub_bucket_count,
    u32 **output_sub_bucket_rank,
    u32 arity, u32 join_column_count,
    int head_rel_hash_col_count,
    bool canonical)
{
    if (size() == 0)
        return;
    for (const t_tuple &cur_path : ind)
    {
        u64 reordered_cur_path[buffer.width[ra_id]];
        for (int j =0; j < buffer.width[ra_id]; j++)
            reordered_cur_path[j] = cur_path[reorder_map[j]];

        uint64_t bucket_id = tuple_hash(reordered_cur_path, head_rel_hash_col_count) % buckets;
        uint64_t sub_bucket_id=0;
        if (canonical == false && arity != 0 && arity >= head_rel_hash_col_count)
        {
            sub_bucket_id = tuple_hash(reordered_cur_path + head_rel_hash_col_count, arity-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];
        }
        int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];
        buffer.local_compute_output_size_rel[ra_id] = buffer.local_compute_output_size_rel[ra_id] + buffer.width[ra_id];
        buffer.local_compute_output_size_total = buffer.local_compute_output_size_total + buffer.width[ra_id];
        buffer.local_compute_output_size_flat[index * buffer.ra_count + ra_id] = buffer.local_compute_output_size_flat[index * buffer.ra_count + ra_id] + buffer.width[ra_id];
        buffer.local_compute_output_count_flat[index * buffer.ra_count + ra_id] ++;
        
        buffer.local_compute_output_size[ra_id][index] = buffer.local_compute_output_size[ra_id][index] + buffer.width[ra_id];
        buffer.cumulative_tuple_process_map[index] = buffer.cumulative_tuple_process_map[index] + buffer.width[ra_id];
        buffer.local_compute_output[ra_id][index].vector_buffer_append((const unsigned char*)reordered_cur_path, sizeof(u64)*buffer.width[ra_id]);
    }
}

void shmap_relation::as_all_to_allv_copy_buffer(
    all_to_allv_buffer &buffer,
    std::vector<u64> prefix,        // useless arg
    std::vector<int> reorder_map,
    int ra_id, u32 buckets,
    u32 *output_sub_bucket_count,
    u32 **output_sub_bucket_rank,
    u32 arity, u32 join_column_count,
    int head_rel_hash_col_count,
    bool canonical)
{
    if (size() == 0)
        return;
    for (const t_tuple &cur_path : ind)
    {
        u64 reordered_cur_path[buffer.width[ra_id]];
        for (u32 j =0; j < reorder_map.size(); j++)
            reordered_cur_path[j] = cur_path[reorder_map[j]];

        uint64_t bucket_id = tuple_hash(reordered_cur_path, head_rel_hash_col_count) % buckets;
        uint64_t sub_bucket_id=0;
        if (canonical == false && arity != 0 && arity >= head_rel_hash_col_count)
            sub_bucket_id = tuple_hash(reordered_cur_path + head_rel_hash_col_count, arity-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];
        int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];
        buffer.local_compute_output_size_rel[ra_id] = buffer.local_compute_output_size_rel[ra_id] + buffer.width[ra_id];
        buffer.local_compute_output_size_total = buffer.local_compute_output_size_total + buffer.width[ra_id];
        buffer.local_compute_output_size_flat[index * buffer.ra_count + ra_id] = buffer.local_compute_output_size_flat[index * buffer.ra_count + ra_id] + buffer.width[ra_id];
        buffer.local_compute_output_count_flat[index * buffer.ra_count + ra_id] ++;

        buffer.local_compute_output_size[ra_id][index] = buffer.local_compute_output_size[ra_id][index] + buffer.width[ra_id];
        buffer.cumulative_tuple_process_map[index] = buffer.cumulative_tuple_process_map[index] + buffer.width[ra_id];
        buffer.local_compute_output[ra_id][index].vector_buffer_append((const unsigned char*)reordered_cur_path, sizeof(u64)*buffer.width[ra_id]); 
    }
}

void shmap_relation::as_all_to_allv_copy_filter_buffer(
    all_to_allv_buffer &buffer,
    std::vector<u64> prefix,
    std::vector<int> reorder_map,
    int ra_id, u32 buckets,
    u32 *output_sub_bucket_count,
    u32 **output_sub_bucket_rank,
    u32 arity, u32 join_column_count,
    bool (*lambda)(const u64 *const),
    int head_rel_hash_col_count,
    bool canonical)
{
    if (size() == 0)
        return;
    for (const t_tuple &cur_path : ind)
    {
        u64 reordered_cur_path[buffer.width[ra_id]];
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
            buffer.local_compute_output_size_rel[ra_id] = buffer.local_compute_output_size_rel[ra_id] + buffer.width[ra_id];
            buffer.local_compute_output_size_total = buffer.local_compute_output_size_total + buffer.width[ra_id];
            buffer.local_compute_output_size_flat[index * buffer.ra_count + ra_id] = buffer.local_compute_output_size_flat[index * buffer.ra_count + ra_id] + buffer.width[ra_id];
            buffer.local_compute_output_count_flat[index * buffer.ra_count + ra_id] ++;

            buffer.local_compute_output_size[ra_id][index] = buffer.local_compute_output_size[ra_id][index] + buffer.width[ra_id];
            buffer.cumulative_tuple_process_map[index] = buffer.cumulative_tuple_process_map[index] + buffer.width[ra_id];
            buffer.local_compute_output[ra_id][index].vector_buffer_append((const unsigned char*)reordered_cur_path, sizeof(u64)*buffer.width[ra_id]);
        }
    }
}

void shmap_relation::as_all_to_allv_copy_generate_buffer(
    all_to_allv_buffer &buffer,
    std::vector<u64> prefix,
    int ra_id, u32 buckets,
    u32 *output_sub_bucket_count,
    u32 **output_sub_bucket_rank,
    u32 arity, u32 join_column_count,
    int (*lambda)(const u64 *const, u64 *const),
    int head_rel_hash_col_count, bool canonical)
{
    if (size() == 0)
        return;
    for (const t_tuple &cur_path : ind)
    {
        int output_length = buffer.width[ra_id];
        if (buffer.width[ra_id] == 0) {
            output_length = 1;
        }
        u64 reordered_cur_path[output_length];
        u64 cur_path_array[cur_path.size()];
        for (u32 i=0; i < cur_path.size(); i++)
            cur_path_array[i] = cur_path[i];

        if (lambda(cur_path_array, reordered_cur_path) !=0)
        {
            uint64_t bucket_id = tuple_hash(reordered_cur_path, head_rel_hash_col_count) % buckets;
            uint64_t sub_bucket_id=0;
            if (canonical == false && arity != 0 && arity >= head_rel_hash_col_count)
                sub_bucket_id = tuple_hash(reordered_cur_path + head_rel_hash_col_count, arity-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

            int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];
            buffer.local_compute_output_size_rel[ra_id] = buffer.local_compute_output_size_rel[ra_id] + buffer.width[ra_id];
            buffer.local_compute_output_size_total = buffer.local_compute_output_size_total + buffer.width[ra_id];
            buffer.local_compute_output_size_flat[index * buffer.ra_count + ra_id] = buffer.local_compute_output_size_flat[index * buffer.ra_count + ra_id] + buffer.width[ra_id];
            buffer.local_compute_output_count_flat[index * buffer.ra_count + ra_id] ++;

            buffer.local_compute_output_size[ra_id][index] = buffer.local_compute_output_size[ra_id][index] + buffer.width[ra_id];
            buffer.cumulative_tuple_process_map[index] = buffer.cumulative_tuple_process_map[index] + buffer.width[ra_id];
            buffer.local_compute_output[ra_id][index].vector_buffer_append((const unsigned char*)reordered_cur_path, sizeof(u64)*buffer.width[ra_id]);
        }
    }
}

void shmap_relation::as_all_to_allv_right_join_buffer(
    std::vector<u64> prefix,
    all_to_allv_buffer &join_buffer,
    u64 *input0_buffer,
    int input0_buffer_width,
    int input1_buffer_width,
    int ra_id, u32 buckets,
    u32 *output_sub_bucket_count,
    u32 **output_sub_bucket_rank,
    std::vector<int> reorder_map,
    int join_column_count,
    shmap_relation &deduplicate,
    int *local_join_count,
    u32 *local_join_duplicates,
    u32 *local_join_inserts,
    int head_rel_hash_col_count,
    bool canonical)
{
    if (size() == 0)
        return;
    // construct range
    t_tuple upper_bound(arity+1, std::numeric_limits<u64>::max());
    t_tuple lower_bound(arity+1, std::numeric_limits<u64>::min());
    for(size_t i = 0; i < prefix.size(); i++)
    {
        upper_bound[i] = prefix[i];
        lower_bound[i] = prefix[i];
    }
    auto joined_range = lowerUpperRange(lower_bound, upper_bound);
    for(auto it = joined_range.first; it != joined_range.second && it != ind.end(); ++it)
    {
        auto cur_path = *it;
        u64 projected_path[join_buffer.width[ra_id]];
        u64 reordered_cur_path[input0_buffer_width + input1_buffer_width - join_column_count];
        for (int i = 0; i < input1_buffer_width; i++)
            reordered_cur_path[i] = cur_path[i];

        for (int i = join_column_count; i < input0_buffer_width; i++)
            reordered_cur_path[input1_buffer_width + (i - join_column_count)] = input0_buffer[i];

        for (int i =0; i < join_buffer.width[ra_id]; i++)
            projected_path[i] = reordered_cur_path[reorder_map[i]];

        if (deduplicate.insert_tuple_from_array(projected_path, join_buffer.width[ra_id]) == true)
        {
            uint64_t bucket_id = tuple_hash(projected_path, head_rel_hash_col_count) % buckets;
            uint64_t sub_bucket_id=0;
            if (canonical == false)
                sub_bucket_id = tuple_hash(projected_path + head_rel_hash_col_count, join_buffer.width[ra_id]-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

            int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];

            join_buffer.local_compute_output_size_rel[ra_id] = join_buffer.local_compute_output_size_rel[ra_id] + join_buffer.width[ra_id];
            join_buffer.local_compute_output_size_total = join_buffer.local_compute_output_size_total + join_buffer.width[ra_id];
            join_buffer.local_compute_output_size_flat[index*join_buffer.ra_count + ra_id] = join_buffer.local_compute_output_size_flat[index*join_buffer.ra_count + ra_id] + join_buffer.width[ra_id];
            join_buffer.local_compute_output_count_flat[index * join_buffer.ra_count + ra_id] ++;
            join_buffer.local_compute_output_size[ra_id][index] = join_buffer.local_compute_output_size[ra_id][index] + join_buffer.width[ra_id];

            join_buffer.cumulative_tuple_process_map[index] = join_buffer.cumulative_tuple_process_map[index] + join_buffer.width[ra_id];
            join_buffer.local_compute_output[ra_id][index].vector_buffer_append((const unsigned char*)projected_path, sizeof(u64)*join_buffer.width[ra_id]);
            (*local_join_inserts)++;
            (*local_join_count)++;
        }
        else {
            (*local_join_duplicates)++;
        }
    }
    // std::cout << "inserted " << *local_join_inserts << std::endl;
}

void shmap_relation::as_all_to_allv_left_join_buffer(
    std::vector<u64> prefix,
    all_to_allv_buffer &join_buffer,
    u64 *input0_buffer, int input0_buffer_width,
    int input1_buffer_width, int ra_id, u32 buckets,
    u32 *output_sub_bucket_count,
    u32 **output_sub_bucket_rank,
    std::vector<int> reorder_map,
    int join_column_count,
    shmap_relation &deduplicate,
    int *local_join_count,
    u32 *local_join_duplicates,
    u32 *local_join_inserts,
    int head_rel_hash_col_count,
    bool canonical)
{
    if (size() == 0)
        return;
    // construct range
    t_tuple upper_bound(arity+1, std::numeric_limits<u64>::max());
    t_tuple lower_bound(arity+1, std::numeric_limits<u64>::min());
    for(size_t i = 0; i < prefix.size(); i++)
    {
        upper_bound[i] = prefix[i];
        lower_bound[i] = prefix[i];
    }
    auto joined_range = lowerUpperRange(lower_bound, upper_bound);
    for(auto it = joined_range.first; it != joined_range.second && it != ind.end(); ++it)
    {
        auto cur_path = *it;
        u64 projected_path[join_buffer.width[ra_id]];
        u64 reordered_cur_path[input0_buffer_width + input1_buffer_width - join_column_count];
        for (int i = 0; i < input0_buffer_width; i++)
            reordered_cur_path[i] = input0_buffer[i];

        for (int i = join_column_count; i < input1_buffer_width; i++)
            reordered_cur_path[input0_buffer_width + (i - join_column_count)] = cur_path[i];

        for (int i =0; i < join_buffer.width[ra_id]; i++)
            projected_path[i] = reordered_cur_path[reorder_map[i]];

        //std::cout << "NT " << projected_path[0] << " " << projected_path[1] << std::endl;
        if (deduplicate.insert_tuple_from_array(projected_path, join_buffer.width[ra_id]) == true)
        {
            uint64_t bucket_id = tuple_hash(projected_path, head_rel_hash_col_count) % buckets;
            uint64_t sub_bucket_id=0;
            if (canonical == false)
                sub_bucket_id = tuple_hash(projected_path + head_rel_hash_col_count, join_buffer.width[ra_id]-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

            int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];

            join_buffer.local_compute_output_size_rel[ra_id] = join_buffer.local_compute_output_size_rel[ra_id] + join_buffer.width[ra_id];
            join_buffer.local_compute_output_size_total = join_buffer.local_compute_output_size_total + join_buffer.width[ra_id];
            join_buffer.local_compute_output_size_flat[index*join_buffer.ra_count + ra_id] = join_buffer.local_compute_output_size_flat[index*join_buffer.ra_count + ra_id] + join_buffer.width[ra_id];
            join_buffer.local_compute_output_count_flat[index * join_buffer.ra_count + ra_id] ++;

            join_buffer.local_compute_output_size[ra_id][index] = join_buffer.local_compute_output_size[ra_id][index] + join_buffer.width[ra_id];
            join_buffer.cumulative_tuple_process_map[index] = join_buffer.cumulative_tuple_process_map[index] + join_buffer.width[ra_id];
            join_buffer.local_compute_output[ra_id][index].vector_buffer_append((const unsigned char*)projected_path, sizeof(u64)*join_buffer.width[ra_id]);
            (*local_join_inserts)++;
            (*local_join_count)++;
        }
        else {
            (*local_join_duplicates)++;
        }
    }
    // std::cout << "inserted " << *local_join_inserts << std::endl;
}

void shmap_relation::as_all_to_allv_right_outer_join_buffer(
    u64 *input0_buffer, int input0_buffer_size, int input0_buffer_width,
    int *offset,
    all_to_allv_buffer &join_buffer,
    int ra_id, u32 buckets,
    u32 *output_sub_bucket_count,
    u32 **output_sub_bucket_rank,
    std::vector<int> &reorder_map,
    int join_column_count,
    int out_airty,
    int head_rel_hash_col_count,
    bool canonical)
{
    if (this->size() == 0)
        return;
    // should I reconstruct the btree here? is there better data structure here?
    shmap_relation negated_target(join_column_count, false);
    for (int k1 = *offset; k1 < input0_buffer_size; k1 = k1 + input0_buffer_width)
    {
        negated_target.insert_tuple_from_array(input0_buffer+k1, join_column_count);
    }

    for (const t_tuple &cur_path : ind)
    {
        t_tuple joined_cur_path(cur_path.begin(), cur_path.begin() + join_column_count);
        if (!negated_target.contains(joined_cur_path))
        {
            u64 reordered_cur_path[join_buffer.width[ra_id]];
            for (u32 j =0; j < reorder_map.size(); j++)
                reordered_cur_path[j] = cur_path[reorder_map[j]];

            uint64_t bucket_id = tuple_hash(reordered_cur_path, head_rel_hash_col_count) % buckets;
            uint64_t sub_bucket_id=0;
            if (canonical == false && out_airty != 0 && out_airty >= head_rel_hash_col_count)
                sub_bucket_id = tuple_hash(reordered_cur_path + head_rel_hash_col_count, out_airty-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

            int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];
            join_buffer.local_compute_output_size_rel[ra_id] = join_buffer.local_compute_output_size_rel[ra_id] + join_buffer.width[ra_id];
            join_buffer.local_compute_output_size_total = join_buffer.local_compute_output_size_total + join_buffer.width[ra_id];
            join_buffer.local_compute_output_size_flat[index * join_buffer.ra_count + ra_id] = join_buffer.local_compute_output_size_flat[index * join_buffer.ra_count + ra_id] + join_buffer.width[ra_id];
            join_buffer.local_compute_output_count_flat[index * join_buffer.ra_count + ra_id] ++;

            join_buffer.local_compute_output_size[ra_id][index] = join_buffer.local_compute_output_size[ra_id][index] + join_buffer.width[ra_id];
            join_buffer.cumulative_tuple_process_map[index] = join_buffer.cumulative_tuple_process_map[index] + join_buffer.width[ra_id];
            join_buffer.local_compute_output[ra_id][index].vector_buffer_append((const unsigned char*)reordered_cur_path, sizeof(u64)*join_buffer.width[ra_id]);
        }
    }
    negated_target.purge();
}
