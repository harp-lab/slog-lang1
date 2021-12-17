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
#include "btree_relation.h"
#include "shmap_relation.h"
// #include <cstddef>
// #include <limits>

// #define MAX_U64 std::numeric_limits<uint64_t>.max()
// #define MIN_U64 std::numeric_limits<uint64_t>.min()


btree_relation::btree_relation(int arity)
{
    this->arity = arity;
    data_structure_type = BTREE;
}

souffle::range<btree_relation::iterator> btree_relation::lowerUpperRange(
    const t_tuple &lower, const t_tuple &upper, context &h)
{
    btree_relation::t_comparator comparator;
    int cmp = comparator(lower, upper);
    if (cmp == 0) {
      auto pos = ind.find(lower, h.hints_0_lower);
      auto fin = ind.end();
      if (pos != fin) {
        fin = pos;
        ++fin;
      }
      return souffle::make_range(pos, fin);
    }
    if (cmp > 0) {
      return souffle::make_range(ind.end(), ind.end());
    }
    return souffle::make_range(ind.lower_bound(lower, h.hints_0_lower),
                               ind.upper_bound(upper, h.hints_0_upper));
}

bool btree_relation::insert_tuple_from_array(u64 *t, int arity)
{
    // std::cout << "inserting ..." << std::endl;
    t_tuple tp(t, t+arity-1);

    return insert(tp);
}

int btree_relation::count()
{
    return size();
}

void btree_relation::remove_tuple()
{
    this->purge();
}

bool btree_relation::find_tuple_from_array(u64 *t, int arity)
{
    t_tuple tp(t, t+arity);
    return this->contains(tp);
}

// NOTE: prefix in this function is useless and also actually never use in other code
void btree_relation::as_vector_buffer_recursive(vector_buffer *vb, std::vector<u64> prefix)
{
    iterator cur = this->begin();
    while (cur != this->end()) {
        vb->vector_buffer_append((const unsigned char*)(&(*cur)), sizeof(u64)*this->arity);
        ++cur;
    }
}

// NOTE: prefix in this function is useless and also actually never use in other code
void btree_relation::as_all_to_allv_acopy_buffer(
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
    for (const t_tuple &cur_path : (*this))
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
        buffer.local_compute_output_size[ra_id][index] = buffer.local_compute_output_size[ra_id][index] + buffer.width[ra_id];
        buffer.cumulative_tuple_process_map[index] = buffer.cumulative_tuple_process_map[index] + buffer.width[ra_id];
        buffer.local_compute_output[ra_id][index].vector_buffer_append((const unsigned char*)reordered_cur_path, sizeof(u64)*buffer.width[ra_id]);
    }
}

void btree_relation::as_all_to_allv_copy_buffer(
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
    for (const t_tuple &cur_path : (*this))
    {
        u64 reordered_cur_path[buffer.width[ra_id]];
        for (u32 j =0; j < reorder_map.size(); j++)
            reordered_cur_path[j] = cur_path[reorder_map[j]];

        uint64_t bucket_id = tuple_hash(reordered_cur_path, head_rel_hash_col_count) % buckets;
        uint64_t sub_bucket_id=0;
        if (canonical == false && arity != 0 && arity >= head_rel_hash_col_count)
            sub_bucket_id = tuple_hash(reordered_cur_path + head_rel_hash_col_count, arity-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

        //std::cout << "Copy size " << buffer.width[ra_id] << std::endl;
        //std::cout << "Copy happening " << cur_path[0] << " " << cur_path[1] <<  std::endl;
        //std::cout << "Copy happening " << reordered_cur_path[0] << " " << reordered_cur_path[1] <<  std::endl;
        //std::cout << "Bucket id " << bucket_id << " sub bucket id " <<sub_bucket_id << std::endl;
        int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];
        buffer.local_compute_output_size_rel[ra_id] = buffer.local_compute_output_size_rel[ra_id] + buffer.width[ra_id];
        buffer.local_compute_output_size_total = buffer.local_compute_output_size_total + buffer.width[ra_id];
        buffer.local_compute_output_size_flat[index * buffer.ra_count + ra_id] = buffer.local_compute_output_size_flat[index * buffer.ra_count + ra_id] + buffer.width[ra_id];

        buffer.local_compute_output_size[ra_id][index] = buffer.local_compute_output_size[ra_id][index] + buffer.width[ra_id];
        buffer.cumulative_tuple_process_map[index] = buffer.cumulative_tuple_process_map[index] + buffer.width[ra_id];
        buffer.local_compute_output[ra_id][index].vector_buffer_append((const unsigned char*)reordered_cur_path, sizeof(u64)*buffer.width[ra_id]); 
    }
}

void btree_relation::as_all_to_allv_copy_filter_buffer(
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
    for (const t_tuple &cur_path : (*this))
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

            buffer.local_compute_output_size[ra_id][index] = buffer.local_compute_output_size[ra_id][index] + buffer.width[ra_id];
            buffer.cumulative_tuple_process_map[index] = buffer.cumulative_tuple_process_map[index] + buffer.width[ra_id];
            buffer.local_compute_output[ra_id][index].vector_buffer_append((const unsigned char*)reordered_cur_path, sizeof(u64)*buffer.width[ra_id]);
        }
    }
}

void btree_relation::as_all_to_allv_copy_generate_buffer(
    all_to_allv_buffer &buffer,
    std::vector<u64> prefix,
    int ra_id, u32 buckets,
    u32 *output_sub_bucket_count,
    u32 **output_sub_bucket_rank,
    u32 arity, u32 join_column_count,
    int (*lambda)(const u64 *const, u64 *const),
    int head_rel_hash_col_count, bool canonical)
{
    for (const t_tuple &cur_path : (*this))
    {
        u64 reordered_cur_path[buffer.width[ra_id]];
        u64 cur_path_array[cur_path.size()];
        for (u32 i=0; i < cur_path.size(); i++)
            cur_path_array[i] = cur_path[i];

        if (lambda(cur_path_array, reordered_cur_path) !=0)
        {
            uint64_t bucket_id = tuple_hash(reordered_cur_path, head_rel_hash_col_count) % buckets;
            uint64_t sub_bucket_id=0;
            if (canonical == false && arity != 0 && arity >= head_rel_hash_col_count)
                sub_bucket_id = tuple_hash(reordered_cur_path + head_rel_hash_col_count, arity-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

            //std::cout << "CG bucket_id " << bucket_id << " sub_bucket_id " << sub_bucket_id << std::endl;
            int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];
            buffer.local_compute_output_size_rel[ra_id] = buffer.local_compute_output_size_rel[ra_id] + buffer.width[ra_id];
            buffer.local_compute_output_size_total = buffer.local_compute_output_size_total + buffer.width[ra_id];
            buffer.local_compute_output_size_flat[index * buffer.ra_count + ra_id] = buffer.local_compute_output_size_flat[index * buffer.ra_count + ra_id] + buffer.width[ra_id];

            buffer.local_compute_output_size[ra_id][index] = buffer.local_compute_output_size[ra_id][index] + buffer.width[ra_id];
            buffer.cumulative_tuple_process_map[index] = buffer.cumulative_tuple_process_map[index] + buffer.width[ra_id];
            buffer.local_compute_output[ra_id][index].vector_buffer_append((const unsigned char*)reordered_cur_path, sizeof(u64)*buffer.width[ra_id]);
        }
    }
}

void btree_relation::as_all_to_allv_right_join_buffer(
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
    // construct range
    t_tuple upper_bound(arity, std::numeric_limits<u64>::max());
    t_tuple lower_bound(arity, std::numeric_limits<u64>::min());
    for(size_t i = 0; i < prefix.size(); i++)
    {
        if (i >= arity)
        {
            break;
        }
        upper_bound[i] = prefix[i];
        lower_bound[i] = prefix[i];
    }
    auto joined_range = lowerUpperRange(upper_bound, lower_bound);
    for(const auto &cur_path : joined_range)
    {
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
            join_buffer.local_compute_output_size[ra_id][index] = join_buffer.local_compute_output_size[ra_id][index] + join_buffer.width[ra_id];
            join_buffer.cumulative_tuple_process_map[index] = join_buffer.cumulative_tuple_process_map[index] + join_buffer.width[ra_id];
            join_buffer.local_compute_output[ra_id][index].vector_buffer_append((const unsigned char*)projected_path, sizeof(u64)*join_buffer.width[ra_id]);
            (*local_join_inserts)++;
            (*local_join_count)++;
        }
        else
            (*local_join_duplicates)++;
    }
}

void btree_relation::as_all_to_allv_left_join_buffer(
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
    // construct range
    t_tuple upper_bound(arity, std::numeric_limits<u64>::max());
    t_tuple lower_bound(arity, std::numeric_limits<u64>::min());
    for(size_t i = 0; i < prefix.size(); i++)
    {
        if (i >= arity)
        {
            break;
        }
        upper_bound[i] = prefix[i];
        lower_bound[i] = prefix[i];
    }
    auto joined_range = lowerUpperRange(upper_bound, lower_bound);
    for(const auto &cur_path : joined_range)
    {
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
            join_buffer.local_compute_output_size[ra_id][index] = join_buffer.local_compute_output_size[ra_id][index] + join_buffer.width[ra_id];
            join_buffer.cumulative_tuple_process_map[index] = join_buffer.cumulative_tuple_process_map[index] + join_buffer.width[ra_id];
            join_buffer.local_compute_output[ra_id][index].vector_buffer_append((const unsigned char*)projected_path, sizeof(u64)*join_buffer.width[ra_id]);
            (*local_join_inserts)++;
            (*local_join_count)++;
        }
        else
            (*local_join_duplicates)++;
    }
}

void btree_relation::as_all_to_allv_right_outer_join_buffer(
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
    // should I reconstruct the btree here? is there better data structure here?
    btree_relation negated_target(join_column_count);
    for (int k1 = *offset; k1 < input0_buffer_size; k1 = k1 + input0_buffer_width)
    {
        // std::cout << "NEG PREFIX  ";
        // for (int jc=0; jc < join_column_count; jc++)
        // {
        //     prefix[jc] = input0_buffer[k1 + jc];
        //     // std::cout << input0_buffer[k1 + jc] << " ";
        // }
        // std::cout << std::endl;
        negated_target.insert_tuple_from_array(input0_buffer+k1, join_column_count);
    }

    for (const t_tuple &cur_path : (*this))
    {
        t_tuple joined_cur_path(cur_path.begin(), cur_path.begin() + join_column_count);
        if (negated_target.contains(joined_cur_path))
        {
            u64 reordered_cur_path[join_buffer.width[ra_id]];
            for (u32 j =0; j < reorder_map.size(); j++)
                reordered_cur_path[j] = cur_path[reorder_map[j]];

            uint64_t bucket_id = tuple_hash(reordered_cur_path, head_rel_hash_col_count) % buckets;
            uint64_t sub_bucket_id=0;
            if (canonical == false && arity != 0 && arity >= head_rel_hash_col_count)
                sub_bucket_id = tuple_hash(reordered_cur_path + head_rel_hash_col_count, arity-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

            //std::cout << "Copy size " << buffer.width[ra_id] << std::endl;
            //std::cout << "Copy happening " << cur_path[0] << " " << cur_path[1] <<  std::endl;
            //std::cout << "Copy happening " << reordered_cur_path[0] << " " << reordered_cur_path[1] <<  std::endl;
            //std::cout << "Bucket id " << bucket_id << " sub bucket id " <<sub_bucket_id << std::endl;
            int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];
            join_buffer.local_compute_output_size_rel[ra_id] = join_buffer.local_compute_output_size_rel[ra_id] + join_buffer.width[ra_id];
            join_buffer.local_compute_output_size_total = join_buffer.local_compute_output_size_total + join_buffer.width[ra_id];
            join_buffer.local_compute_output_size_flat[index * join_buffer.ra_count + ra_id] = join_buffer.local_compute_output_size_flat[index * join_buffer.ra_count + ra_id] + join_buffer.width[ra_id];

            join_buffer.local_compute_output_size[ra_id][index] = join_buffer.local_compute_output_size[ra_id][index] + join_buffer.width[ra_id];
            join_buffer.cumulative_tuple_process_map[index] = join_buffer.cumulative_tuple_process_map[index] + join_buffer.width[ra_id];
            join_buffer.local_compute_output[ra_id][index].vector_buffer_append((const unsigned char*)reordered_cur_path, sizeof(u64)*join_buffer.width[ra_id]);
    
        }
    }
}
