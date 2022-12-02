/**
 * @file btree_relation.cpp
 * @author Yihao Sun (ysun67@syr.edu)
 * @brief the implementation of slog relation using souffle's btree,
 *        reload all function in original shmap, just to keep other part code working...
 * @version 0.1
 * @date 2021-12-15
 * 
 * @copyright Yihao Sun Copyright (c) 2021
 * 
 */

#include "../parallel_RA_inc.h"
#include "balanced_hash_relation.h"
#include "shmap_relation.h"
#include <cassert>
#include <cstddef>
#include <iostream>
#include <mpi.h>
#include <ostream>
#include <vector>

shmap_relation::shmap_relation(int arity, bool id_flag)
{
    this->arity = arity;
    // ind = new t_ind(t_comparator(id_flag));
}

int shmap_relation::insert_tuple_from_array(u64 *t, int width)
{
    t_tuple tp(t, t+width);
    // check if relation has functional dependance
    if (dependent_column_indices.size() > 0) {
        std::vector<u64> index_columns;
        std::vector<u64> dependent_columns;
        t_tuple upper_bound(width, std::numeric_limits<u64>::max());
        t_tuple lower_bound(width, std::numeric_limits<u64>::min());
        for (int i = 0; i < width-dependent_column_indices.size();  i++) {
            upper_bound[i] = tp[i];
            lower_bound[i] = tp[i];
        }
        for (auto i: dependent_column_indices) {
            dependent_columns.push_back(t[i]);
        }
        auto exist_tuples_range = lowerUpperRange(lower_bound, upper_bound);
        if (exist_tuples_range.first == ind.end()) {
            // std::cout << "adding to lattice with <<<<<< ";
            // for (auto c: tp) {
            //     std::cout << c << " ";
            // }
            // std::cout << " while lower bound ... ";
            // for (auto c: lower_bound) {
            //     std::cout << c << " ";
            // }
            // std::cout << std::endl;
            // std::cout << "The current btree: " << std::endl;
            // for (auto t: ind) {
            //     std::cout << "Tuple : ";
            //     for (auto c: t) {
            //         std::cout << c << " ";
            //     }
            //     std::cout << std::endl;
            // }
            if (insert(tp)) {
                return INSERT_SUCCESS;
            } else {
                return INSERT_FAIL;
            }
        } else {
            // update
            // iterator need_delete = ind.end();
            std::vector<iterator> need_deletes;
            bool joined = false;
            for (auto it = exist_tuples_range.first; it != exist_tuples_range.second; it++) {
                auto cur_tuple = *it;
                // if (tp[0] == 59 && tp[1] == 58) {
                //     std::cout << "tppppp  <<<<<< ";
                //     for (auto c: cur_tuple) {
                //         std::cout << c << " ";
                //     }
                //     std::cout << std::endl;
                // }
                
                std::vector<u64> old_t;
                for (auto i: dependent_column_indices) {
                    old_t.push_back(cur_tuple[i]);
                }
                auto compare_res = update_compare_func(old_t, dependent_columns, tp);
                if (!compare_res.has_value()) {
                    continue;
                }
                if (compare_res.value()) {
                    need_deletes.push_back(it);  
                    // if (tp[0] == 59 && tp[1] == 58) {
                    //     for (auto c: cur_tuple) {
                    //         std::cout << c << " ";
                    //     }
                    //     std::cout << "update with " << compare_res.value() <<" <<<<<< ";
                    //     for (auto c: tp) {
                    //         std::cout << c << " ";
                    //     }
                    //     std::cout << std::endl;
                    // }
                }
                joined = true;
            }
            if (!joined) {
                if (insert(tp)) {
                    return INSERT_SUCCESS;
                } else {
                    return INSERT_FAIL;
                }
            }
            if (!need_deletes.empty()) {
                for (auto d: need_deletes) {
                    // std::cout << "delete >>>>  ";
                    // for (auto c: *d) {
                    //     std::cout << c << " ";
                    // }
                    // std::cout << std::endl;
                    ind.erase(*d);
                }
                if (insert(tp)) {
                    return INSERT_SUCCESS;
                } else {
                    return INSERT_UPDATED;
                }
            } else {
                return INSERT_FAIL;
            }
        }
    } else {
        // std::cout << "adding to normal "<< arity << "  with <<<<<< ";
        // for (auto c: tp) {
        //     std::cout << c << " ";
        // }
        // std::cout << std::endl;
        if (insert(tp)) {
            return INSERT_SUCCESS;
        } else {
            return INSERT_FAIL;
        }
    }
}

bool
shmap_relation::check_dependent_insertion(const std::vector<u64> &tp) {
    if (dependent_column_indices.size() > 0) {
        std::vector<u64> index_columns;
        std::vector<u64> dependent_columns;
        t_tuple upper_bound(tp.size(), std::numeric_limits<u64>::max());
        t_tuple lower_bound(tp.size(), std::numeric_limits<u64>::min());
        for (size_t i = 0; i < tp.size()-dependent_column_indices.size();  i++) {
            upper_bound[i] = tp[i];
            lower_bound[i] = tp[i];
        }
        for (auto i: dependent_column_indices) {
            dependent_columns.push_back(tp[i]);
        }
        auto exist_tuples_range = lowerUpperRange(lower_bound, upper_bound);
        if (exist_tuples_range.first == ind.end()) {
            return true;
        } else {
            auto joined = false;
            for (auto it = exist_tuples_range.first; it != exist_tuples_range.second; it++) {
                auto cur_tuple = *it;
                std::vector<u64> old_t;
                for (auto i: dependent_column_indices) {
                    old_t.push_back(cur_tuple[i]);
                }
                auto compare_res = update_compare_func(old_t, dependent_columns, tp);
                if (!compare_res.has_value()) {
                    continue;
                }
                if (compare_res.value()) {
                    joined = true;
                    return true;
                } else {
                    joined = true;
                }
            }
            // std::cout << " not adding to lattice with <<<<<< ";
            // for (auto c: tp) {
            //     std::cout << c << " ";
            // }
            // std::cout << " while lower bound ... ";
            // for (auto c: lower_bound) {
            //     std::cout << c << " ";
            // }
            // std::cout << std::endl;
            // std::cout << "The current btree: " << std::endl;
            // for (auto& t: ind) {
            //     std::cout << "Tuple : ";
            //     for (auto c: t) {
            //         std::cout << c << " ";
            //     }
            //     std::cout << std::endl;
            // }
            if (!joined) {
                return true;
            } else {
                return false;        
            }
        }
    } else {
        return true;
    }
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
    bool canonical,
    bool generator_mode, join_generator_func_t gen_func)
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
    // std::cout << "cur tree >>> " << std::endl;
    // for (auto r:  ind) {
    //     std::cout << ">>> ";
    //     for (auto c: r) {
    //         std::cout << c << " ";
    //     }
    //     std::cout << std::endl;
    // }
    // std::cout << "upper bound >> ";
    // for (auto c: upper_bound) {
    //     std::cout << c << " ";
    // }
    // std::cout << std::endl;
    auto joined_range = lowerUpperRange(lower_bound, upper_bound);

    if (generator_mode) {
        std::vector<u64> input_t(input0_buffer, input0_buffer+input0_buffer_width);
        std::vector<std::vector<u64>> eq_tuple_set;
        std::vector<std::vector<u64>> generated_tuple_set;
        std::vector<u64> prev_non_dependent_columns;
        for(auto it = joined_range.first; it != joined_range.second && it != ind.end(); ++it){
            auto cur_path = *it;
            std::vector<u64> cur_non_dependent_columns(cur_path.begin(), cur_path.begin()+arity+1-dependent_column_indices.size());
            if (cur_non_dependent_columns == prev_non_dependent_columns) {
                eq_tuple_set.push_back(cur_path);
                continue;
            } else {
                if (eq_tuple_set.size() != 0) {
                    gen_func(eq_tuple_set, input_t, generated_tuple_set);
                    eq_tuple_set.clear();
                }
                prev_non_dependent_columns = cur_non_dependent_columns;
                eq_tuple_set.push_back(cur_path);
            }
        }
        if (eq_tuple_set.size() != 0) {
            gen_func(eq_tuple_set, input_t, generated_tuple_set);
        }
        for (auto& tp: generated_tuple_set) {
            uint64_t bucket_id = tuple_hash(tp.data(), head_rel_hash_col_count) % buckets;
            uint64_t sub_bucket_id=0;
            if (canonical == false)
                sub_bucket_id = tuple_hash(tp.data() + head_rel_hash_col_count, join_buffer.width[ra_id]-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

            int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];

            join_buffer.local_compute_output_size_rel[ra_id] = join_buffer.local_compute_output_size_rel[ra_id] + join_buffer.width[ra_id];
            join_buffer.local_compute_output_size_total = join_buffer.local_compute_output_size_total + join_buffer.width[ra_id];
            join_buffer.local_compute_output_size_flat[index*join_buffer.ra_count + ra_id] = join_buffer.local_compute_output_size_flat[index*join_buffer.ra_count + ra_id] + join_buffer.width[ra_id];
            join_buffer.local_compute_output_count_flat[index * join_buffer.ra_count + ra_id] ++;

            join_buffer.local_compute_output_size[ra_id][index] = join_buffer.local_compute_output_size[ra_id][index] + join_buffer.width[ra_id];
            join_buffer.cumulative_tuple_process_map[index] = join_buffer.cumulative_tuple_process_map[index] + join_buffer.width[ra_id];
            join_buffer.local_compute_output[ra_id][index].vector_buffer_append((const unsigned char*)tp.data(), sizeof(u64)*join_buffer.width[ra_id]);
            (*local_join_inserts)++;
            (*local_join_count)++;
        }
    } else {
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
            // std::cout << "add new facts ";
            // for (auto c: projected_path) {
            //     std::cout << c << " ";
            // }
            // std::cout << std::endl;
            if (deduplicate.insert_tuple_from_array(projected_path, join_buffer.width[ra_id]) != INSERT_FAIL)
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
    }
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
    bool canonical,
    bool generator_mode, join_generator_func_t gen_func)
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

    // std::cout << "cur tree >>> " << std::endl;
    // for (auto r:  ind) {
    //     std::cout << ">>> ";
    //     for (auto c: r) {
    //         std::cout << c << " ";
    //     }
    //     std::cout << std::endl;
    // }

    if (generator_mode) {
        std::vector<u64> input_t(input0_buffer, input0_buffer+input0_buffer_width);
        // std::cout << "Input >>>>>> ";
        // for (auto c: input_t) {
        //     std::cout << c << " ";
        // }
        // std::cout << std::endl;
        std::vector<std::vector<u64>> eq_tuple_set;
        std::vector<std::vector<u64>> generated_tuple_set;
        std::vector<u64> prev_non_dependent_columns;
        for(auto it = joined_range.first; it != joined_range.second && it != ind.end(); ++it){
            auto cur_path = *it;
            std::vector<u64> cur_non_dependent_columns(cur_path.begin(), cur_path.begin()+cur_path.size()-dependent_column_indices.size());
            // std::cout << " cur prefix >>>>>>> ";
            // for (auto c: cur_path) {
            //     std::cout << c << " ";
            // }
            // std::cout << std::endl;
            if (cur_non_dependent_columns == prev_non_dependent_columns) {
                eq_tuple_set.push_back(cur_path);
                continue;
            } else {
                if (eq_tuple_set.size() != 0) {
                    gen_func(eq_tuple_set, input_t, generated_tuple_set);
                    eq_tuple_set.clear();
                }
                prev_non_dependent_columns = cur_non_dependent_columns;
                eq_tuple_set.push_back(cur_path);
            }
        }
        if (eq_tuple_set.size() != 0) {
            gen_func(eq_tuple_set, input_t, generated_tuple_set);
        }
        for (auto& tp: generated_tuple_set) {
            uint64_t bucket_id = tuple_hash(tp.data(), head_rel_hash_col_count) % buckets;
            uint64_t sub_bucket_id=0;
            if (canonical == false)
                sub_bucket_id = tuple_hash(tp.data() + head_rel_hash_col_count, join_buffer.width[ra_id]-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

            int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];

            join_buffer.local_compute_output_size_rel[ra_id] = join_buffer.local_compute_output_size_rel[ra_id] + join_buffer.width[ra_id];
            join_buffer.local_compute_output_size_total = join_buffer.local_compute_output_size_total + join_buffer.width[ra_id];
            join_buffer.local_compute_output_size_flat[index*join_buffer.ra_count + ra_id] = join_buffer.local_compute_output_size_flat[index*join_buffer.ra_count + ra_id] + join_buffer.width[ra_id];
            join_buffer.local_compute_output_count_flat[index * join_buffer.ra_count + ra_id] ++;

            join_buffer.local_compute_output_size[ra_id][index] = join_buffer.local_compute_output_size[ra_id][index] + join_buffer.width[ra_id];
            join_buffer.cumulative_tuple_process_map[index] = join_buffer.cumulative_tuple_process_map[index] + join_buffer.width[ra_id];
            join_buffer.local_compute_output[ra_id][index].vector_buffer_append((const unsigned char*)tp.data(), sizeof(u64)*join_buffer.width[ra_id]);
            (*local_join_inserts)++;
            (*local_join_count)++;
        }
    } else {
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
            if (deduplicate.insert_tuple_from_array(projected_path, join_buffer.width[ra_id]) != INSERT_FAIL)
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
    }
    // std::cout << "inserted " << *local_join_inserts << std::endl;
}

void shmap_relation::as_all_to_allv_right_outer_join_buffer(
    shmap_relation* target_relation,
    u64 *input0_buffer, int input0_buffer_size, int input0_buffer_width,
    int *offset,
    all_to_allv_buffer &join_buffer,
    int ra_id, u32 buckets,
    u32 *output_sub_bucket_count,
    u32 **output_sub_bucket_rank,
    std::vector<int> &reorder_map,
    int join_column_count,
    int out_arity,
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
            if (canonical == false && out_arity != 0 && out_arity >= head_rel_hash_col_count)
                sub_bucket_id = tuple_hash(reordered_cur_path + head_rel_hash_col_count, out_arity-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

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
