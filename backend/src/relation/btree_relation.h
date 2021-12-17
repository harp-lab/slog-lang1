/* a normal btree version of relation
 *
 * Yihao Sun
 */

#pragma once

#include "shmap_relation.h"
#include <cstdint>
#include <iostream>
#include <souffle/datastructure/BTree.h>
#include <souffle/utility/Iteration.h>


struct btree_relation :public shmap_relation 
{
    // arity of this relation, later it should be a template argument
    // all code are encoded in u64 vector (refact to array later)
    using t_tuple = std::vector<uint64_t>;
    struct t_comparator {
        int operator()(const t_tuple &a, const t_tuple &b) const {
            // make it an unroll loop when change to array
            for (int i=0; i < a.size(); i++)
            {
                if (a[i] > b[i])
                    return 1;
                if (a[i] < b[i])
                    return -1;
            }
            return 0;
        }

        bool less(const t_tuple &a, const t_tuple &b) const {
            for (int i=0; i < a.size(); i++)
            {
                if (a[i] < b[i])
                    return true;
                if (a[i] > b[i])
                    return false;
            }
            return false;
        }

        bool equal(const t_tuple &a, const t_tuple &b) const
        {
            for (int i=0; i < a.size(); i++)
            {
                if (a[i] != b[i])
                    return false;
            }
            return true;
        }
    };

    // souffle use multi set for some relation
    using t_ind = souffle::btree_set<std::vector<uint64_t>, t_comparator>;
    t_ind ind;
    using iterator = t_ind::iterator;

    struct context {
        t_ind::operation_hints hints_0_lower;
        t_ind::operation_hints hints_0_upper;
    };
    
    context createContext() { return context(); }
    
    bool insert(const t_tuple &t) {
        // std::cout << "inserting ..." << std::endl;
        context h;
        return insert(t, h);
    }

    bool insert(const t_tuple &t, context &h) {
        if (ind.insert(t, h.hints_0_lower)) {
            return true;
        } else { 
            return false;
        }
    }

    // we also need some range fucntion maybe

    std::size_t size() const { return ind.size(); }

    bool contains(const t_tuple &t, context &h) const {
        return ind.contains(t, h.hints_0_lower);
    }

    bool contains(const t_tuple &t) const {
        context h;
        return contains(t, h);
    }


    iterator find(const t_tuple &t, context &h) const {
        return ind.find(t, h.hints_0_lower);
    }
    iterator find(const t_tuple &t) const {
        context h;
        return find(t, h);
    }

    bool empty() const { return ind.empty(); }

    // need partition?
    // std::vector<range<iterator>> partition() const {
    //     return int.getChunks(400);
    // }

    // I keep this weird  name from souffle, actually join helper fucntion
    // in souffle its index selection function, in slog we don't need select
    // so only one version of this function
    souffle::range<iterator> lowerUpperRange(
        const t_tuple &lower,
        const t_tuple &upper,
        context &h);

    souffle::range<iterator> lowerUpperRange( const t_tuple &lower, const t_tuple &upper)
    {
        context h;
        lowerUpperRange(lower, upper, h);
    }

    // souffle::range

    void purge() { ind.clear(); }

    iterator begin() const { return ind.begin(); }

    iterator end() const { return ind.end(); }

    void printStatistics(std::ostream &o) const {
        ind.printStats(o);
    }

    btree_relation(int arity);
    btree_relation() {};

    ~btree_relation() { purge(); }

    int count();
    bool insert_tuple_from_array(u64* t, int arity);
    void remove_tuple();
    bool find_tuple_from_array(u64* t, int arity);

    void as_vector_buffer_recursive(vector_buffer* vb, std::vector<u64> prefix);

    void as_all_to_allv_copy_buffer(all_to_allv_buffer& buffer, std::vector<u64> prefix, std::vector<int> reorder_map, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, u32 arity, u32 join_column_count, int head_rel_hash_col_count, bool canonical);
    
    void as_all_to_allv_copy_filter_buffer(all_to_allv_buffer& buffer, std::vector<u64> prefix, std::vector<int> reorder_map, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, u32 arity, u32 join_column_count, bool(*lambda)(const u64* const), int head_rel_hash_col_count, bool canonical);
    void as_all_to_allv_acopy_buffer(all_to_allv_buffer& buffer, std::vector<u64> prefix, std::vector<int> reorder_map, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, u32 arity, u32 join_column_count, int head_rel_hash_col_count, bool canonical);
    void as_all_to_allv_right_join_buffer(
        std::vector<u64> prefix, all_to_allv_buffer& join_buffer,
        u64 *input0_buffer, int input0_buffer_width,
        int input1_buffer_width, int ra_id,
        u32 buckets, u32* output_sub_bucket_count,
        u32** output_sub_bucket_rank, std::vector<int> reorder_map,
        int join_column_count, shmap_relation& deduplicate,
        int* local_join_count, u32* local_join_duplicates,
        u32* local_join_inserts,
        int head_rel_hash_col_count, bool canonical);

    void as_all_to_allv_left_join_buffer(
        std::vector<u64> prefix, all_to_allv_buffer& join_buffer,
        u64 *input0_buffer, int input0_buffer_width,
        int input1_buffer_width, int ra_id,
        u32 buckets, u32* output_sub_bucket_count,
        u32** output_sub_bucket_rank, std::vector<int> reorder_map,
        int join_column_count, shmap_relation& deduplicate,
        int* local_join_count, u32* local_join_duplicates,
        u32* local_join_inserts, int head_rel_hash_col_count,
        bool canonical);
    
    void as_all_to_allv_right_outer_join_buffer(
        // shmap_relation* neg_target,
        u64 *input0_buffer, int input0_buffer_size, int input0_buffer_width,
        int* offset,
        all_to_allv_buffer& join_buffer,
        int ra_id,
        u32 buckets, u32* output_sub_bucket_count,
        u32** output_sub_bucket_rank, std::vector<int>& reorder_map,
        int join_column_count, int out_airty,
        int head_rel_hash_col_count, bool canonical);

    void as_all_to_allv_copy_generate_buffer(all_to_allv_buffer& buffer, std::vector<u64> prefix, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, u32 arity, u32 join_column_count, int(*lambda)(const u64* const, u64* const), int head_rel_hash_col_count, bool canonical);
    
};
