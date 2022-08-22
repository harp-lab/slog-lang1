/*
 * base class for relation storage
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#pragma once

#include "../parallel_RA_inc.h"
#include "../btree/btree_set.h"
#include <cstdint>
#include <utility>

struct shmap_relation {

    int arity;

    int data_structure_type;

    int id_flag;    // does this btree contain id column?

    using t_tuple = std::vector<uint64_t>;
    struct t_comparator {
        // 0-arity compare will fail
        t_comparator() : _id_flag(true) {}

        t_comparator(bool id_flag) : _id_flag(id_flag) {} 
        
        bool operator()(const t_tuple &a, const t_tuple &b) const {
            // make it an unroll loop when change to array
            int size = a.size();
            // if (_id_flag) {
            //     size--;
            // }
            for (int i=0; i < size; i++)
            {
                if (a[i] < b[i])
                    return true;
                if (a[i] > b[i])
                    return false;
            }
            return false;
        }
        bool _id_flag;
    };

    // souffle use multi set for some relation
    using t_ind = btree::btree_set<t_tuple, t_comparator>;
    t_ind* ind;
    using iterator = t_ind::iterator;

    bool insert(const t_tuple &t) {
        return ind->insert(t).second;
    }

    std::size_t size() const { return ind->size(); }

    bool contains(const t_tuple &t) const {
        auto res = ind->find(t);
        return res != ind->end();
    }

    iterator find(const t_tuple &t) const {
        return ind->find(t);
    }

    bool empty() const { return ind->empty(); }

    // I keep this weird  name from souffle, actually join helper fucntion
    // in souffle its index selection function, in slog we don't need select
    // so only one version of this function
    std::pair<iterator, iterator> lowerUpperRange(const t_tuple &lower, const t_tuple &upper) const
    {
        auto lower_it = ind->lower_bound(lower);
        auto upper_it = ind->upper_bound(upper);
        if (lower_it == ind->end()) {
            return std::make_pair(ind->end(), ind->end());
        }
        if (upper_it == ind->end()) {
            return std::make_pair(lower_it, upper_it);
        }
        auto lower_v = *lower_it;
        auto upper_v = *upper_it;
        int valid = 0;
        for (int i = 0; i < lower_v.size(); i++) {
            if (lower_v[i] > upper_v[i]) {
                valid = -1;
                break;
            }
            if (lower_v[i] < upper_v[i]) {
                valid = 1;
                break;
            }
        }
        if (valid == 1) {
            return std::make_pair(lower_it, upper_it);
        }
        if (valid == 0) {
            if (ind->find(lower) != ind->end()) {
                return std::make_pair(lower_it, lower_it);
            }
        }
        return std::make_pair(ind->end(), ind->end());
    }


    void purge() { ind->clear(); }

    iterator begin() const { return ind->begin(); }

    iterator end() const { return ind->end(); }

    shmap_relation(int arity, bool id_flag);
    shmap_relation() {
        id_flag = true;
        ind = new t_ind(t_comparator(id_flag));
        // int rank;
        // MPI_Comm_rank(MPI_COMM_WORLD, &rank);
        // std::cout << "default constructor " << rank <<std::endl;
    };

    int count();
    bool insert_tuple_from_array(uint64_t* t, int arity);
    void remove_tuple();
    bool find_tuple_from_array(uint64_t* t, int arity);

    void as_vector_buffer_recursive(vector_buffer* vb, std::vector<uint64_t> prefix);

    void as_all_to_allv_copy_buffer(all_to_allv_buffer& buffer, std::vector<uint64_t> prefix, std::vector<int> reorder_map, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, u32 arity, u32 join_column_count, int head_rel_hash_col_count, bool canonical);
    
    void as_all_to_allv_copy_filter_buffer(all_to_allv_buffer& buffer, std::vector<uint64_t> prefix, std::vector<int> reorder_map, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, u32 arity, u32 join_column_count, bool(*lambda)(const uint64_t* const), int head_rel_hash_col_count, bool canonical);
    void as_all_to_allv_acopy_buffer(all_to_allv_buffer& buffer, std::vector<uint64_t> prefix, std::vector<int> reorder_map, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, u32 arity, u32 join_column_count, int head_rel_hash_col_count, bool canonical);
    void as_all_to_allv_right_join_buffer(
        std::vector<uint64_t> prefix, all_to_allv_buffer& join_buffer,
        uint64_t *input0_buffer, int input0_buffer_width,
        int input1_buffer_width, int ra_id,
        u32 buckets, u32* output_sub_bucket_count,
        u32** output_sub_bucket_rank, std::vector<int> reorder_map,
        int join_column_count, shmap_relation& deduplicate,
        int* local_join_count, u32* local_join_duplicates,
        u32* local_join_inserts,
        int head_rel_hash_col_count, bool canonical);

    void as_all_to_allv_left_join_buffer(
        std::vector<uint64_t> prefix, all_to_allv_buffer& join_buffer,
        uint64_t *input0_buffer, int input0_buffer_width,
        int input1_buffer_width, int ra_id,
        u32 buckets, u32* output_sub_bucket_count,
        u32** output_sub_bucket_rank, std::vector<int> reorder_map,
        int join_column_count, shmap_relation& deduplicate,
        int* local_join_count, u32* local_join_duplicates,
        u32* local_join_inserts, int head_rel_hash_col_count,
        bool canonical);
    
    void as_all_to_allv_right_outer_join_buffer(
        // shmap_relation* neg_target,
        uint64_t *input0_buffer, int input0_buffer_size, int input0_buffer_width,
        int* offset,
        all_to_allv_buffer& join_buffer,
        int ra_id,
        u32 buckets, u32* output_sub_bucket_count,
        u32** output_sub_bucket_rank, std::vector<int>& reorder_map,
        int join_column_count, int out_airty,
        int head_rel_hash_col_count, bool canonical);

    void as_all_to_allv_copy_generate_buffer(all_to_allv_buffer& buffer, std::vector<uint64_t> prefix, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, u32 arity, u32 join_column_count, int(*lambda)(const uint64_t* const, uint64_t* const), int head_rel_hash_col_count, bool canonical);
 
    ~shmap_relation()
    {
        ind->clear();
        delete ind;
    }

private:
    shmap_relation(const shmap_relation& other);
    shmap_relation& operator=(const shmap_relation& other);
};
