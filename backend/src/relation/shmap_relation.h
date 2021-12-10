/*
 * Google's btree relation
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#pragma once

struct shmap_relation {

    shmap<shmap_relation*> next = {};

    int count();
    bool insert_tuple_from_array(u64* t, int arity);
    void remove_tuple();
    void remove_tuple_helper(shmap<void*> map);
    bool find_tuple_from_array(u64* t, int arity);

    void as_vector_buffer_recursive(vector_buffer* vb, std::vector<u64> prefix);
    void as_vector_buffer_recursive_helper(shmap_relation*& cur_trie, std::vector<u64>& cur_path, vector_buffer*& result_vector);

    void as_all_to_allv_copy_buffer(all_to_allv_buffer& buffer, std::vector<u64> prefix, std::vector<int> reorder_map, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, u32 arity, u32 join_column_count, int head_rel_hash_col_count, bool canonical);
    void as_all_to_allv_copy_buffer_helper(
        shmap_relation*& cur_trie, std::vector<u64>& cur_path,
        all_to_allv_buffer& buffer, int ra_id,
        u32 buckets, u32* output_sub_bucket_count,
        u32** output_sub_bucket_rank, std::vector<int> &reorder_map,
        u32 arity, u32 join_column_count,
        int head_rel_hash_col_count, bool canonical);
    void as_all_to_allv_copy_filter_buffer(all_to_allv_buffer& buffer, std::vector<u64> prefix, std::vector<int> reorder_map, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, u32 arity, u32 join_column_count, bool(*lambda)(const u64* const), int head_rel_hash_col_count, bool canonical);
    void as_all_to_allv_copy_filter_buffer_helper(shmap_relation*& cur_trie, std::vector<u64>& cur_path, all_to_allv_buffer& buffer, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int>& reorder_map, u32 arity, u32 join_column_count, bool(*lambda)(const u64* const), int head_rel_hash_col_count, bool canonical);
    void as_all_to_allv_acopy_buffer(all_to_allv_buffer& buffer, std::vector<u64> prefix, std::vector<int> reorder_map, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, u32 arity, u32 join_column_count, int head_rel_hash_col_count, bool canonical);
    void as_all_to_allv_acopy_buffer_helper(shmap_relation*& cur_trie, std::vector<u64>& cur_path, all_to_allv_buffer& buffer, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int>& reorder_map, u32 arity, u32 join_column_count, int head_rel_hash_col_count, bool canonical);
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
    void as_all_to_allv_right_join_buffer_helper(
        shmap_relation*& cur_trie, std::vector<u64>& cur_path,
        all_to_allv_buffer& join_buffer, u64 *input0_buffer,
        int input0_buffer_width, int input1_buffer_width,
        int ra_id, u32 buckets,
        u32* output_sub_bucket_count, u32** output_sub_bucket_rank,
        std::vector<int>& reorder_map, int join_column_count,
        shmap_relation& deduplicate, int* local_join_count,
        u32* local_join_duplicates, u32* local_join_inserts,
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
    void as_all_to_allv_left_join_buffer_helper(
        shmap_relation*& cur_trie, std::vector<u64>& cur_path,
        all_to_allv_buffer& join_buffer, u64 *input0_buffer,
        int input0_buffer_width, int input1_buffer_width,
        int ra_id, u32 buckets,
        u32* output_sub_bucket_count, u32** output_sub_bucket_rank,
        std::vector<int>& reorder_map, int join_column_count,
        shmap_relation& deduplicate, int* local_join_count,
        u32* local_join_duplicates, u32* local_join_inserts,
        int head_rel_hash_col_count, bool canonical);
    void as_all_to_allv_right_outer_join_buffer(
        shmap_relation* neg_target, all_to_allv_buffer& join_buffer,
        int ra_id,
        u32 buckets, u32* output_sub_bucket_count,
        u32** output_sub_bucket_rank, std::vector<int>& reorder_map,
        int join_column_count, int out_airty,
        int head_rel_hash_col_count, bool canonical);
    void as_all_to_allv_right_outer_join_buffer_helper(
        shmap_relation* neg_target, shmap_relation*& cur_trie,
        std::vector<u64>& cur_path, all_to_allv_buffer& join_buffer,
        int ra_id,
        u32 buckets, u32* output_sub_bucket_count,
        u32** output_sub_bucket_rank, std::vector<int>& reorder_map,
        int join_column_count, int out_arity,
        int head_rel_hash_col_count, bool canonical);

    void as_all_to_allv_copy_generate_buffer(all_to_allv_buffer& buffer, std::vector<u64> prefix, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, u32 arity, u32 join_column_count, int(*lambda)(const u64* const, u64* const), int head_rel_hash_col_count, bool canonical);
    void as_all_to_allv_copy_generate_buffer_helper(shmap_relation*& cur_trie, std::vector<u64>& cur_path, all_to_allv_buffer& buffer, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, u32 arity, u32 join_column_count, int(*lambda)(const u64* const, u64* const), int head_rel_hash_col_count, bool canonical);

};
