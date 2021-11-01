/*
 * Google's btree relation
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#include "../parallel_RA_inc.h"


bool shmap_relation::insert_tuple_from_array(u64* t, int arity)
{
    bool counter = false;
    shmap_relation *node = this;
    for (int i = 0; i < arity; i++)
    {
        if (node->next.find(t[i]) == NULL)
        {
            node->next.insert(t[i], new shmap_relation());
            counter = true;
        }
        node = *((node->next).find(t[i]));
    }
    if (counter == true)
        node->is_end = true;
    return counter;
}



void shmap_relation::remove_tuple()
{
    shmap_relation *node = this;
    for (auto nxt = node->next.begin(); nxt; nxt.next()){
        shmap_relation *nxt_trie = nxt.val();
        nxt_trie->remove_tuple();
        delete nxt_trie;
    }
    node->next.clear();
}



bool shmap_relation::find_tuple_from_array(u64* t, int arity)
{
    shmap_relation *node = this;
    for (int i = 0; i < arity; i++)
    {
        auto it = (node->next.find(t[i]));
        if (it == NULL)
            return false;
        node = *it;
    }
    return true;
}



void shmap_relation::as_vector_buffer_recursive(vector_buffer* vb, std::vector<u64> prefix)
{

    shmap_relation *m_trie = this;
    for (u64 n : prefix)
    {
        if (m_trie->next.find(n) == NULL)
            return;
        m_trie = *(m_trie->next.find(n));
    }

    as_vector_buffer_recursive_helper(m_trie, prefix, vb);
}



void shmap_relation::as_vector_buffer_recursive_helper(shmap_relation*& cur_trie, std::vector<u64> cur_path, vector_buffer*& result_vector)
{
    if(cur_trie->is_end)
    {
        u64 path[cur_path.size()];
        for (u32 i = 0; i < cur_path.size(); i++)
            path[i] = cur_path[i];
        result_vector->vector_buffer_append((const unsigned char*)path, sizeof(u64)*cur_path.size());
    }

    for (auto nxt = cur_trie->next.begin(); nxt; nxt.next())
    {
        u64 nxt_node = nxt.key();
        shmap_relation *nxt_trie = nxt.val();
        cur_path.push_back(nxt_node);
        as_vector_buffer_recursive_helper(nxt_trie, cur_path, result_vector);
        cur_path.pop_back();
    }
}



void shmap_relation::as_all_to_allv_acopy_buffer(all_to_allv_buffer& buffer, std::vector<u64> prefix, std::vector<int> reorder_map, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, u32 arity, u32 join_column_count, int head_rel_hash_col_count, bool canonical)
{
    shmap_relation *m_trie = this;
    for (u64 n : prefix)
    {
        if (m_trie->next.find(n)==NULL)
            return;
        m_trie = *(m_trie->next.find(n));
    }
    as_all_to_allv_acopy_buffer_helper(m_trie, prefix, buffer, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, arity, join_column_count, head_rel_hash_col_count, canonical);
}



void shmap_relation::as_all_to_allv_acopy_buffer_helper(shmap_relation*& cur_trie, std::vector<u64> cur_path, all_to_allv_buffer& buffer, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int> reorder_map, u32 arity, u32 join_column_count, int head_rel_hash_col_count, bool canonical)
{
    if(cur_trie->is_end)
    {
        u64 reordered_cur_path[buffer.width[ra_id]];
        for (int j =0; j < buffer.width[ra_id]; j++)
            reordered_cur_path[j] = cur_path[reorder_map[j]];

        uint64_t bucket_id = tuple_hash(reordered_cur_path, head_rel_hash_col_count) % buckets;
        uint64_t sub_bucket_id=0;
        if (canonical == false)
            sub_bucket_id = tuple_hash(reordered_cur_path + head_rel_hash_col_count, arity-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

        int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];

        buffer.local_compute_output_size_total = buffer.local_compute_output_size_total + buffer.width[ra_id];
        buffer.local_compute_output_size_flat[index * buffer.ra_count + ra_id] = buffer.local_compute_output_size_flat[index * buffer.ra_count + ra_id] + buffer.width[ra_id];
        buffer.local_compute_output_size[ra_id][index] = buffer.local_compute_output_size[ra_id][index] + buffer.width[ra_id];
        buffer.cumulative_tuple_process_map[index] = buffer.cumulative_tuple_process_map[index] + buffer.width[ra_id];
        buffer.local_compute_output[ra_id][index].vector_buffer_append((const unsigned char*)reordered_cur_path, sizeof(u64)*buffer.width[ra_id]);
    }

    for (auto nxt = cur_trie->next.begin(); nxt; nxt.next())
    {
        u64 nxt_node = nxt.key();
        shmap_relation *nxt_trie = nxt.val();
        cur_path.push_back(nxt_node);
        as_all_to_allv_acopy_buffer_helper(nxt_trie, cur_path, buffer, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, arity, join_column_count, head_rel_hash_col_count, canonical);
        cur_path.pop_back();
    }
}



void shmap_relation::as_all_to_allv_copy_buffer(all_to_allv_buffer& buffer, std::vector<u64> prefix, std::vector<int> reorder_map, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, u32 arity, u32 join_column_count, int head_rel_hash_col_count, bool canonical)
{
    shmap_relation *m_trie = this;
    for (u64 n : prefix)
    {
        if (m_trie->next.find(n)==NULL)
            return;
        m_trie = *(m_trie->next.find(n));
    }
    as_all_to_allv_copy_buffer_helper(m_trie, prefix, buffer, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, arity, join_column_count, head_rel_hash_col_count, canonical);
}



void shmap_relation::as_all_to_allv_copy_buffer_helper(shmap_relation*& cur_trie, std::vector<u64> cur_path, all_to_allv_buffer& buffer, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int> reorder_map, u32 arity, u32 join_column_count, int head_rel_hash_col_count, bool canonical)
{
    if(cur_trie->is_end)
    {
        u64 reordered_cur_path[buffer.width[ra_id]];
        for (u32 j =0; j < reorder_map.size(); j++)
            reordered_cur_path[j] = cur_path[reorder_map[j]];

        uint64_t bucket_id = tuple_hash(reordered_cur_path, head_rel_hash_col_count) % buckets;
        uint64_t sub_bucket_id=0;
        if (canonical == false)
            sub_bucket_id = tuple_hash(reordered_cur_path + head_rel_hash_col_count, arity-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

        int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];
        buffer.local_compute_output_size_total = buffer.local_compute_output_size_total + buffer.width[ra_id];
        buffer.local_compute_output_size_flat[index * buffer.ra_count + ra_id] = buffer.local_compute_output_size_flat[index * buffer.ra_count + ra_id] + buffer.width[ra_id];

        buffer.local_compute_output_size[ra_id][index] = buffer.local_compute_output_size[ra_id][index] + buffer.width[ra_id];
        buffer.cumulative_tuple_process_map[index] = buffer.cumulative_tuple_process_map[index] + buffer.width[ra_id];
        buffer.local_compute_output[ra_id][index].vector_buffer_append((const unsigned char*)reordered_cur_path, sizeof(u64)*buffer.width[ra_id]);
    }

    for (auto nxt = cur_trie->next.begin(); nxt; nxt.next())
    {
        u64 nxt_node = nxt.key();
        shmap_relation *nxt_trie = nxt.val();
        cur_path.push_back(nxt_node);
        as_all_to_allv_copy_buffer_helper(nxt_trie, cur_path, buffer, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, arity, join_column_count, head_rel_hash_col_count, canonical);
        cur_path.pop_back();
    }
}


void shmap_relation::as_all_to_allv_copy_filter_buffer(all_to_allv_buffer& buffer, std::vector<u64> prefix, std::vector<int> reorder_map, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, u32 arity, u32 join_column_count, bool(*lambda)(const u64* const), int head_rel_hash_col_count, bool canonical)
{
    shmap_relation *m_trie = this;
    for (u64 n : prefix)
    {
        if (m_trie->next.find(n)==NULL)
            return;
        m_trie = *(m_trie->next.find(n));
    }
    as_all_to_allv_copy_filter_buffer_helper(m_trie, prefix, buffer, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, arity, join_column_count, lambda, head_rel_hash_col_count, canonical);
}



void shmap_relation::as_all_to_allv_copy_filter_buffer_helper(shmap_relation*& cur_trie, std::vector<u64> cur_path, all_to_allv_buffer& buffer, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int> reorder_map, u32 arity, u32 join_column_count, bool(*lambda)(const u64* const), int head_rel_hash_col_count, bool canonical)
{
    if(cur_trie->is_end)
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
            if (canonical == false)
                sub_bucket_id = tuple_hash(reordered_cur_path + head_rel_hash_col_count, arity-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

            int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];
            buffer.local_compute_output_size_total = buffer.local_compute_output_size_total + buffer.width[ra_id];
            buffer.local_compute_output_size_flat[index * buffer.ra_count + ra_id] = buffer.local_compute_output_size_flat[index * buffer.ra_count + ra_id] + buffer.width[ra_id];

            buffer.local_compute_output_size[ra_id][index] = buffer.local_compute_output_size[ra_id][index] + buffer.width[ra_id];
            buffer.cumulative_tuple_process_map[index] = buffer.cumulative_tuple_process_map[index] + buffer.width[ra_id];
            buffer.local_compute_output[ra_id][index].vector_buffer_append((const unsigned char*)reordered_cur_path, sizeof(u64)*buffer.width[ra_id]);
        }
    }

    for (auto nxt = cur_trie->next.begin(); nxt; nxt.next())
    {
        u64 nxt_node = nxt.key();
        shmap_relation *nxt_trie = nxt.val();
        cur_path.push_back(nxt_node);
        as_all_to_allv_copy_filter_buffer_helper(nxt_trie, cur_path, buffer, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, arity, join_column_count, lambda, head_rel_hash_col_count, canonical);
        cur_path.pop_back();
    }
}



void shmap_relation::as_all_to_allv_right_join_buffer(std::vector<u64> prefix, all_to_allv_buffer& join_buffer, u64 *input0_buffer, int input0_buffer_width, int input1_buffer_width, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int> reorder_map, int join_column_count, shmap_relation& deduplicate, int *local_join_count, u32* local_join_duplicates, u32* local_join_inserts, std::string name, int head_rel_hash_col_count, bool canonical)
{
    shmap_relation *m_trie = this;
    for (u64 n : prefix)  {
        if (m_trie->next.find(n)==NULL)
            return;
        m_trie = *(m_trie->next.find(n));
    }

    as_all_to_allv_right_join_buffer_helper(m_trie, prefix, join_buffer, input0_buffer, input0_buffer_width, input1_buffer_width, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, join_column_count, deduplicate, local_join_count, local_join_duplicates, local_join_inserts, name, head_rel_hash_col_count, canonical);
}



void shmap_relation::as_all_to_allv_right_join_buffer_helper(shmap_relation*& cur_trie, std::vector<u64> cur_path, all_to_allv_buffer& join_buffer, u64 *input0_buffer, int input0_buffer_width, int input1_buffer_width, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int> reorder_map, int join_column_count, shmap_relation& deduplicate, int *local_join_count, u32* local_join_duplicates, u32* local_join_inserts, std::string name, int head_rel_hash_col_count, bool canonical)
{
    if(cur_trie->is_end)
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

    for (auto nxt = cur_trie->next.begin(); nxt; nxt.next())
    {

        u64 nxt_node = nxt.key();
        shmap_relation *nxt_trie = nxt.val();
        cur_path.push_back(nxt_node);
        as_all_to_allv_right_join_buffer_helper(nxt_trie, cur_path, join_buffer, input0_buffer, input0_buffer_width, input1_buffer_width, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, join_column_count, deduplicate, local_join_count, local_join_duplicates, local_join_inserts, name, head_rel_hash_col_count, canonical);
        cur_path.pop_back();
    }
}


void shmap_relation::as_all_to_allv_left_outer_join_buffer(
    std::vector<u64> prefix, all_to_allv_buffer& join_buffer, u64 *input0_buffer,
    int input0_buffer_width, int input1_buffer_width, int ra_id, u32 buckets,
    u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int> reorder_map,
    int join_column_count, shmap_relation& deduplicate, int* local_join_count,
    u32* local_join_duplicates, u32* local_join_inserts, int head_rel_hash_col_count, bool canonical)
{
    shmap_relation *m_trie = this;
    for (u64 n : prefix) {
        if (m_trie->next.find(n) != NULL)
            return
    }
    as_all_to_allv_left_join_buffer_helper(m_trie, prefix, join_buffer, input0_buffer, input0_buffer_width, input1_buffer_width, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, join_column_count, deduplicate, local_join_count, local_join_duplicates, local_join_inserts, head_rel_hash_col_count, canonical);
}


void shmap_relation::as_all_to_allv_left_join_buffer(std::vector<u64> prefix, all_to_allv_buffer& join_buffer, u64 *input0_buffer, int input0_buffer_width, int input1_buffer_width, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int> reorder_map, int join_column_count, shmap_relation& deduplicate, int* local_join_count, u32* local_join_duplicates, u32* local_join_inserts, int head_rel_hash_col_count, bool canonical)
{
    shmap_relation *m_trie = this;
    for (u64 n : prefix)  {
        if (m_trie->next.find(n)==NULL)
            return;
        m_trie = *(m_trie->next.find(n));
    }

    as_all_to_allv_left_join_buffer_helper(m_trie, prefix, join_buffer, input0_buffer, input0_buffer_width, input1_buffer_width, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, join_column_count, deduplicate, local_join_count, local_join_duplicates, local_join_inserts, head_rel_hash_col_count, canonical);
}



void shmap_relation::as_all_to_allv_left_join_buffer_helper(shmap_relation*& cur_trie, std::vector<u64> cur_path, all_to_allv_buffer& join_buffer, u64 *input0_buffer, int input0_buffer_width, int input1_buffer_width, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int> reorder_map, int join_column_count, shmap_relation& deduplicate, int* local_join_count, u32* local_join_duplicates, u32* local_join_inserts, int head_rel_hash_col_count, bool canonical)
{
    if(cur_trie->is_end)
    {
        u64 projected_path[join_buffer.width[ra_id]];
        u64 reordered_cur_path[input0_buffer_width + input1_buffer_width - join_column_count];
        for (int i = 0; i < input0_buffer_width; i++)
            reordered_cur_path[i] = input0_buffer[i];

        for (int i = join_column_count; i < input1_buffer_width; i++)
            reordered_cur_path[input0_buffer_width + (i - join_column_count)] = cur_path[i];

        for (int i =0; i < join_buffer.width[ra_id]; i++)
            projected_path[i] = reordered_cur_path[reorder_map[i]];

        if (deduplicate.insert_tuple_from_array(projected_path, join_buffer.width[ra_id]) == true)
        {
            uint64_t bucket_id = tuple_hash(projected_path, head_rel_hash_col_count) % buckets;
            uint64_t sub_bucket_id=0;
            if (canonical == false)
                sub_bucket_id = tuple_hash(projected_path + head_rel_hash_col_count, join_buffer.width[ra_id]-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

            int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];

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

    for (auto nxt = cur_trie->next.begin(); nxt; nxt.next())
    {
        u64 nxt_node = nxt.key();
        shmap_relation *nxt_trie = nxt.val();
        cur_path.push_back(nxt_node);
        as_all_to_allv_left_join_buffer_helper(nxt_trie, cur_path, join_buffer, input0_buffer, input0_buffer_width, input1_buffer_width, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, join_column_count, deduplicate, local_join_count, local_join_duplicates, local_join_inserts, head_rel_hash_col_count, canonical);
        cur_path.pop_back();
    }
}



void shmap_relation::as_all_to_all_copy_buffer(int threshold, int RA_count, all_to_all_buffer& buffer, std::vector<u64> prefix, std::vector<int> reorder_map, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, u32 arity, u32 join_column_count, int head_rel_hash_col_count, bool canonical)
{
    shmap_relation *m_trie = this;
    for (u64 n : prefix)
    {
        if (m_trie->next.find(n)==NULL)
            return;
        m_trie = *(m_trie->next.find(n));
    }
    as_all_to_all_copy_buffer_helper(threshold, RA_count, m_trie, prefix, buffer, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, arity, join_column_count, head_rel_hash_col_count, canonical);
}



void shmap_relation::as_all_to_all_copy_buffer_helper(int threshold, int RA_count, shmap_relation*& cur_trie, std::vector<u64> cur_path, all_to_all_buffer& buffer, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int> reorder_map, u32 arity, u32 join_column_count, int head_rel_hash_col_count, bool canonical)
{
    if(cur_trie->is_end)
    {
        u64 reordered_cur_path[buffer.width[ra_id]];
        for (u32 j =0; j < reorder_map.size(); j++)
            reordered_cur_path[j] = cur_path[reorder_map[j]];

        uint64_t bucket_id = tuple_hash(reordered_cur_path, head_rel_hash_col_count) % buckets;
        uint64_t sub_bucket_id=0;
        if (canonical == false)
            sub_bucket_id = tuple_hash(reordered_cur_path + head_rel_hash_col_count, arity-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

        int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];
        memcpy (buffer.local_compute_output + (index * (RA_count * threshold + RA_count)) + (RA_count + threshold * ra_id) + buffer.local_compute_output_size[ra_id][index], (const unsigned char*)reordered_cur_path, sizeof(u64)*buffer.width[ra_id]);
        buffer.local_compute_output_size[ra_id][index] = buffer.local_compute_output_size[ra_id][index] + buffer.width[ra_id];
        buffer.local_compute_output[(index * (RA_count * threshold + RA_count)) + ra_id] = buffer.local_compute_output[(index * (RA_count * threshold + RA_count)) + ra_id] + buffer.width[ra_id];
    }

    // for (auto nxt = cur_trie->next.begin(); nxt; nxt.next())
    // for ( auto nxt = cur_trie->next.begin(); nxt!= cur_trie->next.end(); ++nxt )
    for (auto nxt = cur_trie->next.begin(); nxt; nxt.next())
    {
        u64 nxt_node = nxt.key();
        shmap_relation *nxt_trie = nxt.val();
        cur_path.push_back(nxt_node);
        as_all_to_all_copy_buffer_helper(threshold, RA_count, nxt_trie, cur_path, buffer, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, arity, join_column_count, head_rel_hash_col_count, canonical);
        cur_path.pop_back();
    }
}


void shmap_relation::as_all_to_all_copy_buffer_with_starting_index(int threshold, int RA_count, all_to_all_buffer& buffer, std::vector<u64> prefix, std::vector<int> reorder_map, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, u32 arity, u32 join_column_count, int head_rel_hash_col_count, bool canonical)
{
    shmap_relation *m_trie = this;
    for (u64 n : prefix)
    {
        if (m_trie->next.find(n)==NULL)
            return;
        m_trie = *(m_trie->next.find(n));
    }
    as_all_to_all_copy_buffer_helper_with_starting_index(threshold, RA_count, m_trie, prefix, buffer, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, arity, join_column_count, head_rel_hash_col_count, canonical);


    for (auto nxt = m_trie->next.begin(); nxt; nxt.next())
    {
        std::vector<u64> prefix1;
        prefix1.push_back(nxt.key());
        u64 nxt_node = nxt.key();
        shmap_relation *temp_trie = *(m_trie->next.find(nxt_node));
        as_all_to_all_copy_buffer_helper_with_starting_index(threshold, RA_count, temp_trie, prefix1, buffer, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, arity, join_column_count, head_rel_hash_col_count, canonical);
    }
}



void shmap_relation::as_all_to_all_copy_buffer_helper_with_starting_index(int threshold, int RA_count, shmap_relation*& cur_trie, std::vector<u64> cur_path, all_to_all_buffer& buffer, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int> reorder_map, u32 arity, u32 join_column_count, int head_rel_hash_col_count, bool canonical)
{
    if(cur_trie->is_end)
    {
        u64 reordered_cur_path[buffer.width[ra_id]];
        for (u32 j =0; j < reorder_map.size(); j++)
            reordered_cur_path[j] = cur_path[reorder_map[j]];

        uint64_t bucket_id = tuple_hash(reordered_cur_path, head_rel_hash_col_count) % buckets;
        uint64_t sub_bucket_id=0;
        if (canonical == false)
            sub_bucket_id = tuple_hash(reordered_cur_path + head_rel_hash_col_count, arity-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

        int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];
        memcpy (buffer.local_compute_output + (index * (RA_count * threshold + RA_count)) + (RA_count + threshold * ra_id) + buffer.local_compute_output_size[ra_id][index], (const unsigned char*)reordered_cur_path, sizeof(u64)*buffer.width[ra_id]);
        buffer.local_compute_output_size[ra_id][index] = buffer.local_compute_output_size[ra_id][index] + buffer.width[ra_id];
        buffer.local_compute_output[(index * (RA_count * threshold + RA_count)) + ra_id] = buffer.local_compute_output[(index * (RA_count * threshold + RA_count)) + ra_id] + buffer.width[ra_id];
    }

    // for (auto nxt = cur_trie->next.begin(); nxt; nxt.next())
    //for ( auto nxt = cur_trie->next.begin(); nxt!= cur_trie->next.end(); ++nxt )
    for (auto nxt = cur_trie->next.begin(); nxt; nxt.next())
    {
        u64 nxt_node = nxt.key();
        shmap_relation *nxt_trie = nxt.val();
        cur_path.push_back(nxt_node);
        as_all_to_all_copy_buffer_helper_with_starting_index(threshold, RA_count, nxt_trie, cur_path, buffer, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, arity, join_column_count, head_rel_hash_col_count, canonical);
        cur_path.pop_back();
    }
}


void shmap_relation::as_all_to_all_copy_filter_buffer(int threshold, int RA_count, all_to_all_buffer& buffer, std::vector<u64> prefix, std::vector<int> reorder_map, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, u32 arity, u32 join_column_count, bool(*lambda)(const u64* const), int head_rel_hash_col_count, bool canonical)
{
    shmap_relation *m_trie = this;
    for (u64 n : prefix)
    {
        if (m_trie->next.find(n)==NULL)
            return;
        m_trie = *(m_trie->next.find(n));
    }
    as_all_to_all_copy_filter_buffer_helper(threshold, RA_count, m_trie, prefix, buffer, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, arity, join_column_count, lambda, head_rel_hash_col_count, canonical);
}



void shmap_relation::as_all_to_all_copy_filter_buffer_helper(int threshold, int RA_count, shmap_relation*& cur_trie, std::vector<u64> cur_path, all_to_all_buffer& buffer, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int> reorder_map, u32 arity, u32 join_column_count, bool(*lambda)(const u64* const), int head_rel_hash_col_count, bool canonical)
{
    if(cur_trie->is_end)
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
            if (canonical == false)
                sub_bucket_id = tuple_hash(reordered_cur_path + head_rel_hash_col_count, arity-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

            int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];
            memcpy (buffer.local_compute_output + (index * (RA_count * threshold + RA_count)) + (RA_count + threshold * ra_id) + buffer.local_compute_output_size[ra_id][index], (const unsigned char*)reordered_cur_path, sizeof(u64)*buffer.width[ra_id]);
            buffer.local_compute_output_size[ra_id][index] = buffer.local_compute_output_size[ra_id][index] + buffer.width[ra_id];
            buffer.local_compute_output[(index * (RA_count * threshold + RA_count)) + ra_id] = buffer.local_compute_output[(index * (RA_count * threshold + RA_count)) + ra_id] + buffer.width[ra_id];
        }
    }

    for (auto nxt = cur_trie->next.begin(); nxt; nxt.next())
    {
        u64 nxt_node = nxt.key();
        shmap_relation *nxt_trie = nxt.val();
        cur_path.push_back(nxt_node);
        as_all_to_all_copy_filter_buffer_helper(threshold, RA_count, nxt_trie, cur_path, buffer, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, arity, join_column_count, lambda, head_rel_hash_col_count, canonical);
        cur_path.pop_back();
    }
}


void shmap_relation::as_all_to_all_acopy_buffer(int threshold, int RA_count, all_to_all_buffer& buffer, std::vector<u64> prefix, std::vector<int> reorder_map, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, u32 arity, u32 join_column_count, int head_rel_hash_col_count, bool canonical)
{
    shmap_relation *m_trie = this;
    for (u64 n : prefix)
    {
        if (m_trie->next.find(n)==NULL)
            return;
        m_trie = *(m_trie->next.find(n));
    }
    as_all_to_all_acopy_buffer_helper(threshold, RA_count, m_trie, prefix, buffer, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, arity, join_column_count, head_rel_hash_col_count, canonical);
}

void shmap_relation::as_all_to_all_acopy_buffer_helper(int threshold, int RA_count, shmap_relation*& cur_trie, std::vector<u64> cur_path, all_to_all_buffer& buffer, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int> reorder_map, u32 arity, u32 join_column_count, int head_rel_hash_col_count, bool canonical)
{
    if(cur_trie->is_end)
    {
        u64 reordered_cur_path[buffer.width[ra_id]];
        for (int j =0; j < buffer.width[ra_id]; j++)
            reordered_cur_path[j] = cur_path[reorder_map[j]];

        uint64_t bucket_id = tuple_hash(reordered_cur_path, head_rel_hash_col_count) % buckets;
        uint64_t sub_bucket_id=0;
        if (canonical == false)
            sub_bucket_id = tuple_hash(reordered_cur_path + head_rel_hash_col_count, arity-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

        int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];


        //std::cout << "Index " << (index * (RA_count * threshold + RA_count)) + (RA_count + threshold * ra_id) + buffer.local_compute_output_size[ra_id][index] << std::endl;
        memcpy (buffer.local_compute_output + (index * (RA_count * threshold + RA_count)) + (RA_count + threshold * ra_id) + buffer.local_compute_output_size[ra_id][index], (const unsigned char*)reordered_cur_path, sizeof(u64)*buffer.width[ra_id]);

        buffer.local_compute_output_size[ra_id][index] = buffer.local_compute_output_size[ra_id][index] + buffer.width[ra_id];

        buffer.local_compute_output[(index * (RA_count * threshold + RA_count)) + ra_id] = buffer.local_compute_output[(index * (RA_count * threshold + RA_count)) + ra_id] + buffer.width[ra_id];

    }

    for (auto nxt = cur_trie->next.begin(); nxt; nxt.next())
    {
        u64 nxt_node = nxt.key();
        shmap_relation *nxt_trie = nxt.val();
        cur_path.push_back(nxt_node);
        as_all_to_all_acopy_buffer_helper(threshold, RA_count, nxt_trie, cur_path, buffer, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, arity, join_column_count, head_rel_hash_col_count, canonical);
        cur_path.pop_back();
    }
}



void shmap_relation::as_all_to_all_right_join_buffer(int threshold, int RA_count, std::vector<u64> prefix, all_to_all_buffer& join_buffer, u64 *input0_buffer, int input0_buffer_width, int input1_buffer_width, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int> reorder_map, int join_column_count, shmap_relation& deduplicate, int *local_join_count, u32* local_join_duplicates, u32* local_join_inserts, std::string name, int head_rel_hash_col_count, bool canonical, u32* tracker)
{
    shmap_relation *m_trie = this;
    for (u64 n : prefix)  {
        if (m_trie->next.find(n)==NULL)
            return;
        m_trie = *(m_trie->next.find(n));
    }

    as_all_to_all_right_join_buffer_helper(threshold, RA_count, m_trie, prefix, join_buffer, input0_buffer, input0_buffer_width, input1_buffer_width, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, join_column_count, deduplicate, local_join_count, local_join_duplicates, local_join_inserts, name, head_rel_hash_col_count, canonical, tracker);
}



void shmap_relation::as_all_to_all_right_join_buffer_helper(int threshold, int RA_count, shmap_relation*& cur_trie, std::vector<u64> cur_path, all_to_all_buffer& join_buffer, u64 *input0_buffer, int input0_buffer_width, int input1_buffer_width, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int> reorder_map, int join_column_count, shmap_relation& deduplicate, int *local_join_count, u32* local_join_duplicates, u32* local_join_inserts, std::string name, int head_rel_hash_col_count, bool canonical, u32* tracker)
{
    if(cur_trie->is_end)
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

            if ((int)join_buffer.local_compute_output[(index * (RA_count * threshold + RA_count)) + ra_id] + join_buffer.width[ra_id] >= threshold || *tracker == 1)
            {
                *tracker = *tracker | 1;
                return;
            }


            //std::cout << "index " << (index * (RA_count * threshold + RA_count)) + (RA_count + threshold * ra_id) + join_buffer.local_compute_output_size[ra_id][index] << std::endl;
            //std::cout << "Value " << projected_path[0] << " " << projected_path[1] << std::endl;
            memcpy (join_buffer.local_compute_output + (index * (RA_count * threshold + RA_count)) + (RA_count + threshold * ra_id) + join_buffer.local_compute_output_size[ra_id][index], (const unsigned char*)projected_path, sizeof(u64) * join_buffer.width[ra_id]);

            join_buffer.local_compute_output_size[ra_id][index] = join_buffer.local_compute_output_size[ra_id][index] + join_buffer.width[ra_id];
            join_buffer.local_compute_output[(index * (RA_count * threshold + RA_count)) + ra_id] = join_buffer.local_compute_output[(index * (RA_count * threshold + RA_count)) + ra_id] + join_buffer.width[ra_id];


            (*local_join_inserts)++;
            (*local_join_count)++;
        }
        else
            (*local_join_duplicates)++;
    }

    for (auto nxt = cur_trie->next.begin(); nxt; nxt.next())
    {

        u64 nxt_node = nxt.key();
        shmap_relation *nxt_trie = nxt.val();
        cur_path.push_back(nxt_node);
        as_all_to_all_right_join_buffer_helper(threshold, RA_count, nxt_trie, cur_path, join_buffer, input0_buffer, input0_buffer_width, input1_buffer_width, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, join_column_count, deduplicate, local_join_count, local_join_duplicates, local_join_inserts, name, head_rel_hash_col_count, canonical, tracker);
        cur_path.pop_back();
    }
}


void shmap_relation::as_all_to_all_left_join_buffer(int threshold, int RA_count, std::vector<u64> prefix, all_to_all_buffer& join_buffer, u64 *input0_buffer, int input0_buffer_width, int input1_buffer_width, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int> reorder_map, int join_column_count, shmap_relation& deduplicate, int* local_join_count, u32* local_join_duplicates, u32* local_join_inserts, int head_rel_hash_col_count, bool canonical, u32* tracker)
{
    shmap_relation *m_trie = this;
    for (u64 n : prefix)  {
        if (m_trie->next.find(n)==NULL)
            return;
        m_trie = *(m_trie->next.find(n));
    }

    as_all_to_all_left_join_buffer_helper(threshold, RA_count, m_trie, prefix, join_buffer, input0_buffer, input0_buffer_width, input1_buffer_width, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, join_column_count, deduplicate, local_join_count, local_join_duplicates, local_join_inserts, head_rel_hash_col_count, canonical, tracker);
}



void shmap_relation::as_all_to_all_left_join_buffer_helper(int threshold, int RA_count, shmap_relation*& cur_trie, std::vector<u64> cur_path, all_to_all_buffer& join_buffer, u64 *input0_buffer, int input0_buffer_width, int input1_buffer_width, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int> reorder_map, int join_column_count, shmap_relation& deduplicate, int* local_join_count, u32* local_join_duplicates, u32* local_join_inserts, int head_rel_hash_col_count, bool canonical, u32* tracker)
{
    if(cur_trie->is_end)
    {
        u64 projected_path[join_buffer.width[ra_id]];
        u64 reordered_cur_path[input0_buffer_width + input1_buffer_width - join_column_count];
        for (int i = 0; i < input0_buffer_width; i++)
            reordered_cur_path[i] = input0_buffer[i];

        for (int i = join_column_count; i < input1_buffer_width; i++)
            reordered_cur_path[input0_buffer_width + (i - join_column_count)] = cur_path[i];

        for (int i =0; i < join_buffer.width[ra_id]; i++)
            projected_path[i] = reordered_cur_path[reorder_map[i]];

        if (deduplicate.insert_tuple_from_array(projected_path, join_buffer.width[ra_id]) == true)
        {
            uint64_t bucket_id = tuple_hash(projected_path, head_rel_hash_col_count) % buckets;
            uint64_t sub_bucket_id=0;
            if (canonical == false)
                sub_bucket_id = tuple_hash(projected_path + head_rel_hash_col_count, join_buffer.width[ra_id]-head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

            int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];

            if ((int)join_buffer.local_compute_output[(index * (RA_count * threshold + RA_count)) + ra_id] + join_buffer.width[ra_id] >= threshold  || *tracker == 1)
            {
                *tracker = *tracker | 1;
                return;
            }


            //std::cout << "index " << (index * (RA_count * threshold + RA_count)) + (RA_count + threshold * ra_id) + join_buffer.local_compute_output_size[ra_id][index] << std::endl;
            //std::cout << "Value " << projected_path[0] << " " << projected_path[1] << " " << input0_buffer_width + input1_buffer_width - join_column_count  << std::endl;
            memcpy (join_buffer.local_compute_output + (index * (RA_count * threshold + RA_count)) + (RA_count + threshold * ra_id) + join_buffer.local_compute_output_size[ra_id][index], (const unsigned char*)projected_path, sizeof(u64) * join_buffer.width[ra_id]);

            join_buffer.local_compute_output_size[ra_id][index] = join_buffer.local_compute_output_size[ra_id][index] + join_buffer.width[ra_id];
            join_buffer.local_compute_output[(index * (RA_count * threshold + RA_count)) + ra_id] = join_buffer.local_compute_output[(index * (RA_count * threshold + RA_count)) + ra_id] + join_buffer.width[ra_id];

            (*local_join_inserts)++;
            (*local_join_count)++;
        }
        else
            (*local_join_duplicates)++;
    }

    for (auto nxt = cur_trie->next.begin(); nxt; nxt.next())
    {
        u64 nxt_node = nxt.key();
        shmap_relation *nxt_trie = nxt.val();
        cur_path.push_back(nxt_node);
        as_all_to_all_left_join_buffer_helper(threshold, RA_count, nxt_trie, cur_path, join_buffer, input0_buffer, input0_buffer_width, input1_buffer_width, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, join_column_count, deduplicate, local_join_count, local_join_duplicates, local_join_inserts, head_rel_hash_col_count, canonical, tracker);
        cur_path.pop_back();
    }
}
