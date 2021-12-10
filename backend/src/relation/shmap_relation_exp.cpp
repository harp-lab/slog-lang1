/*
 * Google's btree relation
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#include "../parallel_RA_inc.h"
#include <iostream>
#include <iterator>
#include <vector>


bool shmap_relation::insert_tuple_from_array(u64* t, int arity)
{
    //for (int i=0; i < arity; i++)
    //    std::cout << "Insert newt " << t[i] << "\t";
    //std::cout << "\n";

    bool counter = false;
    shmap_relation *node = this;
    for (int i = 0; i < arity-1; i++)
    {
        if (node->next.find(t[i]) == NULL)
        {
            node->next.insert(t[i], new shmap_relation());
            counter = true;
        }
        node = *((node->next).find(t[i]));
    }

    if (node->next.find(t[arity-1]) == NULL)
    {
        //std::cout << "Inserting null " << t[arity-1] << std::endl;
        node->next.insert(t[arity-1], NULL);
        counter = true;
    }

    return counter;
}



void shmap_relation::remove_tuple()
{
    shmap_relation *node = this;
    //if (node != NULL)
    //{
        for (auto nxt = node->next.begin(); nxt; nxt.next())
        {
            //std::cout << "Delete value " << nxt.key() << std::endl;
            shmap_relation *nxt_trie = nxt.val();
            if (nxt_trie != NULL)
            {
            nxt_trie->remove_tuple();
            delete nxt_trie;
            }
        }
        node->next.clear();
    //}
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



void shmap_relation::as_vector_buffer_recursive_helper(shmap_relation*& cur_trie, std::vector<u64> &cur_path, vector_buffer*& result_vector)
{
    if ( cur_path.size() != 0 && cur_trie == NULL)
    {
        u64 path[cur_path.size()];
        for (u32 i = 0; i < cur_path.size(); i++)
            path[i] = cur_path[i];
        result_vector->vector_buffer_append((const unsigned char*)path, sizeof(u64)*cur_path.size());
        return;
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



void shmap_relation::as_all_to_allv_acopy_buffer_helper(shmap_relation*& cur_trie, std::vector<u64> &cur_path, all_to_allv_buffer& buffer, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int>& reorder_map, u32 arity, u32 join_column_count, int head_rel_hash_col_count, bool canonical)
{
    if ( cur_path.size() != 0 && cur_trie == NULL)
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
        return;
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



void shmap_relation::as_all_to_allv_copy_buffer_helper(
    shmap_relation*& cur_trie, std::vector<u64>& cur_path,
    all_to_allv_buffer& buffer, int ra_id,
    u32 buckets, u32* output_sub_bucket_count,
    u32** output_sub_bucket_rank, std::vector<int>& reorder_map,
    u32 arity, u32 join_column_count,
    int head_rel_hash_col_count, bool canonical)
{
    if ( cur_path.size() != 0 && cur_trie == NULL)
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
        return;
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



void shmap_relation::as_all_to_allv_copy_filter_buffer_helper(shmap_relation*& cur_trie, std::vector<u64>& cur_path, all_to_allv_buffer& buffer, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int>& reorder_map, u32 arity, u32 join_column_count, bool(*lambda)(const u64* const), int head_rel_hash_col_count, bool canonical)
{

    if ( cur_path.size() != 0 && cur_trie == NULL)
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
        return;
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


void shmap_relation::as_all_to_allv_copy_generate_buffer(all_to_allv_buffer& buffer, std::vector<u64> prefix, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, u32 arity, u32 join_column_count, int(*lambda)(const u64* const, u64* const), int head_rel_hash_col_count, bool canonical)
{
    shmap_relation *m_trie = this;
    for (u64 n : prefix)
    {
        if (m_trie->next.find(n)==NULL)
            return;
        m_trie = *(m_trie->next.find(n));
    }
    as_all_to_allv_copy_generate_buffer_helper(m_trie, prefix, buffer, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, arity, join_column_count, lambda, head_rel_hash_col_count, canonical);
}



void shmap_relation::as_all_to_allv_copy_generate_buffer_helper(shmap_relation*& cur_trie, std::vector<u64>& cur_path, all_to_allv_buffer& buffer, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, u32 arity, u32 join_column_count, int(*lambda)(const u64* const, u64* const), int head_rel_hash_col_count, bool canonical)
{

    if ( cur_path.size() != 0 && cur_trie == NULL)
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
        return;
    }

    for (auto nxt = cur_trie->next.begin(); nxt; nxt.next())
    {
        u64 nxt_node = nxt.key();
        shmap_relation *nxt_trie = nxt.val();
        cur_path.push_back(nxt_node);
        as_all_to_allv_copy_generate_buffer_helper(nxt_trie, cur_path, buffer, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, arity, join_column_count, lambda, head_rel_hash_col_count, canonical);
        cur_path.pop_back();
    }
}


void shmap_relation::as_all_to_allv_right_join_buffer(std::vector<u64> prefix, all_to_allv_buffer& join_buffer, u64 *input0_buffer, int input0_buffer_width, int input1_buffer_width, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int> reorder_map, int join_column_count, shmap_relation& deduplicate, int *local_join_count, u32* local_join_duplicates, u32* local_join_inserts, int head_rel_hash_col_count, bool canonical)
{
    //std::cout << "RIGHT" << std::endl;
    shmap_relation *m_trie = this;
    for (u64 n : prefix)  {
        if (m_trie->next.find(n)==NULL)
            return;
        m_trie = *(m_trie->next.find(n));
    }

    as_all_to_allv_right_join_buffer_helper(m_trie, prefix, join_buffer, input0_buffer, input0_buffer_width, input1_buffer_width, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, join_column_count, deduplicate, local_join_count, local_join_duplicates, local_join_inserts, head_rel_hash_col_count, canonical);
}



void shmap_relation::as_all_to_allv_right_join_buffer_helper(shmap_relation*& cur_trie, std::vector<u64>& cur_path, all_to_allv_buffer& join_buffer, u64 *input0_buffer, int input0_buffer_width, int input1_buffer_width, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int>& reorder_map, int join_column_count, shmap_relation& deduplicate, int *local_join_count, u32* local_join_duplicates, u32* local_join_inserts, int head_rel_hash_col_count, bool canonical)
{

    if ( cur_path.size() != 0 && cur_trie == NULL)
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
        return;
    }

    for (auto nxt = cur_trie->next.begin(); nxt; nxt.next())
    {

        u64 nxt_node = nxt.key();
        shmap_relation *nxt_trie = nxt.val();
        cur_path.push_back(nxt_node);
        as_all_to_allv_right_join_buffer_helper(nxt_trie, cur_path, join_buffer, input0_buffer, input0_buffer_width, input1_buffer_width, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, join_column_count, deduplicate, local_join_count, local_join_duplicates, local_join_inserts, head_rel_hash_col_count, canonical);
        cur_path.pop_back();
    }
}


void shmap_relation::as_all_to_allv_right_outer_join_buffer(
    shmap_relation* neg_target, all_to_allv_buffer& join_buffer, u64 *input0_buffer,
    int input0_buffer_width, int input1_buffer_width, int ra_id, u32 buckets,
    u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int>& reorder_map,
    int join_column_count, int out_arity,
    int head_rel_hash_col_count, bool canonical)
{
    shmap_relation *m_trie = this;
    std::vector<u64> init_path;
    if (neg_target == NULL)
    {
        std::cout << "empty negate target !" << std::endl;   
        // if nothing need to negate, copy the whole trie
        std::vector<u64> cur_path;
        as_all_to_allv_copy_buffer_helper(
                m_trie, cur_path,
                join_buffer, ra_id, buckets,
                output_sub_bucket_count,
                output_sub_bucket_rank,
                reorder_map, out_arity,
                join_column_count, head_rel_hash_col_count,
                canonical);
        
        return;
    }
    as_all_to_allv_right_outer_join_buffer_helper(
        neg_target, m_trie, init_path, join_buffer, input0_buffer,
        input0_buffer_width, input1_buffer_width,
        ra_id, buckets,
        output_sub_bucket_count, output_sub_bucket_rank,
        reorder_map, join_column_count,
        out_arity,
        head_rel_hash_col_count, canonical);
}

void shmap_relation::as_all_to_allv_right_outer_join_buffer_helper(
    shmap_relation* neg_target,
    shmap_relation*& cur_trie, std::vector<u64>& cur_path,
    all_to_allv_buffer& join_buffer, u64 *input0_buffer,
    int input0_buffer_width, int input1_buffer_width,
    int ra_id, u32 buckets,
    u32* output_sub_bucket_count, u32** output_sub_bucket_rank,
    std::vector<int> &reorder_map, int join_column_count,
    int out_arity,
    int head_rel_hash_col_count, bool canonical)
{ 
    if (cur_path.size() >= join_column_count || neg_target == NULL)
    {
        return;
    }
    for (auto nxt = cur_trie->next.begin(); nxt; nxt.next())
    {
        u64 nxt_node = nxt.key();
        // std::cout << "current check trie " << nxt_node << std::endl;
        shmap_relation *nxt_trie = nxt.val();
        cur_path.push_back(nxt_node);
        auto nxt_neg = neg_target->next.find(nxt_node);
        if (nxt_neg != NULL)
        {
            //prefix match recursive down to check prefix
            as_all_to_allv_right_outer_join_buffer_helper(
                (*nxt_neg), nxt_trie, cur_path, join_buffer, input0_buffer,
                input0_buffer_width, input1_buffer_width,
                ra_id, buckets,
                output_sub_bucket_count, output_sub_bucket_rank,
                reorder_map, join_column_count,
                out_arity,
                head_rel_hash_col_count, canonical);
        }
        else
        {
            // unmatch data, copy them all to target relation
            as_all_to_allv_copy_buffer_helper(
                nxt_trie, cur_path,
                join_buffer, ra_id, buckets,
                output_sub_bucket_count,
                output_sub_bucket_rank,
                reorder_map, out_arity,
                join_column_count, head_rel_hash_col_count,
                canonical);         
            // std::cout << "left join count " << (*local_join_count) << std::endl;
        }
        cur_path.pop_back();
    }

}

void shmap_relation::as_all_to_allv_left_join_buffer(std::vector<u64> prefix, all_to_allv_buffer& join_buffer, u64 *input0_buffer, int input0_buffer_width, int input1_buffer_width, int ra_id, u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, std::vector<int> reorder_map, int join_column_count, shmap_relation& deduplicate, int* local_join_count, u32* local_join_duplicates, u32* local_join_inserts, int head_rel_hash_col_count, bool canonical)
{
    //std::cout << "LEFT" << std::endl;
    shmap_relation *m_trie = this;
    for (u64 n : prefix)  {
        if (m_trie->next.find(n)==NULL)
            return;
        m_trie = *(m_trie->next.find(n));
    }

    as_all_to_allv_left_join_buffer_helper(m_trie, prefix, join_buffer, input0_buffer, input0_buffer_width, input1_buffer_width, ra_id, buckets, output_sub_bucket_count, output_sub_bucket_rank, reorder_map, join_column_count, deduplicate, local_join_count, local_join_duplicates, local_join_inserts, head_rel_hash_col_count, canonical);
}



void shmap_relation::as_all_to_allv_left_join_buffer_helper(
    shmap_relation*& cur_trie,
    std::vector<u64>& cur_path,
    all_to_allv_buffer& join_buffer,
    u64 *input0_buffer,
    int input0_buffer_width,
    int input1_buffer_width,
    int ra_id,
    u32 buckets,
    u32* output_sub_bucket_count,
    u32** output_sub_bucket_rank,
    std::vector<int>& reorder_map,
    int join_column_count,
    shmap_relation& deduplicate,
    int* local_join_count,
    u32* local_join_duplicates,
    u32* local_join_inserts,
    int head_rel_hash_col_count,
    bool canonical)
{

    if ( cur_path.size() != 0 && cur_trie == NULL)
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
        return;
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
