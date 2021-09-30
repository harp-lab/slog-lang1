/*
 * Google's btree relation
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#include "../parallel_RA_inc.h"



bool relation::load_balance_merge_full_and_delta(float rf)
{
    u32 rank = (u32)mcomm.get_local_rank();
    int nprocs = mcomm.get_local_nprocs();
    int buckets = get_bucket_count();

    int mcount = 0;
    u32 total_old_sub_bucket_count = 0;
    for (int i = 0; i < buckets; i++)
    {
        total_old_sub_bucket_count = total_old_sub_bucket_count + sub_bucket_per_bucket_count[i];
        if (sub_bucket_per_bucket_count[i] >= rf)
            mcount++;
    }

    if (mcount > 0.8* buckets)
    {
        if (rank == 0)
            std::cout << "[YES] Bucket Consolidation [" << mcount << " " << 0.8 * buckets << "] Old Sub Bucket count " << total_old_sub_bucket_count << std::endl;
    }
    else
    {
        if (rank == 0)
            std::cout << "[NO] Bucket Consolidation [" << mcount << " " << 0.8 * buckets << "] Old Sub Bucket count " << total_old_sub_bucket_count << std::endl;
        return false;
    }

    int *max_sub_bucket_size = new int[buckets];
    memset(max_sub_bucket_size, 0, buckets * sizeof(int));
    u32 global_total_sub_bucket_size = 0;
    u32 total_sub_bucket_size = 0;
    u32 total_sub_bucket_count = 0;
    for (int i = 0; i < buckets; i++)
    {
        total_sub_bucket_count = total_sub_bucket_count + sub_bucket_per_bucket_count[i];
        if (bucket_map[i] == 1)
        {
            for (u32 j = 0; j < sub_bucket_per_bucket_count[i]; j++)
            {
                if (full_sub_bucket_element_count[i][j] != 0)
                {
                    if ((int)full_sub_bucket_element_count[i][j] > max_sub_bucket_size[i])
                        max_sub_bucket_size[i] = full_sub_bucket_element_count[i][j];

                    total_sub_bucket_size = total_sub_bucket_size + full_sub_bucket_element_count[i][j];
                }
            }
        }
    }

    int *global_max = new int[buckets];
    memset(global_max, 0, buckets * sizeof(int));
    int average_sub_bucket_size;
    MPI_Allreduce(max_sub_bucket_size, global_max, buckets, MPI_INT, MPI_MAX, mcomm.get_local_comm());
    MPI_Allreduce(&total_sub_bucket_size, &global_total_sub_bucket_size, 1, MPI_INT, MPI_SUM, mcomm.get_local_comm());
    delete[] max_sub_bucket_size;
    average_sub_bucket_size = global_total_sub_bucket_size / total_sub_bucket_count;

    u32 global_new_sub_bucket[buckets];
    memcpy(global_new_sub_bucket, sub_bucket_per_bucket_count, buckets * sizeof(u32));

    int process_size[nprocs];
    memset(process_size, 0, nprocs * sizeof(int));

    int process_data_vector_size=0;
    vector_buffer* process_data_vector = (vector_buffer*)malloc(sizeof(vector_buffer) * nprocs);
    for (int i = 0; i < nprocs; ++i) {
        process_data_vector[i].vector_buffer_create_empty();
    }


    int process_size_dt[nprocs];
    memset(process_size_dt, 0, nprocs * sizeof(int));

    int process_data_vector_dt_size=0;
    vector_buffer* process_data_vector_dt = (vector_buffer*)malloc(sizeof(vector_buffer) * nprocs);
    for (int i = 0; i < nprocs; ++i) {
        process_data_vector_dt[i].vector_buffer_create_empty();
    }

    for (int b = 0; b < buckets; b++)
    {
        if (global_max[b] > average_sub_bucket_size)
            continue;

        global_new_sub_bucket[b] = global_new_sub_bucket[b] / rf;
        if (global_new_sub_bucket[b] == 0)
            global_new_sub_bucket[b] = 1;

        if (sub_bucket_per_bucket_count[b] > global_new_sub_bucket[b])
        {
            bucket_map[b] = 0;

            std::unordered_set<int> distinct_t_ranks;
            int* temp = new int[sub_bucket_per_bucket_count[b]];
            memcpy(temp, sub_bucket_rank[b], sizeof(int) * sub_bucket_per_bucket_count[b]);

            delete[] sub_bucket_rank[b];
            sub_bucket_rank[b] = new u32[global_new_sub_bucket[b]];

            memcpy(sub_bucket_rank[b], temp, sizeof(int) * global_new_sub_bucket[b]);
            delete[] temp;

            for (u64 x = 0; x < global_new_sub_bucket[b]; x++)
            {
                if (sub_bucket_rank[b][x] == rank)
                    bucket_map[b] = 1;

                distinct_t_ranks.insert(sub_bucket_rank[b][x]);
            }

            delete[] distinct_sub_bucket_rank[b];
            distinct_sub_bucket_rank[b] = new int[distinct_t_ranks.size()];
            u32 x = 0;
            for ( auto it = distinct_t_ranks.begin(); it != distinct_t_ranks.end(); ++it )
            {
                distinct_sub_bucket_rank[b][x] = *it;
                x++;
            }
            distinct_sub_bucket_rank_count[b] = x;

            delete[] full_sub_bucket_element_count[b];
            full_sub_bucket_element_count[b] = new u32[global_new_sub_bucket[b]];
            memset(full_sub_bucket_element_count[b], 0, sizeof(u32) * global_new_sub_bucket[b]);

            delete[] delta_sub_bucket_element_count[b];
            delta_sub_bucket_element_count[b] = new u32[global_new_sub_bucket[b]];
            memset(delta_sub_bucket_element_count[b], 0, sizeof(u32) * global_new_sub_bucket[b]);

            delete[] newt_sub_bucket_element_count[b];
            newt_sub_bucket_element_count[b] = new u32[global_new_sub_bucket[b]];
            memset(newt_sub_bucket_element_count[b], 0, sizeof(u32) * global_new_sub_bucket[b]);

            vector_buffer temp_buffer;
            std::vector<u64> prefix = {};

            temp_buffer.vector_buffer_create_empty();
            full[b].as_vector_buffer_recursive(&temp_buffer, prefix);

            for (u32 s = 0; s < temp_buffer.size / sizeof(u64); s=s+(arity+1))
            {
                u64 reordered_cur_path[(arity+1)];
                for (u32 j =0; j < (arity+1); j++)
                    memcpy(reordered_cur_path, (&temp_buffer)->buffer + ((s + j) * sizeof(u64)), sizeof(u64));

                uint64_t bucket_id = tuple_hash(reordered_cur_path, join_column_count) % buckets;
                uint64_t sub_bucket_id = tuple_hash(reordered_cur_path+join_column_count, (arity+1)-join_column_count) % global_new_sub_bucket[bucket_id];
                int index = sub_bucket_rank[bucket_id][sub_bucket_id];

                process_size[index] = process_size[index] + (arity+1);
                process_data_vector[index].vector_buffer_append((const unsigned char*)reordered_cur_path, sizeof(u64)*(arity+1));

                process_data_vector_size = process_data_vector_size + (arity + 1);
            }
            temp_buffer.vector_buffer_free();
            full[b].remove_tuple();

            temp_buffer.vector_buffer_create_empty();
            delta[b].as_vector_buffer_recursive(&temp_buffer, prefix);

            for (u32 s = 0; s < temp_buffer.size / sizeof(u64); s=s+(arity+1))
            {
                u64 reordered_cur_path[(arity+1)];
                for (u32 j =0; j < (arity+1); j++)
                    memcpy(reordered_cur_path, (&temp_buffer)->buffer + ((s + j) * sizeof(u64)), sizeof(u64));

                uint64_t bucket_id = tuple_hash(reordered_cur_path, join_column_count) % buckets;
                uint64_t sub_bucket_id = tuple_hash(reordered_cur_path+join_column_count, (arity+1)-join_column_count) % global_new_sub_bucket[bucket_id];
                int index = sub_bucket_rank[bucket_id][sub_bucket_id];

                process_size_dt[index] = process_size_dt[index] + (arity+1);
                process_data_vector_dt[index].vector_buffer_append((const unsigned char*)reordered_cur_path, sizeof(u64)*(arity+1));

                process_data_vector_dt_size = process_data_vector_dt_size + (arity + 1);
            }
            temp_buffer.vector_buffer_free();
            delta[b].remove_tuple();
        }
    }
    delete[] global_max;

    memcpy(sub_bucket_per_bucket_count, global_new_sub_bucket, sizeof(u32) * buckets);

    int outer_hash_buffer_size = 0;
    u64* outer_hash_data;
    all_to_all_comm(process_data_vector, process_data_vector_size, process_size, &outer_hash_buffer_size, &outer_hash_data, mcomm.get_local_comm());
    free (process_data_vector);

    u64 t[(arity+1)];
    for (int in = 0; in < outer_hash_buffer_size; in = in + (arity+1))
    {
        for (u32 x=0; x < (arity+1); x++)
            t[x] = outer_hash_data[in + x];

        insert_in_full(t);
    }
    delete[] outer_hash_data;


    int dt_outer_hash_buffer_size = 0;
    u64* dt_outer_hash_data;
    all_to_all_comm(process_data_vector_dt, process_data_vector_dt_size, process_size_dt, &dt_outer_hash_buffer_size, &dt_outer_hash_data, mcomm.get_local_comm());
    free (process_data_vector_dt);

    for (int in = 0; in < dt_outer_hash_buffer_size; in = in + (arity+1))
    {
        for (u32 x=0; x < (arity+1); x++)
            t[x] = dt_outer_hash_data[in + x];

        insert_in_delta(t);
    }
    delete[] dt_outer_hash_data;

    return true;
}



bool relation::load_balance_split_full_and_delta(float rf)
{
    u32 rank = (u32)mcomm.get_local_rank();
    int nprocs = mcomm.get_nprocs();
    int buckets = get_bucket_count();

    int min_sub_bucket_size = INT_MAX;
    int *max_sub_bucket_size = new int[buckets];
    memset(max_sub_bucket_size, 0, buckets * sizeof(int));

    u32 global_total_sub_bucket_size = 0;
    u32 total_sub_bucket_size = 0;
    u32 total_sub_bucket_count = 0;
    for (int i = 0; i < buckets; i++)
    {
        total_sub_bucket_count = total_sub_bucket_count + sub_bucket_per_bucket_count[i];
        if (bucket_map[i] == 1)
        {
            for (u32 j = 0; j < sub_bucket_per_bucket_count[i]; j++)
            {
                if (full_sub_bucket_element_count[i][j] != 0)
                {
                    if ((int)full_sub_bucket_element_count[i][j] > max_sub_bucket_size[i])
                        max_sub_bucket_size[i] = full_sub_bucket_element_count[i][j];

                    if ((int)full_sub_bucket_element_count[i][j] < min_sub_bucket_size)
                        min_sub_bucket_size = full_sub_bucket_element_count[i][j];

                    total_sub_bucket_size = total_sub_bucket_size + full_sub_bucket_element_count[i][j];
                }
            }
        }
    }

    int *global_max = new int[buckets];
    memset(global_max, 0, buckets * sizeof(int));

    int global_min;
    int average_sub_bucket_size;
    MPI_Allreduce(max_sub_bucket_size, global_max, buckets, MPI_INT, MPI_MAX, mcomm.get_local_comm());
    MPI_Allreduce(&min_sub_bucket_size, &global_min, 1, MPI_INT, MPI_MIN, mcomm.get_local_comm());
    MPI_Allreduce(&total_sub_bucket_size, &global_total_sub_bucket_size, 1, MPI_INT, MPI_SUM, mcomm.get_local_comm());
    delete[] max_sub_bucket_size;

    average_sub_bucket_size = global_total_sub_bucket_size / total_sub_bucket_count;

    u32 global_new_sub_bucket[buckets];
    memcpy(global_new_sub_bucket, sub_bucket_per_bucket_count, buckets * sizeof(u32));

    int count = 0;
    int old_total_sub_buckets = 0;
    int new_total_sub_buckets = 0;
    int global_global_max = 0;
    int average_global_max = 0;
    for (int i = 0; i < buckets; i++)
    {
        if (global_new_sub_bucket[i] >= (u32)nprocs)
            continue;

        if (global_max[i] > average_sub_bucket_size * rf * 0.8)
        {
            global_new_sub_bucket[i] = global_new_sub_bucket[i] * rf;
            count++;
        }

        average_global_max = average_global_max + global_max[i];

        if (global_global_max < global_max[i])
            global_global_max = global_max[i];


        old_total_sub_buckets = old_total_sub_buckets + sub_bucket_per_bucket_count[i];
        new_total_sub_buckets = new_total_sub_buckets + global_new_sub_bucket[i];
    }
    delete[] global_max;

    if (count == 0)
    {
        if (rank == 0)
            std::cout << "[NO] G RF " << rf << " Bucket Split -- Global Min " << global_min << " Average bucket size " << average_sub_bucket_size << " Global Max [" << global_global_max << " " << average_global_max/buckets  << "] OLD " << old_total_sub_buckets << " NEW " << new_total_sub_buckets << std::endl;
        return false;
    }
    else if (count != 0)
    {
        if (rank == 0)
            std::cout << "[YES] G RF " << rf << " Bucket Split -- Global Min " << global_min << " Average bucket size " << average_sub_bucket_size << " Global Max [" << global_global_max << " " << average_global_max/buckets  << "] OLD " << old_total_sub_buckets << " NEW " << new_total_sub_buckets << std::endl;
        return false;
    }

    int rcount =  get_last_rank();
    int process_size[nprocs];
    memset(process_size, 0, nprocs * sizeof(int));

    int process_data_vector_size=0;
    vector_buffer* process_data_vector = (vector_buffer*)malloc(sizeof(vector_buffer) * nprocs);
    for (int i = 0; i < nprocs; ++i) {
        process_data_vector[i].vector_buffer_create_empty();
    }

    int process_size_dt[nprocs];
    memset(process_size_dt, 0, nprocs * sizeof(int));

    int process_data_vector_dt_size=0;
    vector_buffer* process_data_vector_dt = (vector_buffer*)malloc(sizeof(vector_buffer) * nprocs);
    for (int i = 0; i < nprocs; ++i) {
        process_data_vector_dt[i].vector_buffer_create_empty();
    }

    for (int b = 0; b < buckets; b++)
    {
        if (sub_bucket_per_bucket_count[b] < global_new_sub_bucket[b])
        {
            bucket_map[b] = 0;

            std::unordered_set<int> distinct_t_ranks;
            u32* temp = new u32[sub_bucket_per_bucket_count[b]];
            memcpy(temp, sub_bucket_rank[b], sizeof(int) * sub_bucket_per_bucket_count[b]);

            delete[] sub_bucket_rank[b];
            sub_bucket_rank[b] = new u32[global_new_sub_bucket[b]];

            memcpy(sub_bucket_rank[b], temp, sizeof(int) * sub_bucket_per_bucket_count[b]);
            delete[] temp;

            for (u64 x = 0; x < global_new_sub_bucket[b]; x++)
            {
                if (x >= sub_bucket_per_bucket_count[b])
                {
                    rcount++;
                    sub_bucket_rank[b][x] = rcount % nprocs;
                    set_last_rank(rcount);
                }

                if (sub_bucket_rank[b][x] == rank)
                    bucket_map[b] = 1;

                distinct_t_ranks.insert(sub_bucket_rank[b][x]);
            }

            delete[] distinct_sub_bucket_rank[b];
            distinct_sub_bucket_rank[b] = new int[distinct_t_ranks.size()];
            u32 x = 0;
            for ( auto it = distinct_t_ranks.begin(); it != distinct_t_ranks.end(); ++it )
            {
                distinct_sub_bucket_rank[b][x] = *it;
                x++;
            }
            distinct_sub_bucket_rank_count[b] = x;

            delete[] full_sub_bucket_element_count[b];
            full_sub_bucket_element_count[b] = new u32[global_new_sub_bucket[b]];
            memset(full_sub_bucket_element_count[b], 0, sizeof(u32) * global_new_sub_bucket[b]);

            vector_buffer temp_buffer;
            temp_buffer.vector_buffer_create_empty();

            std::vector<u64> prefix = {};
            full[b].as_vector_buffer_recursive(&temp_buffer, prefix);

            for (u32 s = 0; s < temp_buffer.size / sizeof(u64); s=s+(arity+1))
            {
                u64 reordered_cur_path[(arity+1)];
                for (u32 j =0; j < (arity+1); j++)
                    memcpy(reordered_cur_path, (&temp_buffer)->buffer + ((s + j) * sizeof(u64)), sizeof(u64));

                uint64_t bucket_id = tuple_hash(reordered_cur_path, join_column_count) % buckets;
                uint64_t sub_bucket_id = tuple_hash(reordered_cur_path+join_column_count, (arity+1)-join_column_count) % global_new_sub_bucket[bucket_id];
                int index = sub_bucket_rank[bucket_id][sub_bucket_id];

                process_size[index] = process_size[index] + (arity+1);
                process_data_vector[index].vector_buffer_append((const unsigned char*)reordered_cur_path, sizeof(u64)*(arity+1));

                process_data_vector_size = process_data_vector_size + (arity+1);
            }
            temp_buffer.vector_buffer_free();
            full[b].remove_tuple();


            temp_buffer.vector_buffer_create_empty();
            delta[b].as_vector_buffer_recursive(&temp_buffer, prefix);

            for (u32 s = 0; s < temp_buffer.size / sizeof(u64); s=s+(arity+1))
            {
                u64 reordered_cur_path[(arity+1)];
                for (u32 j =0; j < (arity+1); j++)
                    memcpy(reordered_cur_path, (&temp_buffer)->buffer + ((s + j) * sizeof(u64)), sizeof(u64));

                uint64_t bucket_id = tuple_hash(reordered_cur_path, join_column_count) % buckets;
                uint64_t sub_bucket_id = tuple_hash(reordered_cur_path + join_column_count, ((arity+1)-join_column_count)) % global_new_sub_bucket[bucket_id];
                int index = sub_bucket_rank[bucket_id][sub_bucket_id];

                process_size_dt[index] = process_size_dt[index] + (arity+1);
                process_data_vector_dt[index].vector_buffer_append((const unsigned char*)reordered_cur_path, sizeof(u64)*(arity+1));

                process_data_vector_dt_size = process_data_vector_dt_size + (arity+1);
            }
            temp_buffer.vector_buffer_free();
            delta[b].remove_tuple();
        }
    }

    memcpy(sub_bucket_per_bucket_count, global_new_sub_bucket, sizeof(u32) * buckets);


    int outer_hash_buffer_size = 0;
    u64* outer_hash_data;
    all_to_all_comm(process_data_vector, process_data_vector_size, process_size, &outer_hash_buffer_size, &outer_hash_data, mcomm.get_local_comm());
    free(process_data_vector);

    u64 t[arity + 1];
    for (int in = 0; in < outer_hash_buffer_size; in = in + arity+1)
    {
        for (u32 j=0; j < (arity+1); j++)
            t[j] = outer_hash_data[in + j];
        insert_in_full(t);
    }
    delete[] outer_hash_data;

    int dt_outer_hash_buffer_size = 0;
    u64* dt_outer_hash_data;
    all_to_all_comm(process_data_vector_dt, process_data_vector_dt_size, process_size_dt, &dt_outer_hash_buffer_size, &dt_outer_hash_data, mcomm.get_local_comm());
    free(process_data_vector_dt);

    for (int in = 0; in < dt_outer_hash_buffer_size; in = in + arity+1)
    {
        for (u32 j=0; j < (arity+1); j++)
            t[j] = dt_outer_hash_data[in + j];
        insert_in_delta(t);
    }
    delete[] dt_outer_hash_data;

    return true;
}
