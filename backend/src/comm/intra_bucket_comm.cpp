/*
 * Intra-bucket comm to handle sub-bucketting
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */

#include "../parallel_RA_inc.h"



/// Method implementing intra-bucket comm
///
/// Input:
///     rel (data that needs to be transmitted)
///     input_distinct_sub_bucket_rank_count (the number of processes a process has to send data to (for every bucket))
///     input_distinct_sub_bucket_rank (rank of the processes a process has to send data to (for every bucket))
///     input_bucket_map
///     output_distinct_sub_bucket_rank_count (the number of processes a process has to send data to (for every bucket))
///     output_distinct_sub_bucket_rank (rank of the processes a process has to send data to (for every bucket))
///     output_bucket_map
/// Output:
///     total_buffer_size
///     recvbuf

#ifdef GOOGLE_MAP
void intra_bucket_comm(u32 buckets,
                       google_relation *rel,
                       int* input_distinct_sub_bucket_rank_count, int** input_distinct_sub_bucket_rank, u32* input_bucket_map,
                       int* output_distinct_sub_bucket_rank_count, int** output_distinct_sub_bucket_rank, u32* output_bucket_map,
                       u64 *total_buffer_size, u64 **recvbuf,
                       MPI_Comm mcomm)
{
    // buffer to hold relation data to be sent out
    vector_buffer *input_buffer = new vector_buffer[buckets];
    int *input_buffer_size = new int[buckets];

    //std::cout << "Buckets " << buckets << std::endl;

    u32** meta_buffer_size = new u32*[buckets];
    memset(meta_buffer_size, 0, sizeof(u32*) * buckets);

    *total_buffer_size = 0;
    u32* bucket_offset = new u32[buckets];

    int rank;
    MPI_Comm_rank(mcomm, &rank);

    u64 total_send_buffer_size = 0;
    for (u32 i = 0; i < buckets; i++)
    {
        // Buffer to store relation data
        input_buffer[i].vector_buffer_create_empty();

        // Puts btree data into a vector
        std::vector<u64> prefix = {};
        rel[i].as_vector_buffer_recursive(&(input_buffer[i]), prefix);

        // size of data to be sent
        input_buffer_size[i] = (&input_buffer[i])->size / sizeof(u64);
        total_send_buffer_size = total_send_buffer_size + input_buffer_size[i];

        meta_buffer_size[i] = new u32[input_distinct_sub_bucket_rank_count[i]];
        memset(meta_buffer_size[i], 0, sizeof(u32) * input_distinct_sub_bucket_rank_count[i]);

        u32 req_counter1 = 0;
        MPI_Request *req1 = new MPI_Request[output_distinct_sub_bucket_rank_count[i] + input_distinct_sub_bucket_rank_count[i]];
        MPI_Status *stat1 = new MPI_Status[output_distinct_sub_bucket_rank_count[i] + input_distinct_sub_bucket_rank_count[i]];

        if (input_bucket_map[i] == 1)
        {
            for (int r = 0; r < output_distinct_sub_bucket_rank_count[i]; r++)
            {
                int buffer_size = input_buffer_size[i];
                MPI_Isend(&buffer_size, 1, MPI_INT, output_distinct_sub_bucket_rank[i][r], 123, mcomm, &req1[req_counter1]);
                req_counter1++;
            }
        }

        if (output_bucket_map[i] == 1)
        {
            for (int r = 0; r < input_distinct_sub_bucket_rank_count[i]; r++)
            {
                MPI_Irecv(meta_buffer_size[i] + r, 1, MPI_INT, input_distinct_sub_bucket_rank[i][r], 123, mcomm, &req1[req_counter1]);
                req_counter1++;
            }
        }

        MPI_Waitall(req_counter1, req1, stat1);


        bucket_offset[i] = *total_buffer_size;
        for (int r = 0; r < input_distinct_sub_bucket_rank_count[i]; r++)
            *total_buffer_size = *total_buffer_size + meta_buffer_size[i][r];

        delete[] req1;
        delete[] stat1;
    }


#if 0

    // Code to verify that the intra-bucket comm is setup correctly
    u64 global_send_buffer_size1 = 0;
    u64 global_send_buffer_size2 = 0;
    MPI_Allreduce(&total_send_buffer_size, &global_send_buffer_size1, 1, MPI_UNSIGNED_LONG_LONG, MPI_SUM, mcomm);
    MPI_Allreduce(total_buffer_size, &global_send_buffer_size2, 1, MPI_UNSIGNED_LONG_LONG, MPI_SUM, mcomm);
    std::cout << "total_send_buffer_size = " << total_send_buffer_size << std::endl;
    std::cout << "*total_buffer_size = " << *total_buffer_size << std::endl;

    assert(global_send_buffer_size1 == global_send_buffer_size2);
    if (rank == 0)
        std::cout << "[VERIFY intra_bucket] " << global_send_buffer_size1 << " " << global_send_buffer_size2 << std::endl;
#endif


    /// Actual data Exchange

    // Allocate buffer
    *recvbuf = new u64[*total_buffer_size];

    // Non-blocking point-to-point data exchange
    for (u32 i = 0; i < buckets; i++)
    {
        u32 req_counter2 = 0;
        MPI_Request *req2 = new MPI_Request[output_distinct_sub_bucket_rank_count[i] + input_distinct_sub_bucket_rank_count[i]];
        MPI_Status *stat2 = new MPI_Status[output_distinct_sub_bucket_rank_count[i] + input_distinct_sub_bucket_rank_count[i]];

        // data
        if (input_bucket_map[i] == 1)
        {
            for (int r = 0; r < output_distinct_sub_bucket_rank_count[i]; r++)
            {
                if (input_buffer_size[i] != 0)
                {
                    MPI_Isend(input_buffer[i].buffer, input_buffer_size[i], MPI_UNSIGNED_LONG_LONG, output_distinct_sub_bucket_rank[i][r], 123, mcomm, &req2[req_counter2]);
                    req_counter2++;
                }
            }
        }

        u32 offset = 0;
        if (output_bucket_map[i] == 1)
        {
            for (int r = 0; r < input_distinct_sub_bucket_rank_count[i]; r++)
            {
                if (meta_buffer_size[i][r] != 0)
                {
                    MPI_Irecv((*recvbuf) + offset + bucket_offset[i], meta_buffer_size[i][r], MPI_UNSIGNED_LONG_LONG, input_distinct_sub_bucket_rank[i][r], 123, mcomm, &req2[req_counter2]);
                    offset = offset + meta_buffer_size[i][r];
                    req_counter2++;
                }
            }
        }

        MPI_Waitall(req_counter2, req2, stat2);
        input_buffer[i].vector_buffer_free();

        delete[] req2;
        delete[] stat2;
        delete[] meta_buffer_size[i];
    }
    delete[] meta_buffer_size;
    delete[] input_buffer;
    delete[] input_buffer_size;
    delete[] bucket_offset;

    return;
}
#else


void intra_bucket_comm(u32 buckets,
                       shmap_relation *rel,
                       int* input_distinct_sub_bucket_rank_count, int** input_distinct_sub_bucket_rank, u32* input_bucket_map,
                       int* output_distinct_sub_bucket_rank_count, int** output_distinct_sub_bucket_rank, u32* output_bucket_map,
                       u64 *total_buffer_size, u64 **recvbuf,
                       MPI_Comm mcomm)
{
    // buffer to hold relation data to be sent out
    vector_buffer *input_buffer = new vector_buffer[buckets];
    int *input_buffer_size = new int[buckets];

    //std::cout << "Buckets " << buckets << std::endl;

    u32** meta_buffer_size = new u32*[buckets];
    memset(meta_buffer_size, 0, sizeof(u32*) * buckets);

    *total_buffer_size = 0;
    u32* bucket_offset = new u32[buckets];

    int rank;
    MPI_Comm_rank(mcomm, &rank);

    u64 total_send_buffer_size = 0;
    for (u32 i = 0; i < buckets; i++)
    {
        // Buffer to store relation data
        input_buffer[i].vector_buffer_create_empty();

        // Puts btree data into a vector
        std::vector<u64> prefix = {};
        rel[i].as_vector_buffer_recursive(&(input_buffer[i]), prefix);

        // size of data to be sent
        input_buffer_size[i] = (&input_buffer[i])->size / sizeof(u64);
        total_send_buffer_size = total_send_buffer_size + input_buffer_size[i];

        meta_buffer_size[i] = new u32[input_distinct_sub_bucket_rank_count[i]];
        memset(meta_buffer_size[i], 0, sizeof(u32) * input_distinct_sub_bucket_rank_count[i]);

        u32 req_counter1 = 0;
        MPI_Request *req1 = new MPI_Request[output_distinct_sub_bucket_rank_count[i] + input_distinct_sub_bucket_rank_count[i]];
        MPI_Status *stat1 = new MPI_Status[output_distinct_sub_bucket_rank_count[i] + input_distinct_sub_bucket_rank_count[i]];

        if (input_bucket_map[i] == 1)
        {
            for (int r = 0; r < output_distinct_sub_bucket_rank_count[i]; r++)
            {
                int buffer_size = input_buffer_size[i];
                MPI_Isend(&buffer_size, 1, MPI_INT, output_distinct_sub_bucket_rank[i][r], 123, mcomm, &req1[req_counter1]);
                req_counter1++;
            }
        }

        if (output_bucket_map[i] == 1)
        {
            for (int r = 0; r < input_distinct_sub_bucket_rank_count[i]; r++)
            {
                MPI_Irecv(meta_buffer_size[i] + r, 1, MPI_INT, input_distinct_sub_bucket_rank[i][r], 123, mcomm, &req1[req_counter1]);
                req_counter1++;
            }
        }

        MPI_Waitall(req_counter1, req1, stat1);


        bucket_offset[i] = *total_buffer_size;
        for (int r = 0; r < input_distinct_sub_bucket_rank_count[i]; r++)
            *total_buffer_size = *total_buffer_size + meta_buffer_size[i][r];

        delete[] req1;
        delete[] stat1;
    }


#if 0

    // Code to verify that the intra-bucket comm is setup correctly
    u64 global_send_buffer_size1 = 0;
    u64 global_send_buffer_size2 = 0;
    MPI_Allreduce(&total_send_buffer_size, &global_send_buffer_size1, 1, MPI_UNSIGNED_LONG_LONG, MPI_SUM, mcomm);
    MPI_Allreduce(total_buffer_size, &global_send_buffer_size2, 1, MPI_UNSIGNED_LONG_LONG, MPI_SUM, mcomm);
    std::cout << "total_send_buffer_size = " << total_send_buffer_size << std::endl;
    std::cout << "*total_buffer_size = " << *total_buffer_size << std::endl;

    assert(global_send_buffer_size1 == global_send_buffer_size2);
    if (rank == 0)
        std::cout << "[VERIFY intra_bucket] " << global_send_buffer_size1 << " " << global_send_buffer_size2 << std::endl;
#endif


    /// Actual data Exchange

    // Allocate buffer
    *recvbuf = new u64[*total_buffer_size];

    // Non-blocking point-to-point data exchange
    for (u32 i = 0; i < buckets; i++)
    {
        u32 req_counter2 = 0;
        MPI_Request *req2 = new MPI_Request[output_distinct_sub_bucket_rank_count[i] + input_distinct_sub_bucket_rank_count[i]];
        MPI_Status *stat2 = new MPI_Status[output_distinct_sub_bucket_rank_count[i] + input_distinct_sub_bucket_rank_count[i]];

        // data
        if (input_bucket_map[i] == 1)
        {
            for (int r = 0; r < output_distinct_sub_bucket_rank_count[i]; r++)
            {
                if (input_buffer_size[i] != 0)
                {
                    MPI_Isend(input_buffer[i].buffer, input_buffer_size[i], MPI_UNSIGNED_LONG_LONG, output_distinct_sub_bucket_rank[i][r], 123, mcomm, &req2[req_counter2]);
                    req_counter2++;
                }
            }
        }

        u32 offset = 0;
        if (output_bucket_map[i] == 1)
        {
            for (int r = 0; r < input_distinct_sub_bucket_rank_count[i]; r++)
            {
                if (meta_buffer_size[i][r] != 0)
                {
                    MPI_Irecv((*recvbuf) + offset + bucket_offset[i], meta_buffer_size[i][r], MPI_UNSIGNED_LONG_LONG, input_distinct_sub_bucket_rank[i][r], 123, mcomm, &req2[req_counter2]);
                    offset = offset + meta_buffer_size[i][r];
                    req_counter2++;
                }
            }
        }

        MPI_Waitall(req_counter2, req2, stat2);
        input_buffer[i].vector_buffer_free();

        delete[] req2;
        delete[] stat2;
        delete[] meta_buffer_size[i];
    }
    delete[] meta_buffer_size;
    delete[] input_buffer;
    delete[] input_buffer_size;
    delete[] bucket_offset;

    return;
}
#endif
