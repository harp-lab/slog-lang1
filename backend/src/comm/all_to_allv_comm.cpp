/*
 * Function to abstract all to all communication
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */


#include "../parallel_RA_inc.h"
#include <unistd.h>



void all_to_all_comm(vector_buffer* vectorized_send_buffer, int vectorized_send_buffer_size, int* send_counts, int *recv_buffer_size, u64 **recv_buffer, MPI_Comm comm)
{
    int nprocs;
    MPI_Comm_size(comm, &nprocs);

    /// send_counts ----> recv_counts
    int* recv_counts = new int[nprocs];
    memset(recv_counts, 0, nprocs * sizeof(int));
    MPI_Alltoall(send_counts, 1, MPI_INT, recv_counts, 1, MPI_INT, comm);

    /// creating send and recv displacements
    int* send_displacements = new int[nprocs];
    int* recv_displacements = new int[nprocs];

    /// creating send_buffer
    u64* send_buffer = new u64[vectorized_send_buffer_size];

    /// Populating send, recv, and data buffer
    recv_displacements[0] = 0;
    send_displacements[0] = 0;
    *recv_buffer_size = recv_counts[0];
    memcpy(send_buffer, (&vectorized_send_buffer[0])->buffer, (&vectorized_send_buffer[0])->size);

    vectorized_send_buffer[0].vector_buffer_free();
    for (int i = 1; i < nprocs; i++)
    {
        send_displacements[i] = send_displacements[i - 1] + send_counts[i - 1];
        recv_displacements[i] = recv_displacements[i - 1] + recv_counts[i - 1];

        *recv_buffer_size = *recv_buffer_size + recv_counts[i];

        memcpy(send_buffer + send_displacements[i], (&vectorized_send_buffer[i])->buffer, (&vectorized_send_buffer[i])->size);
        vectorized_send_buffer[i].vector_buffer_free();
    }
    //std::cout << "vectorized_send_buffer_size" << vectorized_send_buffer_size << std::endl;
    assert(send_displacements[nprocs - 1] + send_counts[nprocs - 1] == vectorized_send_buffer_size);

    /// creating recv_buffer
    *recv_buffer = new u64[*recv_buffer_size];

    /// Actual data transfer
    MPI_Alltoallv(send_buffer, send_counts, send_displacements, MPI_UNSIGNED_LONG_LONG, *recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm);

    /// cleanup
    delete[] recv_counts;
    delete[] recv_displacements;
    delete[] send_displacements;
    delete[] send_buffer;

    return;
}



void comm_compaction_all_to_all(all_to_allv_buffer compute_buffer, int **recv_buffer_counts, u64 **recv_buffer, MPI_Comm comm)
{
    u32 RA_count = compute_buffer.ra_count;
    int nprocs = compute_buffer.nprocs;
    int rank;
    MPI_Comm_rank(comm, &rank);

    *recv_buffer_counts = new int[RA_count * nprocs];
    memset(*recv_buffer_counts, 0, RA_count * nprocs * sizeof(int));

    MPI_Alltoall(compute_buffer.local_compute_output_count_flat, RA_count, MPI_INT, *recv_buffer_counts, RA_count, MPI_INT, comm);

    int outer_hash_buffer_size = 0;
    int *send_disp = new int[nprocs];
    int *recv_counts = new int[nprocs];
    int *recv_displacements = new int[nprocs];

    recv_counts[0] = 0;
    send_disp[0] = 0;
    recv_displacements[0] = 0;

    u64* send_buffer = new u64[compute_buffer.local_compute_output_size_total];

    u32 boffset = 0;
    int local_max_count = 0;
    int sum0 = 0;
    int local_min_count = compute_buffer.cumulative_tuple_process_map[0];
    for(int i = 0; i < nprocs; i++)
    {
        sum0 = sum0+compute_buffer.cumulative_tuple_process_map[i];
        recv_counts[i] = 0;

        if (compute_buffer.cumulative_tuple_process_map[i] < local_min_count)
            local_min_count = compute_buffer.cumulative_tuple_process_map[i];

        if (compute_buffer.cumulative_tuple_process_map[i] > local_max_count)
            local_max_count = compute_buffer.cumulative_tuple_process_map[i];

        if (i >= 1)
            send_disp[i] = send_disp[i - 1] + compute_buffer.cumulative_tuple_process_map[i - 1];

        for (u32 r = 0; r < RA_count; r++)
        {
            memcpy(send_buffer + boffset, compute_buffer.local_compute_output[r][i].buffer, compute_buffer.local_compute_output[r][i].size);
            boffset = boffset + (compute_buffer.local_compute_output[r][i].size)/sizeof(u64);
            compute_buffer.local_compute_output[r][i].vector_buffer_free();

            recv_counts[i] = recv_counts[i] + (*recv_buffer_counts)[i*RA_count + r] * compute_buffer.width[r];

            assert(compute_buffer.local_compute_output_size_flat[i*RA_count + r] == 
                   compute_buffer.local_compute_output_count_flat[i*RA_count + r] * compute_buffer.width[r]);
        }

        if (i >= 1)
            recv_displacements[i] = recv_displacements[i - 1] + recv_counts[i - 1];
        outer_hash_buffer_size = outer_hash_buffer_size + recv_counts[i];
    }

    *recv_buffer = new u64[outer_hash_buffer_size];

    int max_send_count = 0;
    int average_send_count = 0;
    MPI_Allreduce(&local_max_count, &max_send_count, 1, MPI_INT, MPI_MAX, comm);
    MPI_Allreduce(&local_max_count, &average_send_count, 1, MPI_INT, MPI_SUM, comm);

    MPI_Alltoallv(send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, *recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm);

    delete[] send_buffer;
    delete[] send_disp;
    delete[] recv_displacements;
    delete[] recv_counts;
}