/*
 * Function to abstract all to all communication
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */


#pragma once
/// Struct to transmit data for multiple relations at once for an all to all communication
struct all_to_allv_buffer
{
    /// Number of Relations it is compacting (for comm compaction)
    int ra_count;

    /// Number of buffers to create = number of processes
    int nprocs;

    /// Number of tuples for each of the relation
    int *width;

    /// local_compute_output is of size RA_list size x nprocs, this contains data per RA rule for per process
    vector_buffer** local_compute_output;
    int* local_compute_output_size_rel;

    /// local_compute_output_size is of size RA_list size x nprocs, this contains size of data per RA rule for per process
    int **local_compute_output_size;


    // TODO: 
    /// This is replaced by `local_compute_output_count_flat`, and can be deleted, except 
    /// it is being used in `all_to_all_benchmark.cpp`.
    int* local_compute_output_size_flat;

    int* local_compute_output_count_flat;

    int local_compute_output_size_total;

    /// cumulative_tuple_process_map is of size nprocs, cumulative_tuple_process_map[i] contains number of tuples to be transmitted to rank-i process, across all Relation Algebra (RA) rules
    int *cumulative_tuple_process_map;
};



/// Struct to transmit data for multiple relations at once for an all to all communication
struct all_to_all_buffer
{
    /// Number of Relations it is compacting (for comm compaction)
    int ra_count;

    /// Number of buffers to create = number of processes
    int nprocs;

    /// Number of tuples for each of the relation
    int *width;

    int threshold;

    /// local_compute_output is of size RA_list size x nprocs, this contains data per RA rule for per process
    u64 *local_compute_output;

    int **local_compute_output_size;
};



/// Function to perform all to all comm
/// input:
///     vectorized_send_buffer: vector_buffer of size nprocs, holding interleaved data that needs to be transmitted to every other process
///     vectorized_send_buffer_size: size of buffer to send
///     send_counts: size of the strided buffer
///
/// output:
///     outer_hash_buffer_size: size of data: size of data received after all to all comm
///     outer_hash_buffer: buffer that holds data after all to all comm
void all_to_all_comm(vector_buffer* vectorized_send_buffer, int vectorized_send_buffer_size, int* send_counts, int *outer_hash_buffer_size, u64 **recv_buffer, MPI_Comm comm);

void comm_compaction_all_to_all(all_to_allv_buffer compute_buffer, int **recv_buffer_counts, u64 **recv_buffer, MPI_Comm comm);
