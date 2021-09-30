/*
 * Function to abstract all to all communication
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */


#pragma once

#if 0
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
    u64 *local_compute_buffer;

    /// local_compute_output_size is of size RA_list size x nprocs, this contains size of data per RA rule for per process
    int **local_compute_output_size;


    int* local_compute_output_size_flat;

    int local_compute_output_size_total;

    /// cumulative_tuple_process_map is of size nprocs, cumulative_tuple_process_map[i] contains number of tuples to be transmitted to rank-i process, across all Relation Algebra (RA) rules
    int *cumulative_tuple_process_map;
};
#endif



//void all_to_all_comm(vector_buffer* vectorized_send_buffer, int vectorized_send_buffer_size, int* send_counts, int *outer_hash_buffer_size, u64 **recv_buffer, MPI_Comm comm);

//void comm_compaction_all_to_all(all_to_allv_buffer compute_buffer, int **recv_buffer_offset_size, u64 **recv_buffer, MPI_Comm comm);
