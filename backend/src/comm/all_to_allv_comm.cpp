/*
 * Function to abstract all to all communication
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



///


/*

  Revisiting Bruck Algorithm for Non-uniform All-to-all Data Communincation
  Abstract
  Introduction

  Related work --- Ke
    SLOAV
    Zero-copy and variants
    Persistent communincation (what?) --- can it beused for a true non-uniform all_to_allv?
  Technique
    technique-1: padding
    technique-2: extra meta-data comm phase + buffer allocation
    Diagrams and algorithms
  Microbenchmark evaluation
    Degree on non-uniformity
    Weak scaling
    Strong scaling
    Padding vs non-padding
  Appplication
    Graph mining
    Program analysis
    Evaluation
  Conclusion

*/



#include "../parallel_RA_inc.h"
#include <unistd.h>
static void sloav_non_uniform_benchmark(char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm, double **running_a2a_find_count_time, double **running_a2a_create_rindex_time, double **running_a2a_total_find_blocks_time, double **running_a2a_total_pre_time, double **running_a2a_total_send_meda_time, double **running_a2a_total_comm_time, double **running_a2a_total_replace_time, double **running_a2a_exchange_time, double **running_a2a_filter_time, int task_id, int loop_counter);

static void sloav_non_uniform_benchmark_with_caching(char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm, double **running_a2a_find_count_time, double **running_a2a_create_rindex_time, double **running_a2a_total_find_blocks_time, double **running_a2a_total_pre_time, double **running_a2a_total_send_meda_time, double **running_a2a_total_comm_time, double **running_a2a_total_replace_time, double **running_a2a_exchange_time, double **running_a2a_filter_time, int task_id, int loop_counter, int* rotate_index_array, int** send_indexes, int local_max_count, int *sendb_num);

static void padded_bruck_non_uniform_benchmark(char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm);



void all_to_all_comm(vector_buffer* vectorized_send_buffer, int vectorized_send_buffer_size, int* send_counts, int *recv_buffer_size, u64 **recv_buffer, MPI_Comm comm)
{
    // printf("all_to_all_comm called!\n");
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



void comm_compaction_all_to_all(all_to_allv_buffer compute_buffer, int **recv_buffer_counts, u64 **recv_buffer, MPI_Comm comm, int loop_counter, int task_id, std::string output_dir, bool record, int sloav_mode, int* rotate_index_array, int** send_indexes, int *sendb_num)
{
    // printf("comm_compaction_all_to_all called! pid: %d \n", getpid());
    u32 RA_count = compute_buffer.ra_count;
    int nprocs = compute_buffer.nprocs;
    // printf("RA_count: %d, nprocs: %d \n", RA_count, nprocs);
    int rank;
    MPI_Comm_rank(comm, &rank);

    *recv_buffer_counts = new int[RA_count * nprocs];
    memset(*recv_buffer_counts, 0, RA_count * nprocs * sizeof(int));

    // printf("compute_buffer widths: ");
    // for (int _i = 0; _i < RA_count; _i++) {printf("%d, ", compute_buffer.width[_i]);}
    // printf("\n");

    // printf("compute_buffer.local_compute_output_count_flat: ");
    // for (int _i = 0; _i < RA_count * nprocs; _i++) {printf("%d, ", compute_buffer.local_compute_output_count_flat[_i]);}
    // printf("\n");

    // printf("calling MPI_Alltoall for counts!\n");
    MPI_Alltoall(compute_buffer.local_compute_output_count_flat, RA_count, MPI_INT, *recv_buffer_counts, RA_count, MPI_INT, comm);
    // printf("called MPI_Alltoall for counts!\n");

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

            // printf("(*recv_buffer_counts)[i*RA_count + r] for i=%d, r=%d is %d\n", i, r, (*recv_buffer_counts)[i*RA_count + r]);
            // recv_counts[i] = recv_counts[i] + (*recv_buffer_offset_size)[i*RA_count + r];
            recv_counts[i] = recv_counts[i] + (*recv_buffer_counts)[i*RA_count + r] * compute_buffer.width[r];

            assert(compute_buffer.local_compute_output_size_flat[i*RA_count + r] == 
                   compute_buffer.local_compute_output_count_flat[i*RA_count + r] * compute_buffer.width[r]);
        }

        if (i >= 1)
            recv_displacements[i] = recv_displacements[i - 1] + recv_counts[i - 1];
        outer_hash_buffer_size = outer_hash_buffer_size + recv_counts[i];
    }
    // printf("done looping!\n");

    // printf("recv_counts: ");
    // for (int _i = 0; _i< nprocs; _i++) printf("%d, ", recv_counts[_i]);
    // printf("\n");


    //if (rank == 0)
    //    std::cout  <<local_min_count << "\t" << local_max_count << "\t" << sum0/nprocs << "\t";

    //assert(compute_buffer.local_compute_output_size_total == send_disp[nprocs - 1] + compute_buffer.cumulative_tuple_process_map[nprocs - 1]);
    // printf("recv_buffer size: %d \n", outer_hash_buffer_size);
    *recv_buffer = new u64[outer_hash_buffer_size];

#if 0
    if (record == true)
    {

        std::string dir_name;
        dir_name = output_dir + "/checkpoint-" + std::to_string(task_id) + "-" + std::to_string(loop_counter);
        //dir_name = output_dir + "/checkpoint-" + std::to_string(loop_counter);
        std::string scc_metadata;
        scc_metadata = dir_name + "/a2a_" + std::to_string(rank);

        FILE *fp;
        fp = fopen(scc_metadata.c_str(), "w");
        for (int i=0; i< compute_buffer.nprocs; i++)
            fprintf (fp, "%d\n", compute_buffer.cumulative_tuple_process_map[i]);
        fclose(fp);


        memcpy(all_to_all_time[task_id][loop_counter], compute_buffer.cumulative_tuple_process_map, compute_buffer.nprocs * sizeof(int));
        //std::cout << "TID " << task_id << " Loop counter " << loop_counter << " -> " << all_to_all_time[task_id][loop_counter][0] << " " << all_to_all_time[task_id][loop_counter][1] << std::endl;
    }
#endif

    int max_send_count = 0;
    int average_send_count = 0;
    // printf("calling MPI_Allreduce!\n");
    MPI_Allreduce(&local_max_count, &max_send_count, 1, MPI_INT, MPI_MAX, comm);
    MPI_Allreduce(&local_max_count, &average_send_count, 1, MPI_INT, MPI_SUM, comm);
    // printf("calling MPI_Allreduce twice done!\n");

#if 0
    if (sloav_mode == 1)
    {
        //if (rank == 0)
        //    std::cout << "M1 \t" << max_send_count << "\t" << average_send_count/nprocs << "\t";
        sloav_non_uniform_benchmark((char*)send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, (char*)*recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm,
                                running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, task_id, loop_counter);
    }
    else
#endif
        if (sloav_mode == 0)
    {
        //if (rank == 0)
        //    std::cout << "M0 \t" << max_send_count << "\t" << average_send_count/nprocs << "\t";
        // printf("calling MPI_Alltoallv, at %s:%s\n", __FILE__, __LINE__);

        // printf("compute_buffer.cumulative_tuple_process_map: ");
        // for(int _i = 0; _i< nprocs; _i++) printf("%d, ", compute_buffer.cumulative_tuple_process_map[_i]);
        // printf("\n");

        // printf("calling MPI_Alltoallv,for data! \n");
        MPI_Alltoallv(send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, *recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm);
        // printf("calling MPI_Alltoallv,for data done!\n");

    }
#if 0
    else if (sloav_mode == 2)
        sloav_non_uniform_benchmark_with_caching((char*)send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, (char*)*recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm,
                                running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, task_id, loop_counter, rotate_index_array, send_indexes, local_max_count, sendb_num);
    else if (sloav_mode == 3)
    {
        //if (rank == 0)
        //    std::cout << "M3 \t" << max_send_count << "\t" << average_send_count/nprocs << "\t";
        padded_bruck_non_uniform_benchmark((char*)send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, (char*)*recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm);
    }
#endif
    //if (outer_hash_buffer_size != 0)
    //{
    //    std::cout << "compute_buffer.cumulative_tuple_process_map " << compute_buffer.cumulative_tuple_process_map[0] << std::endl;
    //    std::cout << "send_buffer " << send_buffer[0] << " " << send_buffer[1] << std::endl;
    //    std::cout << "*recv_buffer " << (*recv_buffer)[0] << " " << (*recv_buffer)[1] << std::endl;
    //}
    //std::cout << "Buffer size " << outer_hash_buffer_size << "Buffer "  << "A2A: " << *recv_buffer[0] << " " << *recv_buffer[1] << " " << std::endl;
        //static void padded_bruck_non_uniform_benchmark(char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm)


#if 0
    else if (sloav_mode == 3)   // Based on figure 7
    {
        int max_send_count = 0;
        int nprocs;
        MPI_Comm_size(comm, &nprocs);
        MPI_Allreduce(&local_max_count, &max_send_count, 1, MPI_INT, MPI_MAX, comm);
        if (nprocs == 128)
        {
            if (max_send_count < 128) // add max_Send_count
            {
                if (rank == 0)
                    std::cout << "M31 \t" << max_send_count << "\t";
                sloav_non_uniform_benchmark((char*)send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, (char*)*recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm,
                                        running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, task_id, loop_counter);
            }
            else
            {
                if (rank == 0)
                    std::cout << "M30 \t" << max_send_count << "\t";
                MPI_Alltoallv(send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, *recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm);
            }

        }
        else if (nprocs == 256)
        {
            if (max_send_count  < 115)
            {
                if (rank == 0)
                    std::cout << "M31 \t" << max_send_count << "\t";
                sloav_non_uniform_benchmark((char*)send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, (char*)*recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm,
                                        running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, task_id, loop_counter);
            }
            else
            {
                if (rank == 0)
                    std::cout << "M30 \t" << max_send_count << "\t";
                MPI_Alltoallv(send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, *recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm);
            }

        }
        if (nprocs == 512)
        {
            if (max_send_count < 128)
            {
                if (rank == 0)
                    std::cout << "M31 \t" << max_send_count << "\t";
                sloav_non_uniform_benchmark((char*)send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, (char*)*recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm,
                                        running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, task_id, loop_counter);
            }
            else
            {
                if (rank == 0)
                    std::cout << "M30 \t" << max_send_count << "\t";
                MPI_Alltoallv(send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, *recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm);
            }

        }
        if (nprocs == 1024)
        {
            if (max_send_count < 153)
            {
                if (rank == 0)
                    std::cout << "M31 \t" << max_send_count << "\t";
                sloav_non_uniform_benchmark((char*)send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, (char*)*recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm,
                                        running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, task_id, loop_counter);
            }
            else
            {
                if (rank == 0)
                    std::cout << "M30 \t" << max_send_count << "\t";
                MPI_Alltoallv(send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, *recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm);
            }

        }
        if (nprocs == 2048)
        {
            if (max_send_count < 128)
            {
                if (rank == 0)
                    std::cout << "M31 \t" << max_send_count << "\t";
                sloav_non_uniform_benchmark((char*)send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, (char*)*recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm,
                                        running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, task_id, loop_counter);
            }
            else
            {
                if (rank == 0)
                    std::cout << "M30 \t" << max_send_count << "\t";
                MPI_Alltoallv(send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, *recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm);
            }

        }
        if (nprocs == 4096)
        {
            if (max_send_count < 115)
            {
                if (rank == 0)
                    std::cout << "M31 \t" << max_send_count << "\t";
                sloav_non_uniform_benchmark((char*)send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, (char*)*recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm,
                                        running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, task_id, loop_counter);
            }
            else
            {
                if (rank == 0)
                    std::cout << "M30 \t" << max_send_count << "\t";
                MPI_Alltoallv(send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, *recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm);
            }

        }
        if (nprocs == 8192)
        {
            if (max_send_count < 58)
            {
                if (rank == 0)
                    std::cout << "M31 \t" << max_send_count << "\t";
                sloav_non_uniform_benchmark((char*)send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, (char*)*recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm,
                                        running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, task_id, loop_counter);
            }
            else
            {
                if (rank == 0)
                    std::cout << "M30 \t" << max_send_count << "\t";
                MPI_Alltoallv(send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, *recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm);
            }

        }
        if (nprocs == 16384)
        {
            if (max_send_count  < 16)
            {
                if (rank == 0)
                    std::cout << "M31 \t" << max_send_count << "\t";
                sloav_non_uniform_benchmark((char*)send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, (char*)*recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm,
                                        running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, task_id, loop_counter);
            }
            else
            {
                if (rank == 0)
                    std::cout << "M30 \t" << max_send_count << "\t";
                MPI_Alltoallv(send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, *recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm);
            }

        }
    }
#endif
#if 0
    else if (sloav_mode == 4) // Based on your new figure
    {
        int max_send_count = 0;
        int nprocs;
        MPI_Comm_size(comm, &nprocs);
        MPI_Allreduce(&local_max_count, &max_send_count, 1, MPI_INT, MPI_MAX, comm);
        if (nprocs == 128)
        {
            if (max_send_count  < 90)
            {
                if (rank == 0)
                    std::cout << "M41 \t" << max_send_count << "\t";
                sloav_non_uniform_benchmark((char*)send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, (char*)*recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm,
                                        running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, task_id, loop_counter);
            }
            else
            {
                if (rank == 0)
                    std::cout << "M40 \t" << max_send_count << "\t";
                MPI_Alltoallv(send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, *recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm);
            }

        }
        else if (nprocs == 256)
        {
            if (max_send_count < 128)
            {
                if (rank == 0)
                    std::cout << "M41 \t" << max_send_count << "\t";
                sloav_non_uniform_benchmark((char*)send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, (char*)*recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm,
                                        running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, task_id, loop_counter);
            }
            else
            {
                if (rank == 0)
                    std::cout << "M40 \t" << max_send_count << "\t";
                MPI_Alltoallv(send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, *recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm);
            }

        }
        if (nprocs == 512)
        {
            if (max_send_count < 115)
            {
                if (rank == 0)
                    std::cout << "M41 \t" << max_send_count << "\t";
                sloav_non_uniform_benchmark((char*)send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, (char*)*recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm,
                                        running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, task_id, loop_counter);
            }
            else
            {
                if (rank == 0)
                    std::cout << "M40 \t" << max_send_count << "\t";
                MPI_Alltoallv(send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, *recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm);
            }

        }
        if (nprocs == 1024)
        {
            if (max_send_count < 141)
            {
                if (rank == 0)
                    std::cout << "M41 \t" << max_send_count << "\t";
                sloav_non_uniform_benchmark((char*)send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, (char*)*recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm,
                                        running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, task_id, loop_counter);
            }
            else
            {
                if (rank == 0)
                    std::cout << "M40 \t" << max_send_count << "\t";
                MPI_Alltoallv(send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, *recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm);
            }

        }
        if (nprocs == 2048)
        {
            if (max_send_count < 115)
            {
                if (rank == 0)
                    std::cout << "M41 \t" << max_send_count << "\t";
                sloav_non_uniform_benchmark((char*)send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, (char*)*recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm,
                                        running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, task_id, loop_counter);
            }
            else
            {
                if (rank == 0)
                    std::cout << "M40 \t" << max_send_count << "\t";
                MPI_Alltoallv(send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, *recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm);
            }

        }
        if (nprocs == 4096)
        {
            if (max_send_count < 102)
            {
                if (rank == 0)
                    std::cout << "M41 \t" << max_send_count << "\t";
                sloav_non_uniform_benchmark((char*)send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, (char*)*recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm,
                                        running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, task_id, loop_counter);
            }
            else
            {
                if (rank == 0)
                    std::cout << "M40 \t" << max_send_count << "\t";
                MPI_Alltoallv(send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, *recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm);
            }

        }
        if (nprocs == 8192)
        {
            if (max_send_count < 64)
            {
                if (rank == 0)
                    std::cout << "M41 \t" << max_send_count << "\t";
                sloav_non_uniform_benchmark((char*)send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, (char*)*recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm,
                                        running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, task_id, loop_counter);
            }
            else
            {
                if (rank == 0)
                    std::cout << "M40 \t" << max_send_count << "\t";
                MPI_Alltoallv(send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, *recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm);
            }

        }
        if (nprocs == 16384)
        {
            if (max_send_count < 16)
            {
                if (rank == 0)
                    std::cout << "M41 \t" << max_send_count << "\t";
                sloav_non_uniform_benchmark((char*)send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, (char*)*recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm,
                                        running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, task_id, loop_counter);
            }
            else
            {
                if (rank == 0)
                    std::cout << "M40 \t" << max_send_count << "\t";
                MPI_Alltoallv(send_buffer, compute_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, *recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm);
            }

        }
    }
#endif


    delete[] send_buffer;
    delete[] send_disp;
    delete[] recv_displacements;
    delete[] recv_counts;

    // printf("comm_compaction_all_to_all done!\n");
}

#if 0
static void sloav_non_uniform_benchmark(char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm, double **running_a2a_find_count_time, double **running_a2a_create_rindex_time, double **running_a2a_total_find_blocks_time, double **running_a2a_total_pre_time, double **running_a2a_total_send_meda_time, double **running_a2a_total_comm_time, double **running_a2a_total_replace_time, double **running_a2a_exchange_time, double **running_a2a_filter_time, int task_id, int loop_counter)
{
    int rank, nprocs;
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &nprocs);

    int typesize;
    MPI_Type_size(sendtype, &typesize);

    // 1. Find max send count
    double find_count_start = MPI_Wtime();
    int local_max_count = 0;
    for (int i = 0; i < nprocs; i++)
    {
        if (sendcounts[i] > local_max_count)
            local_max_count = sendcounts[i];
    }
    int max_send_count = 0;
    MPI_Allreduce(&local_max_count, &max_send_count, 1, MPI_INT, MPI_MAX, comm);
    double find_count_end = MPI_Wtime();
    running_a2a_find_count_time[task_id][loop_counter] = find_count_end - find_count_start;

    // 2. create local index array after rotation
    double create_rindex_start = MPI_Wtime();
    int rotate_index_array[nprocs];
    for (int i = 0; i < nprocs; i++)
        rotate_index_array[i] = (2*rank-i+nprocs)%nprocs;
    double create_rindex_end = MPI_Wtime();
    running_a2a_create_rindex_time[task_id][loop_counter] = create_rindex_end - create_rindex_start;

    // 3. exchange data with log(P) steps
    double exchange_start = MPI_Wtime();
    int max_send_elements = (nprocs+1)/2;
    char* extra_buffer = (char*) malloc(max_send_count*typesize*nprocs);
    char* temp_send_buffer = (char*) malloc(max_send_count*typesize*max_send_elements);
    char* temp_recv_buffer = (char*) malloc(max_send_count*typesize*max_send_elements);
    int pos_status[nprocs];
    memset(pos_status, 0, nprocs*sizeof(int));
    for (int k = 1; k < nprocs; k <<= 1)
    {
        // 1) find which data blocks to send
        double find_blocks_start = MPI_Wtime();
        int send_indexes[max_send_elements];
        int sendb_num = 0;
        for (int i = 1; i < nprocs; i++)
        {
            if (i & k)
                send_indexes[sendb_num++] = (rank+i)%nprocs;
        }
        double find_blocks_end = MPI_Wtime();
        running_a2a_total_find_blocks_time[task_id][loop_counter] += (find_blocks_end - find_blocks_start);

        // 2) prepare metadata and send buffer
        double pre_send_start = MPI_Wtime();
        int metadata_send[sendb_num+1];
        int sendCount = 0;
        int offset = 0;
        for (int i = 0; i < sendb_num; i++)
        {
            int send_index = rotate_index_array[send_indexes[i]];
            metadata_send[i+1] = sendcounts[send_index];
            sendCount += sendcounts[send_index];
            if (pos_status[send_index] == 0)
                memcpy(&temp_send_buffer[offset], &sendbuf[sdispls[send_index]*typesize], sendcounts[send_index]*typesize);
            else
                memcpy(&temp_send_buffer[offset], &extra_buffer[send_indexes[i]*max_send_count*typesize], sendcounts[send_index]*typesize);
            offset += sendcounts[send_index]*typesize;
        }
        metadata_send[0] = sendCount;
        double pre_send_end = MPI_Wtime();
        running_a2a_total_pre_time[task_id][loop_counter] += pre_send_end - pre_send_start;

        // 3) exchange metadata
        double send_meda_start = MPI_Wtime();
        int sendrank = (rank - k + nprocs) % nprocs;
        int recvrank = (rank + k) % nprocs;
        int metadata_recv[sendb_num+1];
        MPI_Sendrecv(metadata_send, sendb_num+1, MPI_INT, sendrank, 0, metadata_recv, sendb_num+1, MPI_INT, recvrank, 0, comm, MPI_STATUS_IGNORE);
        double send_meda_end = MPI_Wtime();
        running_a2a_total_send_meda_time[task_id][loop_counter] += (send_meda_end - send_meda_start);

        // 4) exchange data
        double comm_start = MPI_Wtime();
        MPI_Sendrecv(temp_send_buffer, sendCount*typesize, MPI_CHAR, sendrank, 1, temp_recv_buffer, metadata_recv[0]*typesize, MPI_CHAR, recvrank, 1, comm, MPI_STATUS_IGNORE);
        double comm_end = MPI_Wtime();
        running_a2a_total_comm_time[task_id][loop_counter] = (comm_end - comm_start);

        // 5) replace
        double replace_start = MPI_Wtime();
        offset = 0;
        for (int i = 0; i < sendb_num; i++)
        {
            int send_index = rotate_index_array[send_indexes[i]];
            memcpy(&extra_buffer[send_indexes[i]*max_send_count*typesize], &temp_recv_buffer[offset], metadata_recv[i+1]*typesize);
            offset += metadata_recv[i+1]*typesize;
            pos_status[send_index] = 1;
            sendcounts[send_index] = metadata_recv[i+1];
        }
        double replace_end = MPI_Wtime();
        running_a2a_total_replace_time[task_id][loop_counter] += (replace_end - replace_start);

    }
    free(temp_send_buffer);
    free(temp_recv_buffer);
    double exchange_end = MPI_Wtime();
    running_a2a_exchange_time[task_id][loop_counter] = (exchange_end - exchange_start);

    double filter_start = MPI_Wtime();
    for (int i = 0; i < nprocs; i++)
    {
        if (rank == i)
            memcpy(&recvbuf[rdispls[i]*typesize], &sendbuf[sdispls[i]*typesize], recvcounts[i]*typesize);
        else
            memcpy(&recvbuf[rdispls[i]*typesize], &extra_buffer[i*max_send_count*typesize], recvcounts[i]*typesize);
    }
    free(extra_buffer);
    double filter_end = MPI_Wtime();
    running_a2a_filter_time[task_id][loop_counter] = filter_end - filter_start;


#if 0
    double max_u_time = 0;
    double total_u_time = u_end - u_start;
    MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, comm);
    if (total_u_time == max_u_time)
    {
        std::cout << "[SLOAV] [" << nprocs << " " << range << " " << max_send_count << "] " << total_u_time << " " << find_count_time << " " << create_rindex_time << " " << exchange_time << " ["
                  << total_find_blocks_time << " " << total_pre_time << " " << total_send_meda_time << " " << total_comm_time << " " << total_replace_time << "] " << filter_time << std::endl;
    }
#endif
}


static void sloav_non_uniform_benchmark_with_caching(char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm, double **running_a2a_find_count_time, double **running_a2a_create_rindex_time, double **running_a2a_total_find_blocks_time, double **running_a2a_total_pre_time, double **running_a2a_total_send_meda_time, double **running_a2a_total_comm_time, double **running_a2a_total_replace_time, double **running_a2a_exchange_time, double **running_a2a_filter_time, int task_id, int loop_counter, int* rotate_index_array, int** send_indexes, int local_max_count, int *sendb_num)
{
    int rank, nprocs;
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &nprocs);

    int typesize;
    MPI_Type_size(sendtype, &typesize);

    // 1. Find max send count
    double find_count_start = MPI_Wtime();
    int max_send_count = 0;
    MPI_Allreduce(&local_max_count, &max_send_count, 1, MPI_INT, MPI_MAX, comm);
    double find_count_end = MPI_Wtime();
    running_a2a_find_count_time[task_id][loop_counter] = find_count_end - find_count_start;

    // 3. exchange data with log(P) steps
    double exchange_start = MPI_Wtime();
    int max_send_elements = (nprocs+1)/2;
    char* extra_buffer = (char*) malloc(max_send_count*typesize*nprocs);
    char* temp_send_buffer = (char*) malloc(max_send_count*typesize*max_send_elements);
    char* temp_recv_buffer = (char*) malloc(max_send_count*typesize*max_send_elements);
    int pos_status[nprocs];
    memset(pos_status, 0, nprocs*sizeof(int));
    for (int k = 1; k < nprocs; k <<= 1)
    {
        // 1) find which data blocks to send
        double find_blocks_start = MPI_Wtime();
        double find_blocks_end = MPI_Wtime();
        running_a2a_total_find_blocks_time[task_id][loop_counter] += (find_blocks_end - find_blocks_start);

        // 2) prepare metadata and send buffer
        double pre_send_start = MPI_Wtime();
        int metadata_send[sendb_num[k]+1];
        int sendCount = 0;
        int offset = 0;
        for (int i = 0; i < sendb_num[k]; i++)
        {
            int send_index = rotate_index_array[send_indexes[k][i]];
            metadata_send[i+1] = sendcounts[send_index];
            sendCount += sendcounts[send_index];
            if (pos_status[send_index] == 0)
                memcpy(&temp_send_buffer[offset], &sendbuf[sdispls[send_index]*typesize], sendcounts[send_index]*typesize);
            else
                memcpy(&temp_send_buffer[offset], &extra_buffer[send_indexes[k][i]*max_send_count*typesize], sendcounts[send_index]*typesize);
            offset += sendcounts[send_index]*typesize;
        }
        metadata_send[0] = sendCount;
        double pre_send_end = MPI_Wtime();
        running_a2a_total_pre_time[task_id][loop_counter] += pre_send_end - pre_send_start;

        // 3) exchange metadata
        double send_meda_start = MPI_Wtime();
        int sendrank = (rank - k + nprocs) % nprocs;
        int recvrank = (rank + k) % nprocs;
        int metadata_recv[sendb_num[k]+1];
        MPI_Sendrecv(metadata_send, sendb_num[k]+1, MPI_INT, sendrank, 0, metadata_recv, sendb_num[k]+1, MPI_INT, recvrank, 0, comm, MPI_STATUS_IGNORE);
        double send_meda_end = MPI_Wtime();
        running_a2a_total_send_meda_time[task_id][loop_counter] += (send_meda_end - send_meda_start);

        // 4) exchange data
        double comm_start = MPI_Wtime();
        MPI_Sendrecv(temp_send_buffer, sendCount*typesize, MPI_CHAR, sendrank, 1, temp_recv_buffer, metadata_recv[0]*typesize, MPI_CHAR, recvrank, 1, comm, MPI_STATUS_IGNORE);
        double comm_end = MPI_Wtime();
        running_a2a_total_comm_time[task_id][loop_counter] = (comm_end - comm_start);

        // 5) replace
        double replace_start = MPI_Wtime();
        offset = 0;
        for (int i = 0; i < sendb_num[k]; i++)
        {
            int send_index = rotate_index_array[send_indexes[k][i]];
            memcpy(&extra_buffer[send_indexes[k][i]*max_send_count*typesize], &temp_recv_buffer[offset], metadata_recv[i+1]*typesize);
            offset += metadata_recv[i+1]*typesize;
            pos_status[send_index] = 1;
            sendcounts[send_index] = metadata_recv[i+1];
        }
        double replace_end = MPI_Wtime();
        running_a2a_total_replace_time[task_id][loop_counter] += (replace_end - replace_start);

    }
    free(temp_send_buffer);
    free(temp_recv_buffer);
    double exchange_end = MPI_Wtime();
    running_a2a_exchange_time[task_id][loop_counter] = (exchange_end - exchange_start);

    double filter_start = MPI_Wtime();
    for (int i = 0; i < nprocs; i++)
    {
        if (rank == i)
            memcpy(&recvbuf[rdispls[i]*typesize], &sendbuf[sdispls[i]*typesize], recvcounts[i]*typesize);
        else
            memcpy(&recvbuf[rdispls[i]*typesize], &extra_buffer[i*max_send_count*typesize], recvcounts[i]*typesize);
    }
    free(extra_buffer);
    double filter_end = MPI_Wtime();
    running_a2a_filter_time[task_id][loop_counter] = filter_end - filter_start;


#if 0
    double max_u_time = 0;
    double total_u_time = u_end - u_start;
    MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, comm);
    if (total_u_time == max_u_time)
    {
        std::cout << "[SLOAV] [" << nprocs << " " << range << " " << max_send_count << "] " << total_u_time << " " << find_count_time << " " << create_rindex_time << " " << exchange_time << " ["
                  << total_find_blocks_time << " " << total_pre_time << " " << total_send_meda_time << " " << total_comm_time << " " << total_replace_time << "] " << filter_time << std::endl;
    }
#endif
}


static void padded_bruck_non_uniform_benchmark(char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm)
{
    double u_start = MPI_Wtime();

    int rank, nprocs;
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &nprocs);

    int typesize;
    MPI_Type_size(sendtype, &typesize);

    // 1. Find max send count
    double find_count_start = MPI_Wtime();
    int local_max_count = 0;
    for (int i = 0; i < nprocs; i++)
    {
        if (sendcounts[i] > local_max_count)
            local_max_count = sendcounts[i];
    }
    int max_send_count = 0;
    MPI_Allreduce(&local_max_count, &max_send_count, 1, MPI_INT, MPI_MAX, comm);
    double find_count_end = MPI_Wtime();
    double find_count_time = find_count_end - find_count_start;

    // 2. local rotation
    double rotation_start = MPI_Wtime();
    char* temp_send_buffer = (char*)malloc(max_send_count*nprocs*typesize);
    memset(temp_send_buffer, 0, max_send_count*nprocs*typesize);
    int offset = 0;
    for (int i = 0; i < nprocs; i++)
    {
        int index = (i - rank + nprocs) % nprocs;
        memcpy(&temp_send_buffer[index*max_send_count*typesize], &sendbuf[offset], sendcounts[i]*typesize);
        offset += sendcounts[i]*typesize;
    }
    double rotation_end = MPI_Wtime();
    double rotation_time = rotation_end - rotation_start;

    // 3. exchange data with log(P) steps
    double exchange_start =  MPI_Wtime();
    u64 unit_size = max_send_count * typesize;
    double total_find_blocks_time = 0, total_copy_time = 0, total_comm_time = 0, total_replace_time = 0;
    char* temp_buffer = (char*)malloc(max_send_count*typesize*((nprocs+1)/2));
    char* temp_recv_buffer = (char*)malloc(max_send_count*typesize*((nprocs+1)/2));
    for (int k = 1; k < nprocs; k <<= 1)
    {
        // 1) find which data blocks to send
        double find_blocks_start = MPI_Wtime();
        int send_indexes[(nprocs+1)/2];
        int sendb_num = 0;
        for (int i = k; i < nprocs; i++)
        {
            if (i & k)
                send_indexes[sendb_num++] = i;
        }
        double find_blocks_end = MPI_Wtime();
        total_find_blocks_time += (find_blocks_end - find_blocks_start);

        // 2) copy blocks which need to be sent at this step
        double copy_start = MPI_Wtime();
        for (int i = 0; i < sendb_num; i++)
        {
            u64 offset = send_indexes[i] * unit_size;
            memcpy(temp_buffer+(i*unit_size), temp_send_buffer+offset, unit_size);
        }
        double copy_end = MPI_Wtime();
        total_copy_time += (copy_end - copy_start);

        // 3) send and receive
        double comm_start = MPI_Wtime();
        int recv_proc = (rank - k + nprocs) % nprocs; // receive data from rank - 2^step process
        int send_proc = (rank + k) % nprocs; // send data from rank + 2^k process
        u64 comm_size = sendb_num * unit_size;
        MPI_Sendrecv(temp_buffer, comm_size, MPI_CHAR, send_proc, 0, temp_recv_buffer, comm_size, MPI_CHAR, recv_proc, 0, comm, MPI_STATUS_IGNORE);
        double comm_end = MPI_Wtime();
        total_comm_time += (comm_end - comm_start);

        // 4) replace with received data
        double replace_start = MPI_Wtime();
        for (int i = 0; i < sendb_num; i++)
        {
            u64 offset = send_indexes[i] * unit_size;
            memcpy(temp_send_buffer+offset, temp_recv_buffer+(i*unit_size), unit_size);
        }
        double replace_end = MPI_Wtime();
        total_replace_time += (replace_end - replace_start);
    }
    free(temp_buffer);
    free(temp_recv_buffer);
    double exchange_end = MPI_Wtime();
    double exchange_time = exchange_end - exchange_start;

    // 4. second rotation
    double revs_rotation_start = MPI_Wtime();
    offset = 0;
    for (int i = 0; i < nprocs; i++)
    {
        int index = (rank - i + nprocs) % nprocs;
        memcpy(&recvbuf[rdispls[index]*typesize], &temp_send_buffer[i*unit_size], recvcounts[index]*typesize);
    }
    free(temp_send_buffer);
    double revs_rotation_end = MPI_Wtime();
    double revs_rotation_time = revs_rotation_end - revs_rotation_start;

    /*
    double u_end = MPI_Wtime();
    double max_u_time = 0;
    double total_u_time = u_end - u_start;
    MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, comm);
    if (total_u_time == max_u_time)
    {
         std::cout << "[PaddedBruck] [" << " " << nprocs << " " << max_send_count << "] " << total_u_time << " " << find_count_time << " " << rotation_time << " "
                 << exchange_time << " [" << total_find_blocks_time << " " << total_copy_time << " " << total_comm_time << " " << total_replace_time << "] "<< revs_rotation_time << std::endl;
    }
    */
}
#endif
