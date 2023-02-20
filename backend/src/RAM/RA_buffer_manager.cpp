#include "../parallel_RA_inc.h"
#include <iostream>

u32 RAM::allocate_local_join_buffers()
{
    u32 allocated_memory_size = 0;

    // u64 **cumulative_all_to_allv_buffer_cmp;                      /// result of all to all comm
    // int** cumulative_all_to_allv_recv_process_size_array_cmp;

    compute_buffer.ra_count = RA_list.size();
    compute_buffer.nprocs = get_bucket_count();
    compute_buffer.local_compute_output_size_total = 0;

    compute_buffer.local_compute_output_size_rel = new int[compute_buffer.ra_count];
    memset(compute_buffer.local_compute_output_size_rel, 0, compute_buffer.ra_count * sizeof(int));

    compute_buffer.width = new int[compute_buffer.ra_count];

    allocated_memory_size = allocated_memory_size + compute_buffer.ra_count * sizeof(int);

    compute_buffer.local_compute_output = new vector_buffer*[compute_buffer.ra_count];
    compute_buffer.local_compute_output_size = new int*[compute_buffer.ra_count];
    compute_buffer.local_compute_output_size_flat = new int[compute_buffer.ra_count * compute_buffer.nprocs]{0};
    compute_buffer.local_compute_output_count_flat = new int[compute_buffer.ra_count * compute_buffer.nprocs]{0};

    allocated_memory_size = allocated_memory_size + compute_buffer.ra_count * sizeof(vector_buffer*);
    allocated_memory_size = allocated_memory_size + compute_buffer.ra_count * sizeof(int*);
    allocated_memory_size = allocated_memory_size + compute_buffer.ra_count * compute_buffer.nprocs * sizeof(int);

    compute_buffer.cumulative_tuple_process_map = new int[compute_buffer.nprocs];
    memset(compute_buffer.cumulative_tuple_process_map, 0, compute_buffer.nprocs * sizeof(int));
    allocated_memory_size = allocated_memory_size + compute_buffer.nprocs * sizeof(int);

    for (int i = 0; i < compute_buffer.ra_count; i++)
    {
        compute_buffer.local_compute_output[i] = new vector_buffer[compute_buffer.nprocs];
        allocated_memory_size = allocated_memory_size + compute_buffer.nprocs * sizeof(vector_buffer);

        for (int j = 0; j < compute_buffer.nprocs; j++)
            compute_buffer.local_compute_output[i][j].vector_buffer_create_empty();

        compute_buffer.local_compute_output_size[i] = new int[compute_buffer.nprocs];
        memset(compute_buffer.local_compute_output_size[i], 0, compute_buffer.nprocs * sizeof(int));
        allocated_memory_size = allocated_memory_size + compute_buffer.nprocs * sizeof(int);
    }

    return allocated_memory_size;
}

void RAM::free_local_join_buffers()
{
    for (int i = 0; i < compute_buffer.ra_count; i++)
    {
        delete[] compute_buffer.local_compute_output[i];
        delete[] compute_buffer.local_compute_output_size[i];
    }
    delete[] compute_buffer.local_compute_output_size_rel;
    delete[] compute_buffer.local_compute_output_size_flat;
    delete[] compute_buffer.local_compute_output_count_flat;

    delete[] compute_buffer.width;
    delete[] compute_buffer.local_compute_output;
    delete[] compute_buffer.local_compute_output_size;
    delete[] compute_buffer.cumulative_tuple_process_map;
}