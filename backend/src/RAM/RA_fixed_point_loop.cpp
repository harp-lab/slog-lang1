#include "../parallel_RA_inc.h"
#include <iostream>

void RAM::check_for_fixed_point(std::vector<u32>& history)
{
    int local_delta_sum = 0, local_full_sum = 0, global_delta_sum = 0, global_full_sum = 0;
    for (u32 i=0; i < ram_relation_count; i++)
    {
        local_delta_sum = local_delta_sum + ram_relations[i]->get_delta_element_count();
        local_full_sum = local_full_sum + ram_relations[i]->get_full_element_count();
    }
    MPI_Allreduce(&local_delta_sum, &global_delta_sum, 1, MPI_INT, MPI_SUM, mcomm.get_local_comm());
    MPI_Allreduce(&local_full_sum, &global_full_sum, 1, MPI_INT, MPI_SUM, mcomm.get_local_comm());

    history.push_back(global_delta_sum);
    history.push_back(global_full_sum);
}



void RAM::fixed_point_loop(std::string name, int batch_size, std::vector<u32>& history, std::map<u64, u64>& intern_map)
{
    int inner_loop = 0;
    u32 RA_count = RA_list.size();

    int *offset = new int[RA_count];
    for (u32 i =0; i < RA_count; i++)
        offset[i] = 0;

    while (batch_size != 0)
    {

        intra_bucket_comm_execute();

        bool local_join_status = false;
        while (local_join_status == false)
        {
            allocate_local_join_buffers();

            local_join_status = local_compute(offset);

            comm_compaction_all_to_all(compute_buffer, &cumulative_all_to_allv_recv_process_count_array, &cumulative_all_to_allv_buffer, mcomm.get_local_comm());

            free_local_join_buffers();

            local_insert_in_newt_comm_compaction(intern_map);

            inner_loop++;
        }
        local_insert_in_full();

        batch_size--;
        loop_count_tracker++;

        if (iteration_count == 1)
            break;
    }

    delete[] offset;


    check_for_fixed_point(history);

    if (logging == true)
        print_all_relation();
}