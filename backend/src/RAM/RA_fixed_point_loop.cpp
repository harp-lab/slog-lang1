#include "../parallel_RA_inc.h"
#include <iostream>
#include <vector>

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

    double intra_time = 0;
    double local_join_time = 0;
    double comm_time = 0;
    double newt_insert_time = 0;
    double full_insert_time = 0;
    std::vector<double> intra_detail(4);

    while (batch_size != 0)
    {

        auto before_intra_time = MPI_Wtime();
        intra_bucket_comm_execute(intra_detail);
        auto after_intra_time = MPI_Wtime();
        intra_time += after_intra_time - before_intra_time;

        bool local_join_status = false;
        
        while (local_join_status == false)
        {
            allocate_local_join_buffers();

            auto before_local_join = MPI_Wtime();
            local_join_status = local_compute(offset);
            auto after_local_join = MPI_Wtime();
            local_join_time += after_local_join - before_local_join;

            auto before_comm_join = MPI_Wtime();
            comm_compaction_all_to_all(compute_buffer, &cumulative_all_to_allv_recv_process_count_array, &cumulative_all_to_allv_buffer, mcomm.get_local_comm());
            auto after_comm_join = MPI_Wtime();
            comm_time += after_comm_join - before_comm_join;

            free_local_join_buffers();

            auto before_newt_join = MPI_Wtime();
            local_insert_in_newt_comm_compaction(intern_map);
            auto after_newt_join = MPI_Wtime();
            newt_insert_time += after_newt_join - before_newt_join;

            inner_loop++;
        }

        auto before_full_join = MPI_Wtime();
        local_insert_in_full();
        auto after_full_join = MPI_Wtime();
        full_insert_time += after_full_join - before_full_join;

        batch_size--;
        loop_count_tracker++;

        if (iteration_count == 1)
            break;
    }
    if (mcomm.get_rank() == 0) {
        std::cout << " >> Join: " << local_join_time << " , Comm: " << comm_time << " , Newt: " << newt_insert_time
                  << " , Full: " << full_insert_time << " , Intra: " << intra_time
                  <<" (" << intra_detail[0] << "," << intra_detail[1] << "," << intra_detail[2] << ")";
    }
    delete[] offset;


    check_for_fixed_point(history);

    if (logging == true)
        print_all_relation();
}