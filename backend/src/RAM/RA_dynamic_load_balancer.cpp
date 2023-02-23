#include "../parallel_RA_inc.h"
#include <iostream>


void RAM::load_balance()
{
    for (u32 i=0; i < ram_relation_count; i++)
    {
        relation* current_relation = ram_relations[i];
        if (current_relation->load_balance_merge_full_and_delta(refinement_factor) == false)
            current_relation->load_balance_split_full_and_delta(refinement_factor);

        u64 max_full_element_count = 0, min_full_element_count = 0, sum_full_element_count = 0, full_element_count = 0, max_delta_element_count = 0, min_delta_element_count = 0, sum_delta_element_count = 0, delta_element_count = 0;

        full_element_count = current_relation->get_full_element_count();
        delta_element_count = current_relation->get_delta_element_count();

        MPI_Allreduce(&full_element_count, &max_full_element_count, 1, MPI_UNSIGNED_LONG_LONG, MPI_MAX, mcomm.get_local_comm());
        MPI_Allreduce(&full_element_count, &min_full_element_count, 1, MPI_UNSIGNED_LONG_LONG, MPI_MIN, mcomm.get_local_comm());
        MPI_Allreduce(&full_element_count, &sum_full_element_count, 1, MPI_UNSIGNED_LONG_LONG, MPI_SUM, mcomm.get_local_comm());

        MPI_Allreduce(&delta_element_count, &max_delta_element_count, 1, MPI_UNSIGNED_LONG_LONG, MPI_MAX, mcomm.get_local_comm());
        MPI_Allreduce(&delta_element_count, &min_delta_element_count, 1, MPI_UNSIGNED_LONG_LONG, MPI_MIN, mcomm.get_local_comm());
        MPI_Allreduce(&delta_element_count, &sum_delta_element_count, 1, MPI_UNSIGNED_LONG_LONG, MPI_SUM, mcomm.get_local_comm());

        if (mcomm.get_rank() == 0)
        {
            if (min_full_element_count != 0 && min_delta_element_count != 0)
                std::cout << "[LOAD BALANCING] Full (" << max_full_element_count << ", " << min_full_element_count << ", " << sum_full_element_count/mcomm.get_nprocs() << ") "
                          << (float) max_full_element_count/min_full_element_count << " "
                          << "Delta (" << max_delta_element_count << ", " << min_delta_element_count << ", " << sum_delta_element_count/mcomm.get_nprocs() << ") "
                          << (float) max_delta_element_count/min_delta_element_count << " "
                          << std::endl;
            else if (min_full_element_count != 0 && min_delta_element_count == 0)
                std::cout << "[LOAD BALANCING] Full (" << max_full_element_count << ", " << min_full_element_count << ", " << sum_full_element_count/mcomm.get_nprocs() << ") "
                          << (float) max_full_element_count/min_full_element_count << " "
                          << "Delta (" << max_delta_element_count << ", " << min_delta_element_count << ", " << sum_delta_element_count/mcomm.get_nprocs() << ") "
                          << std::endl;
            else
                std::cout << "[LOAD BALANCING] Full (" << max_full_element_count << ", " << min_full_element_count << ", " << sum_full_element_count/mcomm.get_nprocs() << ") "
                          << "Delta (" << max_delta_element_count << ", " << min_delta_element_count << ", " << sum_delta_element_count/mcomm.get_nprocs() << ") "
                          << std::endl;
        }
    }
}