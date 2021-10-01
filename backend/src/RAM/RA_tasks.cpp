/*
 * scc (tasks)
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#include "../parallel_RA_inc.h"

RAM::~RAM()
{
    loop_count_tracker = 0;
    for (std::vector<parallel_RA*>::iterator it = RA_list.begin() ; it != RA_list.end(); ++it)
        delete (*it);
}

/// Example: RAM* scc13237 = new RAM(true, 1);
/// true: run this scc till fixed point is reached (false: run this scc for only one iteration, for copy and acopy rules)
/// 1: id of scc (used for internal debugging)
RAM::RAM (bool ic, int r_id)
{
    ram_relation_count = 0;
    loop_count_tracker = 0;
    if (ic==false)
        iteration_count=1;
    ram_id = r_id;
    RA_list = {};
}


/// Example
/// rel_edge_2_2: relation that is being added to the SCC
/// false: keep the delta and full the way they are (true: move whatever is in full to delta, once before the start of the fixed point loop)
void RAM::add_relation(relation*& G, bool i_status)
{
    ram_relations[ram_relation_count] = G;
    ram_relation_status[ram_relation_count] = i_status;
    ram_relation_count++;
}



void RAM::set_comm(mpi_comm& mcomm)
{
    this->mcomm = mcomm;

    for (parallel_RA* ra : RA_list)
        ra->set_comm(mcomm);

    for (u32 i=0; i < ram_relation_count; i++)
        ram_relations[i]->set_mcomm(mcomm);
}



void RAM::print_all_relation()
{
    for (u32 i=0; i < ram_relation_count; i++)
        ram_relations[i]->print();
}




void RAM::load_balance()
{
    for (u32 i=0; i < ram_relation_count; i++)
    {
        relation* current_relation = ram_relations[i];
        if (current_relation->load_balance_merge_full_and_delta(refinement_factor) == false)
            current_relation->load_balance_split_full_and_delta(refinement_factor);

        u64 max_full_element_count = 0, min_full_element_count = 0, sum_full_element_count = 0, full_element_count = 0, max_delta_element_count = 0, min_delta_element_count = 0, sum_delta_element_count = 0, delta_element_count = 0;

        u32* bucket_map = current_relation->get_bucket_map();
        u32* sub_bucket_count = current_relation->get_sub_bucket_per_bucket_count();

        u32** full_sub_bucket_size = current_relation->get_full_sub_bucket_element_count();
        u32** delta_sub_bucket_size = current_relation->get_delta_sub_bucket_element_count();

        for (u32 i = 0; i < get_bucket_count(); i++)
        {
            if (bucket_map[i] == 1)
            {
                for (u32 j = 0; j < sub_bucket_count[i]; j++)
                {
                    full_element_count = full_element_count + full_sub_bucket_size[i][j];
                    delta_element_count = delta_element_count + delta_sub_bucket_size[i][j];
                }
            }
        }

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



u64 RAM::intra_bucket_comm_execute()
{
    u64 total_data_moved = 0;
    u32 counter = 0;
    u32 RA_count = RA_list.size();

    intra_bucket_buf_output_size = new u64[RA_count];
    memset(intra_bucket_buf_output_size, 0, RA_count * sizeof(u64));

    intra_bucket_buf_output = new u64*[RA_count];

    for (std::vector<parallel_RA*>::iterator it = RA_list.begin() ; it != RA_list.end(); ++it)
    {
        /// No intra-bucket comm required for copy
        if ((*it)->get_RA_type() == COPY)
        {
            counter++;
            continue;
        }

        /// No intra-bucket comm required for copy
        else if ((*it)->get_RA_type() == COPY_FILTER)
        {
            counter++;
            continue;
        }

        /// No intra-bucket comm required for copy
        else if ((*it)->get_RA_type() == COPY_GENERATE)
        {
            counter++;
            continue;
        }

        /// No intra-bucket comm required for acopy
        else if ((*it)->get_RA_type() == ACOPY)
        {
            counter++;
            continue;
        }

        /// No intra-bucket comm required for fact
        else if ((*it)->get_RA_type() == FACT)
        {
            counter++;
            continue;
        }

        /// Intra-bucket comm for joins
        else if ((*it)->get_RA_type() == JOIN)
        {
            parallel_join* current_ra = (parallel_join*) *it;
            relation* input0 = current_ra->get_join_input0();
            relation* input1 = current_ra->get_join_input1();

            /// Join between delta and delta
            if (current_ra->get_join_input0_graph_type() == DELTA && current_ra->get_join_input1_graph_type() == DELTA)
            {

                intra_bucket_comm(get_bucket_count(),
                                  input0->get_delta(),
                                  input0->get_distinct_sub_bucket_rank_count(), input0->get_distinct_sub_bucket_rank(), input0->get_bucket_map(),
                                  input1->get_distinct_sub_bucket_rank_count(), input1->get_distinct_sub_bucket_rank(), input1->get_bucket_map(),
                                  &intra_bucket_buf_output_size[counter], &intra_bucket_buf_output[counter],
                                  mcomm.get_local_comm());

                total_data_moved = total_data_moved + intra_bucket_buf_output_size[counter];
            }

            /// Join between delta and full
            else if (current_ra->get_join_input0_graph_type() == DELTA && current_ra->get_join_input1_graph_type() == FULL)
            {

                intra_bucket_comm(get_bucket_count(),
                                  input0->get_delta(),
                                  input0->get_distinct_sub_bucket_rank_count(), input0->get_distinct_sub_bucket_rank(), input0->get_bucket_map(),
                                  input1->get_distinct_sub_bucket_rank_count(), input1->get_distinct_sub_bucket_rank(), input1->get_bucket_map(),
                                  &intra_bucket_buf_output_size[counter], &intra_bucket_buf_output[counter],
                                  mcomm.get_local_comm());
                total_data_moved = total_data_moved + intra_bucket_buf_output_size[counter];
            }

            /// Join between full and delta
            else if (current_ra->get_join_input0_graph_type() == FULL && current_ra->get_join_input1_graph_type() == DELTA)
            {
                intra_bucket_comm(get_bucket_count(),
                                  input1->get_delta(),
                                  input1->get_distinct_sub_bucket_rank_count(), input1->get_distinct_sub_bucket_rank(), input1->get_bucket_map(),
                                  input0->get_distinct_sub_bucket_rank_count(), input0->get_distinct_sub_bucket_rank(), input0->get_bucket_map(),
                                  &intra_bucket_buf_output_size[counter], &intra_bucket_buf_output[counter],
                                  mcomm.get_local_comm());
                total_data_moved = total_data_moved + intra_bucket_buf_output_size[counter];
            }

            /// Join between full and full
            else if (current_ra->get_join_input0_graph_type() == FULL && current_ra->get_join_input1_graph_type() == FULL)
            {

                intra_bucket_comm(get_bucket_count(),
                                  input1->get_full(),
                                  input1->get_distinct_sub_bucket_rank_count(), input1->get_distinct_sub_bucket_rank(), input1->get_bucket_map(),
                                  input0->get_distinct_sub_bucket_rank_count(), input0->get_distinct_sub_bucket_rank(), input0->get_bucket_map(),
                                  &intra_bucket_buf_output_size[counter], &intra_bucket_buf_output[counter],
                                  mcomm.get_local_comm());
                total_data_moved = total_data_moved + intra_bucket_buf_output_size[counter];
            }
        }
        counter++;
    }

    return total_data_moved;
}


void RAM::free_all_to_all_compute_buffers()
{
    delete[] all_to_all_compute_buffer.width;
    delete[] all_to_all_compute_buffer.local_compute_output;
    for (int i = 0; i < all_to_all_compute_buffer.ra_count; i++)
        delete[] all_to_all_compute_buffer.local_compute_output_size[i];
    delete[] all_to_all_compute_buffer.local_compute_output_size;
}

void RAM::allocate_all_to_all_compute_buffers()
{
    all_to_all_compute_buffer.ra_count = RA_list.size();
    all_to_all_compute_buffer.nprocs = get_bucket_count();
    all_to_all_compute_buffer.threshold = 2000;

    all_to_all_compute_buffer.width = new int[all_to_all_compute_buffer.ra_count];

    all_to_all_compute_buffer.local_compute_output = new u64[all_to_all_compute_buffer.nprocs * all_to_all_compute_buffer.ra_count * all_to_all_compute_buffer.threshold + all_to_all_compute_buffer.nprocs * all_to_all_compute_buffer.ra_count];
    memset(all_to_all_compute_buffer.local_compute_output, 0, (all_to_all_compute_buffer.nprocs * all_to_all_compute_buffer.ra_count * all_to_all_compute_buffer.threshold + all_to_all_compute_buffer.nprocs * all_to_all_compute_buffer.ra_count) * sizeof(u64));

    all_to_all_compute_buffer.local_compute_output_size = new int*[all_to_all_compute_buffer.ra_count];
    memset(all_to_all_compute_buffer.local_compute_output_size, 0, all_to_all_compute_buffer.ra_count * sizeof(int*));
    for (int i = 0; i < all_to_all_compute_buffer.ra_count; i++)
    {
        all_to_all_compute_buffer.local_compute_output_size[i] = new int[all_to_all_compute_buffer.nprocs];
        memset(all_to_all_compute_buffer.local_compute_output_size[i], 0, all_to_all_compute_buffer.nprocs * sizeof(int));
    }

}


u32 RAM::allocate_compute_buffers()
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
    compute_buffer.local_compute_output_size_flat = new int[compute_buffer.ra_count * compute_buffer.nprocs];
    memset(compute_buffer.local_compute_output_size_flat, 0, compute_buffer.ra_count * compute_buffer.nprocs * sizeof(int));

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


u32 RAM::local_compute(int* offset, int loop_counter, int task_id, int** compute_size1, int** compute_size2)
{
    bool join_completed = true;
    u32 join_tuples = 0;
    u32 join_tuples_duplicates = 0;
    u32 total_join_tuples = 0;
    u32 counter = 0;
    int threshold = 20000000;

    for (std::vector<parallel_RA*>::iterator it = RA_list.begin() ; it != RA_list.end(); ++it)
    {
        if ((*it)->get_RA_type() == COPY)
        {
            parallel_copy* current_ra = (parallel_copy*) *it;

            std::vector<int> reorder_map_array;
            current_ra->get_copy_rename_index(&reorder_map_array);
            relation* output_relation = current_ra->get_copy_output();
            relation* input_relation = current_ra->get_copy_input();

            if (current_ra->get_copy_input0_graph_type() == DELTA)
            {
                current_ra->local_copy(get_bucket_count(),
                                       input_relation->get_delta(), input_relation->get_bucket_map(),
                                       output_relation,
                                       reorder_map_array,
                                       input_relation->get_arity(),
                                       input_relation->get_join_column_count(),
                                       compute_buffer, counter);
            }
            if (current_ra->get_copy_input0_graph_type() == FULL)
            {
                current_ra->local_copy(get_bucket_count(),
                                       input_relation->get_full(), input_relation->get_bucket_map(),
                                       output_relation,
                                       reorder_map_array,
                                       input_relation->get_arity(),
                                       input_relation->get_join_column_count(),
                                       compute_buffer, counter);
            }
        }

        else if ((*it)->get_RA_type() == COPY_FILTER)
        {
            parallel_copy_filter* current_ra = (parallel_copy_filter*) *it;

            std::vector<int> reorder_map_array;
            current_ra->get_copy_filter_rename_index(&reorder_map_array);
            relation* output_relation = current_ra->get_copy_filter_output();
            relation* input_relation = current_ra->get_copy_filter_input();

            if (current_ra->get_copy_filter_input0_graph_type() == DELTA)
            {
                current_ra->local_copy_filter(get_bucket_count(),
                                              input_relation->get_delta(), input_relation->get_bucket_map(),
                                              output_relation,
                                              reorder_map_array,
                                              input_relation->get_arity(),
                                              input_relation->get_join_column_count(),
                                              compute_buffer, counter);
            }
            if (current_ra->get_copy_filter_input0_graph_type() == FULL)
            {
                current_ra->local_copy_filter(get_bucket_count(),
                                              input_relation->get_full(), input_relation->get_bucket_map(),
                                              output_relation,
                                              reorder_map_array,
                                              input_relation->get_arity(),
                                              input_relation->get_join_column_count(),
                                              compute_buffer, counter);
            }
        }

        else if ((*it)->get_RA_type() == COPY_GENERATE)
        {
            parallel_copy_generate* current_ra = (parallel_copy_generate*) *it;

            //std::vector<int> reorder_map_array;
            //current_ra->get_copy_filter_rename_index(&reorder_map_array);
            relation* output_relation = current_ra->get_copy_generate_output();
            relation* input_relation = current_ra->get_copy_generate_input();

            if (current_ra->get_copy_generate_input0_graph_type() == DELTA)
            {
                current_ra->local_copy_generate(get_bucket_count(),
                                                input_relation->get_delta(), input_relation->get_bucket_map(),
                                                output_relation,
                                                input_relation->get_arity(),
                                                input_relation->get_join_column_count(),
                                                compute_buffer, counter);
            }
            if (current_ra->get_copy_generate_input0_graph_type() == FULL)
            {
                current_ra->local_copy_generate(get_bucket_count(),
                                                input_relation->get_full(), input_relation->get_bucket_map(),
                                                output_relation,
                                                input_relation->get_arity(),
                                                input_relation->get_join_column_count(),
                                                compute_buffer, counter);
            }
        }

        else if ((*it)->get_RA_type() == FACT)
        {
            fact* current_ra = (fact*) *it;
            relation* fact_relation = current_ra->get_relation();
            std::vector<u64> data = current_ra->get_init_data();
            current_ra->init_with_fact(get_bucket_count(),
                                       fact_relation->get_sub_bucket_per_bucket_count(),
                                       fact_relation->get_sub_bucket_rank(),
                                       fact_relation->get_arity(),
                                       fact_relation->get_join_column_count(),
                                       fact_relation->get_is_canonical(),
                                       data,
                                       compute_buffer,
                                       counter);


        }


        else if ((*it)->get_RA_type() == ACOPY)
        {
            parallel_acopy* current_ra = (parallel_acopy*) *it;

            std::vector<int> reorder_map_array;
            current_ra->get_acopy_rename_index(&reorder_map_array);
            relation* output_relation = current_ra->get_acopy_output();
            relation* input_relation = current_ra->get_acopy_input();

            if (current_ra->get_acopy_input0_graph_type() == DELTA)
            {
                current_ra->local_acopy(get_bucket_count(),
                                        input_relation->get_delta(), input_relation->get_bucket_map(),
                                        output_relation,
                                        reorder_map_array,
                                        input_relation->get_arity(),
                                        input_relation->get_join_column_count(),
                                        compute_buffer, counter);
            }
            else if (current_ra->get_acopy_input0_graph_type() == FULL)
            {
                current_ra->local_acopy(get_bucket_count(),
                                        input_relation->get_full(), input_relation->get_bucket_map(),
                                        output_relation,
                                        reorder_map_array,
                                        input_relation->get_arity(),
                                        input_relation->get_join_column_count(),
                                        compute_buffer, counter);
            }
        }

        else if ((*it)->get_RA_type() == JOIN)
        {
            parallel_join* current_ra = (parallel_join*) *it;
            relation* output_relation = current_ra->get_join_output();

            std::vector<int> reorder_map_array;
            current_ra->get_join_projection_index(&reorder_map_array);
            relation* input0 = current_ra->get_join_input0();
            relation* input1 = current_ra->get_join_input1();
            relation* output = current_ra->get_join_output();
            assert(output->get_arity() == reorder_map_array.size());
            int join_column_count = input0->get_join_column_count();

            if (current_ra->get_join_input0_graph_type() == DELTA && current_ra->get_join_input1_graph_type() == DELTA)
            {
                join_completed = join_completed & current_ra->local_join(threshold, &(offset[counter]),
                                                                         LEFT,
                                                                         get_bucket_count(),
                                                                         intra_bucket_buf_output_size[counter], input0->get_arity()+1, intra_bucket_buf_output[counter],
                                                                         input1->get_delta(), input1->get_delta_element_count(), input1->get_arity()+1,
                                                                         reorder_map_array,
                                                                         output_relation,
                                                                         compute_buffer,
                                                                         counter,
                                                                         join_column_count,
                                                                         &join_tuples_duplicates,
                                                                         &join_tuples);
                total_join_tuples = total_join_tuples + join_tuples;
            }
            else if (current_ra->get_join_input0_graph_type() == DELTA && current_ra->get_join_input1_graph_type() == FULL)
            {

                join_completed = join_completed & current_ra->local_join(threshold, &(offset[counter]),
                                                                         LEFT,
                                                                         get_bucket_count(),
                                                                         intra_bucket_buf_output_size[counter], input0->get_arity()+1, intra_bucket_buf_output[counter],
                                                                         input1->get_full(), input1->get_full_element_count(), input1->get_arity()+1,
                                                                         reorder_map_array,
                                                                         output_relation,
                                                                         compute_buffer,
                                                                         counter,
                                                                         join_column_count,
                                                                         &join_tuples_duplicates,
                                                                         &join_tuples);
                total_join_tuples = total_join_tuples + join_tuples;
            }
            else if (current_ra->get_join_input0_graph_type() == FULL && current_ra->get_join_input1_graph_type() == DELTA)
            {

                join_completed = join_completed & current_ra->local_join(threshold, &(offset[counter]),
                                                                         RIGHT,
                                                                         get_bucket_count(),
                                                                         intra_bucket_buf_output_size[counter], input1->get_arity()+1, intra_bucket_buf_output[counter],
                                                                         input0->get_full(), input0->get_full_element_count(), input0->get_arity()+1,
                                                                         reorder_map_array,
                                                                         output_relation,
                                                                         compute_buffer,
                                                                         counter,
                                                                         join_column_count,
                                                                         &join_tuples_duplicates,
                                                                         &join_tuples);
                total_join_tuples = total_join_tuples + join_tuples;
            }
            else if (current_ra->get_join_input0_graph_type() == FULL && current_ra->get_join_input1_graph_type() == FULL)
            {
                join_completed = join_completed & current_ra->local_join(threshold, &(offset[counter]),
                                                                         RIGHT,
                                                                         get_bucket_count(),
                                                                         intra_bucket_buf_output_size[counter], input1->get_arity()+1, intra_bucket_buf_output[counter],
                                                                         input0->get_full(), input0->get_full_element_count(), input0->get_arity()+1,
                                                                         reorder_map_array,
                                                                         output_relation,
                                                                         compute_buffer,
                                                                         counter,
                                                                         join_column_count,
                                                                         &join_tuples_duplicates,
                                                                         &join_tuples);
                total_join_tuples = total_join_tuples + join_tuples;
            }

            compute_size1[task_id][loop_counter] = join_tuples_duplicates;
            compute_size2[task_id][loop_counter] = join_tuples;
        }
        counter++;
    }

#if 0
    int global_total_join_tuples = 0;
    int global_join_tuples_duplicates = 0;
    MPI_Allreduce(&total_join_tuples, &global_total_join_tuples, 1, MPI_INT, MPI_SUM, mcomm.get_local_comm());
    MPI_Allreduce(&join_tuples_duplicates, &global_join_tuples_duplicates, 1, MPI_INT, MPI_SUM, mcomm.get_local_comm());
    if (mcomm.get_rank() == 0)
        std::cout << "Joins: " << global_total_join_tuples << " Duplicates " << global_join_tuples_duplicates << " ";
#endif

    int global_synchronizer = 0;
    int synchronizer = 0;
    if (join_completed == true)
        synchronizer = 1;

    MPI_Allreduce(&synchronizer, &global_synchronizer, 1, MPI_INT, MPI_BAND, mcomm.get_comm());
    if (global_synchronizer == 1)
    {
        counter = 0;
        for (std::vector<parallel_RA*>::iterator it = RA_list.begin() ; it != RA_list.end(); ++it)
        {
            parallel_RA* current_ra = *it;
            if (current_ra->get_RA_type() == JOIN)
                delete[] intra_bucket_buf_output[counter];

            offset[counter] = 0;
            counter++;
        }

        delete[] intra_bucket_buf_output_size;
        delete[] intra_bucket_buf_output;
        return true;
    }
    else
        return false;
}


#if 1
void RAM::local_comm()
{
    int cnt=0;
    cumulative_all_to_allv_buffer_cmp = new u64*[RA_list.size()];
    cumulative_all_to_allv_recv_process_size_array_cmp = new int[RA_list.size()];

    for (std::vector<parallel_RA*>::iterator it = RA_list.begin() ; it != RA_list.end(); ++it)
    {
        all_to_all_comm(compute_buffer.local_compute_output[cnt], compute_buffer.local_compute_output_size_rel[cnt], compute_buffer.local_compute_output_size[cnt], &cumulative_all_to_allv_recv_process_size_array_cmp[cnt], &cumulative_all_to_allv_buffer_cmp[cnt], mcomm.get_local_comm());
        cnt++;
    }
}
#endif




void RAM::free_compute_buffers()
{
    for (int i = 0; i < compute_buffer.ra_count; i++)
    {
        delete[] compute_buffer.local_compute_output[i];
        delete[] compute_buffer.local_compute_output_size[i];
    }
    delete[] compute_buffer.local_compute_output_size_rel;
    delete[] compute_buffer.local_compute_output_size_flat;
    delete[] compute_buffer.width;
    delete[] compute_buffer.local_compute_output;
    delete[] compute_buffer.local_compute_output_size;
    delete[] compute_buffer.cumulative_tuple_process_map;
}


void RAM::local_insert_in_newt_comm_compaction(std::map<u64, u64>& intern_map)
{
    u32 successful_insert = 0, starting = 0;
    int nprocs = mcomm.get_local_nprocs();
    int RA_count = RA_list.size();
    u64 relation_id=0, bucket_id=0, intern_key=0, intern_value=0;

    for (int k = 0; k < RA_count * nprocs; k++)
    {
        successful_insert = 0;
        u32 ra_id = k % RA_count;
        u32 elements_to_read = cumulative_all_to_allv_recv_process_size_array[k];
        relation* output;

        if (RA_list[ra_id]->get_RA_type() == COPY)
            output = RA_list[ra_id]->get_copy_output();
        else if (RA_list[ra_id]->get_RA_type() == COPY_FILTER)
            output = RA_list[ra_id]->get_copy_filter_output();
        else if (RA_list[ra_id]->get_RA_type() == JOIN)
            output = RA_list[ra_id]->get_join_output();
        else if (RA_list[ra_id]->get_RA_type() == COPY_GENERATE)
            output = RA_list[ra_id]->get_copy_generate_output();
        else if (RA_list[ra_id]->get_RA_type() == FACT)
            output = RA_list[ra_id]->get_relation();
        else
            output = RA_list[ra_id]->get_acopy_output();

        if (RA_list[ra_id]->get_RA_type() == COPY || RA_list[ra_id]->get_RA_type() == JOIN || RA_list[ra_id]->get_RA_type() == COPY_FILTER || RA_list[ra_id]->get_RA_type() == COPY_GENERATE || RA_list[ra_id]->get_RA_type() == FACT)
        {
            u32 width = output->get_arity();
            u64 tuple[width + 1];

            if (output->get_arity() == 0)
            {
                tuple[width] = 0;
                assert(elements_to_read == 0);
                relation_id = output->get_intern_tag();
                relation_id = relation_id<<46;
                bucket_id = tuple_hash(tuple, output->get_join_column_count()) % get_bucket_count();

                if (bucket_id == mcomm.get_local_rank())
                {
                    bucket_id = bucket_id<<28;
                    intern_key = relation_id | bucket_id;

                    std::map<u64 ,u64>::const_iterator it = intern_map.find(intern_key);
                    if( it == intern_map.end() )
                        intern_value=0;
                    else
                        intern_value = it->second + 1;

                    intern_map[intern_key] = intern_value;
                    tuple[width] = intern_key | intern_value;    /// Intern here

                    if (output->insert_in_newt(tuple) == true)
                        successful_insert++;
                }

            }

            for (u32 x = starting; x < starting + elements_to_read; x = x + width)
            {
                if (output->find_in_full(cumulative_all_to_allv_buffer + x, width) == false &&
                        output->find_in_delta(cumulative_all_to_allv_buffer + x, width) == false &&
                        output->find_in_newt(cumulative_all_to_allv_buffer + x, width) == false)
                {
                    for (u32 i = 0; i < width; i++)
                        tuple[i] = cumulative_all_to_allv_buffer[x+i];

                    relation_id = output->get_intern_tag();
                    relation_id = relation_id<<46;
                    bucket_id = tuple_hash(tuple, output->get_join_column_count()) % get_bucket_count();
                    bucket_id = bucket_id<<28;

                    intern_key = relation_id | bucket_id;

                    std::map<u64 ,u64>::const_iterator it = intern_map.find(intern_key);
                    if( it == intern_map.end() )
                        intern_value=0;
                    else
                        intern_value = it->second + 1;

                    intern_map[intern_key] = intern_value;
                    tuple[width] = intern_key | intern_value;    /// Intern here


                    if (output->insert_in_newt(tuple) == true)
                        successful_insert++;

                    //if (RA_list[ra_id]->get_RA_type() == FACT)
                    //    std::cout << "FFFFFFFFFF "<< tuple[0] << " " << tuple[1] << " " << successful_insert << std::endl;
                }
            }
        }
        else if (RA_list[ra_id]->get_RA_type() == ACOPY)
        {
            //std::cout <<  "ACOPY finishing" << std::endl;
            u32 width = output->get_arity() + 1;
            u64 tuple[width];
            successful_insert = 0;
            for (u32 x = starting; x < starting + elements_to_read; x = x + width)
            {
                if (output->find_in_full(cumulative_all_to_allv_buffer + x, width) == false && output->find_in_delta(cumulative_all_to_allv_buffer + x, width) == false)
                {
                    for (u32 i = 0; i < width; i++)
                        tuple[i] = cumulative_all_to_allv_buffer[x+i];

                    if (output->insert_in_newt(tuple) == true)
                        successful_insert++;
                    //std::cout << "Inserting " << tuple[0] << " " << tuple[1] << std::endl;
                    //std::cout << "successful_insert " << successful_insert << std::endl;
                    //std::cout << "get_debug_id " << output->get_debug_id() << std::endl;
                }
            }
        }
        //else if (RA_list[ra_id]->get_RA_type() == FACT)
        //    continue;

        starting = starting + elements_to_read;
        //std::cout << output->get_debug_id() << " successful insert " << successful_insert << std::endl;
    }

    delete[] cumulative_all_to_allv_recv_process_size_array;
    delete[] cumulative_all_to_allv_buffer;
}



void RAM::local_insert_in_newt(std::map<u64, u64>& intern_map)
{
    u32 successful_insert = 0;
    //int nprocs = mcomm.get_local_nprocs();
    int RA_count = RA_list.size();
    u64 relation_id=0, bucket_id=0, intern_key=0, intern_value=0;

    for (int r = 0; r < RA_count; r++)
    {
        //for (int k = 0; k < nprocs; k++)
        //{
            successful_insert = 0;
            u32 elements_to_read = cumulative_all_to_allv_recv_process_size_array_cmp[r];
            relation* output;

            if (RA_list[r]->get_RA_type() == COPY)
                output = RA_list[r]->get_copy_output();
            else if (RA_list[r]->get_RA_type() == COPY_FILTER)
                output = RA_list[r]->get_copy_filter_output();
            else if (RA_list[r]->get_RA_type() == JOIN)
                output = RA_list[r]->get_join_output();
            else if (RA_list[r]->get_RA_type() == COPY_GENERATE)
                output = RA_list[r]->get_copy_generate_output();
            else
                output = RA_list[r]->get_acopy_output();

            if (RA_list[r]->get_RA_type() == COPY || RA_list[r]->get_RA_type() == JOIN || RA_list[r]->get_RA_type() == COPY_FILTER || RA_list[r]->get_RA_type() == COPY_GENERATE)
            {
                u32 width = output->get_arity();
                u64 tuple[width + 1];

                for (u32 x = 0; x < elements_to_read; x = x + width)
                {
                    if (output->find_in_full(cumulative_all_to_allv_buffer_cmp[r] + x, width) == false &&
                            output->find_in_delta(cumulative_all_to_allv_buffer_cmp[r] + x, width) == false &&
                            output->find_in_newt(cumulative_all_to_allv_buffer_cmp[r] + x, width) == false)
                    {
                        for (u32 i = 0; i < width; i++)
                            tuple[i] = cumulative_all_to_allv_buffer_cmp[r][x+i];

                        relation_id = output->get_intern_tag();
                        relation_id = relation_id<<46;
                        bucket_id = tuple_hash(tuple, output->get_join_column_count()) % get_bucket_count();
                        bucket_id = bucket_id<<28;

                        intern_key = relation_id | bucket_id;

                        std::map<u64 ,u64>::const_iterator it = intern_map.find(intern_key);
                        if( it == intern_map.end() )
                            intern_value=0;
                        else
                            intern_value = it->second + 1;

                        intern_map[intern_key] = intern_value;
                        tuple[width] = intern_key | intern_value;    /// Intern here

                        if (output->insert_in_newt(tuple) == true)
                            successful_insert++;
                    }
                }
            }
            else if (RA_list[r]->get_RA_type() == ACOPY)
            {
                u32 width = output->get_arity() + 1;
                u64 tuple[width];
                successful_insert = 0;
                for (u32 x = 0; x < elements_to_read; x = x + width)
                {
                    if (output->find_in_full(cumulative_all_to_allv_buffer_cmp[r] + x, width) == false && output->find_in_delta(cumulative_all_to_allv_buffer_cmp[r] + x, width) == false)
                    {
                        for (u32 i = 0; i < width; i++)
                            tuple[i] = cumulative_all_to_allv_buffer_cmp[r][x+i];

                        if (output->insert_in_newt(tuple) == true)
                            successful_insert++;
                    }
                }
            }
            //std::cout << output->get_debug_id() << " successful insert " << successful_insert << std::endl;
        //}
        delete[] cumulative_all_to_allv_buffer_cmp[r];
    }

    //delete[] cumulative_all_to_allv_recv_process_size_array;
    //delete[] cumulative_all_to_allv_buffer;

    delete[] cumulative_all_to_allv_recv_process_size_array_cmp;
    delete[] cumulative_all_to_allv_buffer_cmp;
}


void RAM::local_insert_in_full()
{
    for (u32 i=0; i < ram_relation_count; i++)
        //for (std::map<relation*, bool>::iterator it = ram_relations.begin() ; it != ram_relations.end(); ++it)
    {
        //relation* current_r = it->first;
        relation* current_r = ram_relations[i];
        current_r->insert_delta_in_full();
        current_r->local_insert_in_delta();

        //if (current_r->get_debug_id() == 11)
        //    current_r->print();
    }
    return;
}



void RAM::insert_delta_in_full()
{
    for (u32 i=0; i < ram_relation_count; i++)
        ram_relations[i]->insert_delta_in_full();

    return;
}



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



void RAM::io_all_relation(int status)
{

    char scc_name[1024];

    if (status == 1)
    {
        sprintf(scc_name, "output/scc-%d-iteration_%d", ram_id, loop_count_tracker);
        mkdir(scc_name, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);

        for (u32 i = 0 ; i < ram_relation_count; i++)
            ram_relations[i]->serial_IO(scc_name);
    }
    else if (status == 0)
    {
        sprintf(scc_name, "output/scc-%d-initial-facts", ram_id);
        mkdir(scc_name, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);

        for (u32 i = 0 ; i < ram_relation_count; i++)
            ram_relations[i]->serial_IO(scc_name);
    }
    else if (status == 2)
    {
        sprintf(scc_name, "output/scc-%d-output-facts", ram_id);
        mkdir(scc_name, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);

        for (u32 i = 0 ; i < ram_relation_count; i++)
            ram_relations[i]->serial_IO(scc_name);
    }
}


void RAM::execute_in_batches(std::string name, int batch_size, std::vector<u32>& history, std::map<u64, u64>& intern_map, double **running_time, double **running_intra_bucket_comm, double **running_buffer_allocate, double **running_local_compute, double **running_all_to_all, double **running_buffer_free, double **running_insert_newt, double **running_insert_in_full, double **running_fp, double **running_a2a_find_count_time, double **running_a2a_create_rindex_time, double **running_a2a_total_find_blocks_time, double **running_a2a_total_pre_time, double **running_a2a_total_send_meda_time, double **running_a2a_total_comm_time, double **running_a2a_total_replace_time, double **running_a2a_exchange_time, double **running_a2a_filter_time,  int* loop_counter, int task_id, std::string output_dir, bool all_to_all_record, int*** all_to_all_buffer_size, int** compute_size1, int** compute_size2, int sloav_mode, int* rotate_index_array, int** send_indexes, int *sendb_num)
{
    int inner_loop = 0;
    u32 RA_count = RA_list.size();

    int *offset = new int[RA_count];
    for (u32 i =0; i < RA_count; i++)
        offset[i] = 0;

    while (batch_size != 0)
    {
#if DEBUG_OUTPUT
        //if (mcomm.get_rank() == 0)
        //    std::cout << "--------------FIXED POINT ITERATION " << loop_count_tracker << "--------------" << std::endl;
#endif


        double intra_start = MPI_Wtime();
        intra_bucket_comm_execute();
        double intra_end = MPI_Wtime();
        running_intra_bucket_comm[task_id][*loop_counter] = (intra_end - intra_start);

        bool local_join_status = false;
        while (local_join_status == false)
        {
            double allocate_buffers_start = MPI_Wtime();
            allocate_compute_buffers();
            double allocate_buffers_end = MPI_Wtime();
            running_buffer_allocate[task_id][*loop_counter] = (allocate_buffers_end - allocate_buffers_start);


            double compute_start = MPI_Wtime();
            local_join_status = local_compute(offset, *loop_counter, task_id, compute_size1, compute_size2);
            double compute_end = MPI_Wtime();
            running_local_compute[task_id][*loop_counter] = (compute_end - compute_start);

            if (mcomm.get_rank() == 0)
            {
                std::cout << output_dir << "\t"
                          << mcomm.get_local_nprocs() << "\t";
            }

            double all_to_all_start = MPI_Wtime();
            local_comm();

            double all_to_all_end = MPI_Wtime();
            running_all_to_all[task_id][*loop_counter] = (all_to_all_end - all_to_all_start);


            double free_buffers_start = MPI_Wtime();
            free_compute_buffers();
            double free_buffers_end = MPI_Wtime();
            running_buffer_free[task_id][*loop_counter] = (free_buffers_end - free_buffers_start);


            double insert_in_newt_start = MPI_Wtime();
            local_insert_in_newt(intern_map);
            double insert_in_newt_end = MPI_Wtime();
            running_insert_newt[task_id][*loop_counter] = (insert_in_newt_end - insert_in_newt_start);


#if DEBUG_OUTPUT
            if (mcomm.get_rank() == 0)
            {
#if 0
                std::cout << name << " " << mcomm.get_local_nprocs() << " Current time INNER LOOP [" << loop_count_tracker << " " << inner_loop << "] "
                          << " Buf cre " << (allocate_buffers_end - allocate_buffers_start)
                          << " comp " << (compute_end - compute_start)
                          << " A2A " << (all_to_all_end - all_to_all_start - negative_time)
                          << " Buf free " << (free_buffers_end - free_buffers_start)
                          << " newt " << (insert_in_newt_end - insert_in_newt_start)
                          << std::endl;

                std::cout << name << " "  << mcomm.get_local_nprocs() << " Running time INNER LOOP [" << loop_count_tracker << " " << inner_loop << "] "
                          << " Buf cre " << *running_buffer_allocate
                          << " comp " << *running_local_compute
                          << " A2A " << *running_all_to_all
                          << " Buf free " << *running_buffer_free
                          << " newt " << *running_insert_newt
                          << std::endl;
#endif
                std::cout << loop_count_tracker << "\t"
                          << (allocate_buffers_end - allocate_buffers_start) << "\t"
                          << (compute_end - compute_start) << "\t"
                          << (all_to_all_end - all_to_all_start) << "\t"
                          << (free_buffers_end - free_buffers_start) << "\t"
                          << (insert_in_newt_end - insert_in_newt_start) << "\t";
            }
#endif
            inner_loop++;
        }

        double insert_in_full_start = MPI_Wtime();
        local_insert_in_full();
        double insert_in_full_end = MPI_Wtime();

        running_insert_in_full[task_id][*loop_counter] = (insert_in_full_end - insert_in_full_start);
        running_time[task_id][*loop_counter] = (insert_in_full_end - intra_start);

#if DEBUG_OUTPUT
        if (mcomm.get_rank() == 0)
        {
#if 0
            std::cout  << name << " " << mcomm.get_local_nprocs()<< " Current time OUTER LOOP [" << loop_count_tracker << " ] "
                       << " Intra " << (intra_end - intra_start)
                       << " full " << (insert_in_full_end - insert_in_full_start)
                       << " Total " << (insert_in_full_end - intra_start)
                       << " [ "
                       << *running_time
                       << " ]" << std::endl;

            std::cout  << name << " " << mcomm.get_local_nprocs() << " Running time OUTER LOOP [" << loop_count_tracker << "] "
                       << " Intra " << *running_intra_bucket_comm
                       << " full " << *running_insert_in_full
                       << " Total " << *running_intra_bucket_comm + *running_buffer_allocate + *running_local_compute + *running_all_to_all + *running_buffer_free + *running_insert_newt + *running_insert_in_full << std::endl;
#endif
            std::cout << (intra_end - intra_start) << "\t"
                      << (insert_in_full_end - insert_in_full_start)  << "\t"
                      << (insert_in_full_end - intra_start) << std::endl;

        }
#endif

        batch_size--;
        loop_count_tracker++;

        *loop_counter = *loop_counter + 1;
        if (iteration_count == 1)
            break;
    }

    delete[] offset;

    double fp_start = MPI_Wtime();
    check_for_fixed_point(history);
    double fp_end = MPI_Wtime();
    running_time[task_id][*loop_counter - 1] = running_time[task_id][*loop_counter - 1] + (fp_end - fp_start);
    running_fp[task_id][*loop_counter - 1] = (fp_end - fp_start);

    if (mcomm.get_rank() == 0)
    {
#if 0
        std::cout << name << " " << mcomm.get_local_nprocs() << " Fixed Point [" << loop_count_tracker << "] "
                  << (fp_end - fp_start)
                  << " "
                  << *running_fp
                  << std::endl;
#endif
    }


    if (logging == true)
        print_all_relation();
}



void RAM::execute_in_batches_comm_compaction(std::string name, int batch_size, std::vector<u32>& history, std::map<u64, u64>& intern_map, double **running_time, double **running_intra_bucket_comm, double **running_buffer_allocate, double **running_local_compute, double **running_all_to_all, double **running_buffer_free, double **running_insert_newt, double **running_insert_in_full, double **running_fp, double **running_a2a_find_count_time, double **running_a2a_create_rindex_time, double **running_a2a_total_find_blocks_time, double **running_a2a_total_pre_time, double **running_a2a_total_send_meda_time, double **running_a2a_total_comm_time, double **running_a2a_total_replace_time, double **running_a2a_exchange_time, double **running_a2a_filter_time,  int* loop_counter, int task_id, std::string output_dir, bool all_to_all_record, int*** all_to_all_buffer_size, int** compute_size1, int** compute_size2, int sloav_mode, int* rotate_index_array, int** send_indexes, int *sendb_num)
{
    int inner_loop = 0;
    u32 RA_count = RA_list.size();

    int *offset = new int[RA_count];
    for (u32 i =0; i < RA_count; i++)
        offset[i] = 0;

    while (batch_size != 0)
    {
#if DEBUG_OUTPUT
        //if (mcomm.get_rank() == 0)
        //    std::cout << "--------------FIXED POINT ITERATION " << loop_count_tracker << "--------------" << std::endl;
#endif


        double intra_start = MPI_Wtime();
        intra_bucket_comm_execute();
        double intra_end = MPI_Wtime();
        running_intra_bucket_comm[task_id][*loop_counter] = (intra_end - intra_start);

        bool local_join_status = false;
        while (local_join_status == false)
        {
            double allocate_buffers_start = MPI_Wtime();
            allocate_compute_buffers();
            double allocate_buffers_end = MPI_Wtime();
            running_buffer_allocate[task_id][*loop_counter] = (allocate_buffers_end - allocate_buffers_start);


            double compute_start = MPI_Wtime();
            local_join_status = local_compute(offset, *loop_counter, task_id, compute_size1, compute_size2);
            double compute_end = MPI_Wtime();
            running_local_compute[task_id][*loop_counter] = (compute_end - compute_start);

            if (mcomm.get_rank() == 0)
            {
                std::cout << output_dir << "\t"
                          << mcomm.get_local_nprocs() << "\t";
            }

            double all_to_all_start = MPI_Wtime();
            comm_compaction_all_to_all(compute_buffer, &cumulative_all_to_allv_recv_process_size_array, &cumulative_all_to_allv_buffer, mcomm.get_local_comm(), *loop_counter, task_id, output_dir, all_to_all_record, /*all_to_all_buffer_size*/NULL,
                                       running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, sloav_mode, rotate_index_array, send_indexes, sendb_num);
            double all_to_all_end = MPI_Wtime();
            running_all_to_all[task_id][*loop_counter] = (all_to_all_end - all_to_all_start);


            double free_buffers_start = MPI_Wtime();
            free_compute_buffers();
            double free_buffers_end = MPI_Wtime();
            running_buffer_free[task_id][*loop_counter] = (free_buffers_end - free_buffers_start);


            double insert_in_newt_start = MPI_Wtime();
            local_insert_in_newt_comm_compaction(intern_map);
            double insert_in_newt_end = MPI_Wtime();
            running_insert_newt[task_id][*loop_counter] = (insert_in_newt_end - insert_in_newt_start);


#if DEBUG_OUTPUT
            if (mcomm.get_rank() == 0)
            {
#if 0
                std::cout << name << " " << mcomm.get_local_nprocs() << " Current time INNER LOOP [" << loop_count_tracker << " " << inner_loop << "] "
                          << " Buf cre " << (allocate_buffers_end - allocate_buffers_start)
                          << " comp " << (compute_end - compute_start)
                          << " A2A " << (all_to_all_end - all_to_all_start - negative_time)
                          << " Buf free " << (free_buffers_end - free_buffers_start)
                          << " newt " << (insert_in_newt_end - insert_in_newt_start)
                          << std::endl;

                std::cout << name << " "  << mcomm.get_local_nprocs() << " Running time INNER LOOP [" << loop_count_tracker << " " << inner_loop << "] "
                          << " Buf cre " << *running_buffer_allocate
                          << " comp " << *running_local_compute
                          << " A2A " << *running_all_to_all
                          << " Buf free " << *running_buffer_free
                          << " newt " << *running_insert_newt
                          << std::endl;
#endif
                std::cout << loop_count_tracker << "\t"
                          << (allocate_buffers_end - allocate_buffers_start) << "\t"
                          << (compute_end - compute_start) << "\t"
                          << (all_to_all_end - all_to_all_start) << "\t"
                          << (free_buffers_end - free_buffers_start) << "\t"
                          << (insert_in_newt_end - insert_in_newt_start) << "\t";
            }
#endif
            inner_loop++;
        }

        double insert_in_full_start = MPI_Wtime();
        local_insert_in_full();
        double insert_in_full_end = MPI_Wtime();

        running_insert_in_full[task_id][*loop_counter] = (insert_in_full_end - insert_in_full_start);
        running_time[task_id][*loop_counter] = (insert_in_full_end - intra_start);

#if DEBUG_OUTPUT
        if (mcomm.get_rank() == 0)
        {
#if 0
            std::cout  << name << " " << mcomm.get_local_nprocs()<< " Current time OUTER LOOP [" << loop_count_tracker << " ] "
                       << " Intra " << (intra_end - intra_start)
                       << " full " << (insert_in_full_end - insert_in_full_start)
                       << " Total " << (insert_in_full_end - intra_start)
                       << " [ "
                       << *running_time
                       << " ]" << std::endl;

            std::cout  << name << " " << mcomm.get_local_nprocs() << " Running time OUTER LOOP [" << loop_count_tracker << "] "
                       << " Intra " << *running_intra_bucket_comm
                       << " full " << *running_insert_in_full
                       << " Total " << *running_intra_bucket_comm + *running_buffer_allocate + *running_local_compute + *running_all_to_all + *running_buffer_free + *running_insert_newt + *running_insert_in_full << std::endl;
#endif
            std::cout << (intra_end - intra_start) << "\t"
                      << (insert_in_full_end - insert_in_full_start)  << "\t"
                      << (insert_in_full_end - intra_start) << std::endl;

        }
#endif

        batch_size--;
        loop_count_tracker++;

        *loop_counter = *loop_counter + 1;
        if (iteration_count == 1)
            break;
    }

    delete[] offset;

    double fp_start = MPI_Wtime();
    check_for_fixed_point(history);
    double fp_end = MPI_Wtime();
    running_time[task_id][*loop_counter - 1] = running_time[task_id][*loop_counter - 1] + (fp_end - fp_start);
    running_fp[task_id][*loop_counter - 1] = (fp_end - fp_start);

    if (mcomm.get_rank() == 0)
    {
#if 0
        std::cout << name << " " << mcomm.get_local_nprocs() << " Fixed Point [" << loop_count_tracker << "] "
                  << (fp_end - fp_start)
                  << " "
                  << *running_fp
                  << std::endl;
#endif
    }


    if (logging == true)
        print_all_relation();
}