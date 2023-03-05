#include "../parallel_RA_inc.h"
#include <iostream>


bool RAM::local_compute(int* offset)
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

        else if ((*it)->get_RA_type() == AGGREGATION)
        {
            // parallel_copy_aggregate* current_ra = (parallel_copy_aggregate*) *it;
            parallel_join_aggregate* current_ra = (parallel_join_aggregate*) *it;
            current_ra->local_aggregate(
                get_bucket_count(),
                &(offset[counter]),
                intra_bucket_buf_output_size[counter],
                intra_bucket_buf_output[counter],
                compute_buffer,
                counter);
        }



        else if ((*it)->get_RA_type() == NEGATION)
        {
            // compute negation
            parallel_join_negate* current_ra = (parallel_join_negate*) *it;
            relation* output_relation = current_ra->get_negation_output();
            relation* input_relation = current_ra->get_negation_input();
            relation* target_relation = current_ra->get_negation_target();
            std::vector<int> reorder_map_array;
            current_ra->get_negation_projection_index(&reorder_map_array);
            int join_column_count = target_relation->get_join_column_count();

            shmap_relation* input_rel_trie = NULL;
            int input_size = 0;
            if  (current_ra->get_src_graph_type() == DELTA)
            {
                input_rel_trie = input_relation->get_delta();
                input_size = input_relation->get_delta_element_count();
            }
            else
            {
                input_rel_trie = input_relation->get_full();
                input_size = input_relation->get_full_element_count();
            }
            if (intra_bucket_buf_output_size[counter] == 0)
            {
                current_ra->local_copy(
                    get_bucket_count(), input_rel_trie,
                    input_relation->get_bucket_map(), output_relation,
                    reorder_map_array, output_relation->get_arity(),
                    input_relation->get_join_column_count(),
                    compute_buffer, counter);
                join_completed = true;
            }
            else
            {
                join_completed = join_completed & current_ra->local_negation(
                    threshold,&(offset[counter]),
                    RIGHT,
                    get_bucket_count(),
                    intra_bucket_buf_output_size[counter],
                    target_relation->get_arity()+1, intra_bucket_buf_output[counter],
                    input_rel_trie, input_size,
                    input_relation->get_arity()+1,
                    reorder_map_array,
                    output_relation,
                    compute_buffer,
                    counter,
                    join_column_count);
            }
        }

        else if ((*it)->get_RA_type() == FACT)
        {
            fact* current_ra = (fact*) *it;
            relation* fact_relation = current_ra->get_relation();
            // if (mcomm.get_rank()==0)
            current_ra->init_with_fact(get_bucket_count(),
                                    fact_relation->get_sub_bucket_per_bucket_count(),
                                    fact_relation->get_sub_bucket_rank(),
                                    fact_relation->get_arity(),
                                    fact_relation->get_join_column_count(),
                                    fact_relation->get_is_canonical(),
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
        }
        counter++;
    }

#if 0
    int global_total_join_tuples = 0;
    int global_join_tuples_duplicates = 0;
    MPI_Allreduce(&total_join_tuples, &global_total_join_tuples, 1, MPI_INT, MPI_SUM, mcomm.get_local_comm());
    MPI_Allreduce(&join_tuples_duplicates, &global_join_tuples_duplicates, 1, MPI_INT, MPI_SUM, mcomm.get_local_comm());
    if (mcomm.get_rank() == 0)
        std::cout << "Joins: " << global_total_join_tuples << " Duplicates " << global_join_tuples_duplicates << " " << std::endl;
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
            {
                delete[] intra_bucket_buf_output[counter];
            }

            if (current_ra->get_RA_type() == NEGATION)
            {
                // parallel_join_negate* _ra = (parallel_join_negate*)current_ra;
                delete[] intra_bucket_buf_output[counter];
            }
            if (current_ra->get_RA_type() == AGGREGATION) {
                parallel_join_aggregate* _ra = (parallel_join_aggregate*)current_ra;
                if (*(_ra->join_aggregate_target_table->get_sub_bucket_per_bucket_count()) != 1) {
                    delete[] intra_bucket_buf_output[counter];
                }
            }

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