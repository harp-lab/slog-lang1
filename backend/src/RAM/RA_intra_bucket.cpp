#include "../parallel_RA_inc.h"
#include <iostream>

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
        else if ((*it)->get_RA_type() == AGGREGATION)
        {
            parallel_join_aggregate* current_ra = (parallel_join_aggregate*) *it;
            relation* input_rel = current_ra->join_aggregate_input_table;
            relation* target_rel = current_ra->join_aggregate_target_table;
            if (*(target_rel->get_sub_bucket_per_bucket_count()) == 1) {
                counter++;
                continue;
            } else {
                intra_bucket_comm(get_bucket_count(),
                                target_rel->get_full(),
                                target_rel->get_distinct_sub_bucket_rank_count(),
                                target_rel->get_distinct_sub_bucket_rank(),
                                target_rel->get_bucket_map(),
                                input_rel->get_distinct_sub_bucket_rank_count(),
                                input_rel->get_distinct_sub_bucket_rank(),
                                input_rel->get_bucket_map(),
                                &intra_bucket_buf_output_size[counter],
                                &intra_bucket_buf_output[counter],
                                mcomm.get_local_comm());
            }
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

        else if ((*it)->get_RA_type() == NEGATION)
        {
            parallel_join_negate* current_ra = (parallel_join_negate*) *it;
            relation* input_rel = current_ra->get_negation_input();
            relation* target_rel = current_ra->get_negation_target();

            // negation can only be a right like join, all process need to know negated rule
            intra_bucket_comm(get_bucket_count(),
                              target_rel->get_full(),
                              target_rel->get_distinct_sub_bucket_rank_count(),
                              target_rel->get_distinct_sub_bucket_rank(),
                              target_rel->get_bucket_map(),
                              input_rel->get_distinct_sub_bucket_rank_count(),
                              input_rel->get_distinct_sub_bucket_rank(),
                              input_rel->get_bucket_map(),
                              &intra_bucket_buf_output_size[counter],
                              &intra_bucket_buf_output[counter],
                              mcomm.get_local_comm());
            total_data_moved = total_data_moved + intra_bucket_buf_output_size[counter];
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