#include "../parallel_RA_inc.h"
#include <iostream>

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
        u32 tuples_to_read = cumulative_all_to_allv_recv_process_count_array[k];
        relation* output;

        if (RA_list[ra_id]->get_RA_type() == COPY)
            output = RA_list[ra_id]->get_copy_output();
        else if (RA_list[ra_id]->get_RA_type() == COPY_FILTER)
            output = RA_list[ra_id]->get_copy_filter_output();
        else if (RA_list[ra_id]->get_RA_type() == NEGATION)
            output = RA_list[ra_id]->get_negation_output();
        else if (RA_list[ra_id]->get_RA_type() == JOIN)
            output = RA_list[ra_id]->get_join_output();
        else if (RA_list[ra_id]->get_RA_type() == COPY_GENERATE)
            output = RA_list[ra_id]->get_copy_generate_output();
        else if (RA_list[ra_id]->get_RA_type() == FACT)
            output = RA_list[ra_id]->get_relation();
        else
            output = RA_list[ra_id]->get_acopy_output();

        if (RA_list[ra_id]->get_RA_type() == COPY || RA_list[ra_id]->get_RA_type() == JOIN || RA_list[ra_id]->get_RA_type() == NEGATION || RA_list[ra_id]->get_RA_type() == COPY_FILTER || RA_list[ra_id]->get_RA_type() == COPY_GENERATE || RA_list[ra_id]->get_RA_type() == FACT)
        {
            u32 width = output->get_arity();
            u64 tuple[width + 1];

            u32 elements_to_read = tuples_to_read * width;
            for (int tuple_ind = 0; tuple_ind < tuples_to_read; tuple_ind ++)
            {
                u32 x = starting + tuple_ind * width;
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
                // else {
                    // std::cout << "insert fail ";
                    // for (int i = 0; i < width; i++) {
                    //     std::cout << cumulative_all_to_allv_buffer[i] << " ";
                // }
                // std::cout << std::endl;
                // }
            }
            starting = starting + elements_to_read;
        }
        else if (RA_list[ra_id]->get_RA_type() == ACOPY)
        {
            //std::cout <<  "ACOPY finishing" << std::endl;
            u32 width = output->get_arity() + 1;
            u64 tuple[width];
            successful_insert = 0;
            u32 elements_to_read = tuples_to_read * width;
            for (int tuple_ind = 0; tuple_ind < tuples_to_read; tuple_ind ++)
            {
                u32 x = starting + tuple_ind * width;
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
            starting = starting + elements_to_read;
        }
        //else if (RA_list[ra_id]->get_RA_type() == FACT)
        //    continue;

        //std::cout << output->get_debug_id() << " successful insert " << successful_insert << std::endl;
    }

    delete[] cumulative_all_to_allv_recv_process_count_array;
    delete[] cumulative_all_to_allv_buffer;
}