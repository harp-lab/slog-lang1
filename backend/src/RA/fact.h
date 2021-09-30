/*
 * acopy
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#pragma once
#include "../ds.h"

class fact: public parallel_RA
{

private:

    relation* fact_rel;
    std::vector<u64> data;



public:
    fact()
    {
        RA_type = FACT;
    }

    fact(relation* rel, std::vector<u64> temp_init_val)
    {
        RA_type = FACT;
        fact_rel = rel;
        data = temp_init_val;

        //fact_rel->set_fact_load();
        //fact_rel->set_init_val(temp_init_val);
        //init_val = temp_init_val;
        /*
        if (init_val.size() != 0)
        {
            std::cout << "size = " << init_val.size() << std::endl;
            std::cout << "Filename" << fact_rel->get_filename() << std::endl;

            u64* temp = new u64[init_val.size()];
            for (u32 i=0; i < init_val.size(); i++)
                temp[i] = init_val[i];
            std::cout << "Temp " << temp[0] << std::endl;
            //fact_rel->populate_delta(init_val.size(), reinterpret_cast<u64*>(init_val.data()));
            //fact_rel->populate_delta(init_val.size(), temp);

            fact_rel->populate_delta_with_keys(init_val.size(), temp);
            //fact_rel->populate_delta_with_keys(init_val.size(), reinterpret_cast<u64*>(init_val.data()));
        }
        */
    }

    void init_with_fact(u32 buckets, u32* output_sub_bucket_count, u32** output_sub_bucket_rank, int arity, int join_col_count, bool canonical, std::vector<u64> data, all_to_allv_buffer& buffer, int ra_id)
    {
        buffer.width[ra_id] = arity;
        u64 reordered_cur_path[arity];
        for (int j =0; j < arity; j++)
            reordered_cur_path[j] = data[j];

        uint64_t bucket_id = tuple_hash(reordered_cur_path, join_col_count) % buckets;
        uint64_t sub_bucket_id=0;
        if (canonical == false && arity != 0)
            sub_bucket_id = tuple_hash(reordered_cur_path + join_col_count, arity-join_col_count) % output_sub_bucket_count[bucket_id];


        int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];


        buffer.local_compute_output_size_rel[ra_id] = buffer.local_compute_output_size_rel[ra_id] + buffer.width[ra_id];
        buffer.local_compute_output_size_total = buffer.local_compute_output_size_total + buffer.width[ra_id];
        buffer.local_compute_output_size_flat[index * buffer.ra_count + ra_id] = buffer.local_compute_output_size_flat[index * buffer.ra_count + ra_id] + buffer.width[ra_id];
        buffer.local_compute_output_size[ra_id][index] = buffer.local_compute_output_size[ra_id][index] + buffer.width[ra_id];
        buffer.cumulative_tuple_process_map[index] = buffer.cumulative_tuple_process_map[index] + buffer.width[ra_id];
        buffer.local_compute_output[ra_id][index].vector_buffer_append((const unsigned char*)reordered_cur_path, sizeof(u64)*buffer.width[ra_id]);

        //std::cout << "Fact " << reordered_cur_path[0] << " " << reordered_cur_path[1] << std::endl;

        //u32** output_sub_bucket_rank = output->get_sub_bucket_rank();

        //acopy_buffer.width[ra_counter] = reorder_map.size();
        //assert(acopy_buffer.width[ra_counter] == (int)output->get_arity()+1);
        //assert(arity + 1 == (int)output->get_arity());

        //for (u32 i = 0; i < buckets; i++)
        //    if (input_bucket_map[i] == 1)
        //        output[i].as_vector_buffer_recursive();
        //init_buffer(acopy_buffer, {}, reorder_map, ra_counter, buckets, output_sub_bucket_count, output_sub_bucket_rank, arity, join_column_count, output->get_join_column_count(), output->get_is_canonical());

    }

    relation* get_relation(){return fact_rel;}

    std::vector<u64> get_init_data(){return data;}
};

