/*
 * join
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */

#pragma once
#include "../ds.h"
#include "../parallel_RA_inc.h"
#include "parallel_RA.h"
#include <vector>

// struct _BTree {
//   virtual bool has_key(const u64* key) = 0;
// };


class parallel_join_negate : public parallel_RA
{
private:
    /* data */
    relation* join_negation_input0_table;
    relation* join_negation_target_table;
    relation* join_negation_output_table;
    int src_type;
    std::vector<int> projection_reorder_index_array;
    int projection_reorder_index_array_length;

    int copy_generate_input0_graph_type;

public:
    parallel_join_negate()
    {
        RA_type = NEGATION;
    }

    parallel_join_negate(relation* dest, relation* src,  int t_type, relation* target_rel,
                            std::vector<int> projection_reorder_index_array)
        : join_negation_input0_table(src), join_negation_output_table(dest), join_negation_target_table(target_rel), src_type(t_type),
           projection_reorder_index_array(projection_reorder_index_array)
    {
        // std::cout << "init negate ..." << std::endl;
        RA_type = NEGATION;
    }

    relation* get_negation_input() {return join_negation_input0_table;}
    relation* get_negation_output() {return join_negation_output_table;}
    relation* get_negation_target() {return join_negation_target_table;}
    void get_negation_projection_index(std::vector<int>* projection_reorder_index_array)    {*projection_reorder_index_array = this->projection_reorder_index_array; }

    int get_src_graph_type() {return src_type;}

    bool local_negation(int threshold, int* offset,
                        int join_order,
                        u32 buckets,
                        int input0_buffer_size, 
                        int input0_buffer_width, u64 *input0_buffer,
                        shmap_relation *input1, u32 i1_size, int input1_buffer_width,
                        std::vector<int> &reorder_map_array,
                        relation* output,
                        all_to_allv_buffer& join_buffer,
                        int counter,
                        int join_column_count);

    void local_copy(u32 buckets,
                    shmap_relation* input,
                    u32* input_bucket_map,
                    relation* output,
                    std::vector<int> reorder_map,
                    u32 arity,
                    u32 join_column_count,
                    all_to_allv_buffer& copy_buffer,
                    int ra_counter);
};

class parallel_copy_aggregate : public parallel_RA
{

public:
    relation* copy_aggregate_output_table;
    relation* copy_aggregate_target_table;
    relation* copy_aggregate_input_table;
    SpecialAggregator agg_type;
    local_agg_func_t local_func;
    reduce_agg_func_t reduce_func;
    global_agg_func_t global_func;

    parallel_copy_aggregate(relation* output, relation* target_rel, relation* input,
                            int t_type, local_agg_func_t local_agg_func, 
                            SpecialAggregator special_agg, reduce_agg_func_t reduce_agg_func, 
                            global_agg_func_t global_agg_fun){
        copy_aggregate_output_table = output;
        copy_aggregate_input_table = input;
        copy_aggregate_target_table = target_rel;
        agg_type = special_agg;
        local_func = local_agg_func;
        reduce_func = reduce_agg_func;
        global_func = global_agg_fun;
        RA_type = AGGREGATION;
    }
    
    void local_aggregate(u32 bucket, int *offset,
                         int input0_buffer_size, u64 *input0_buffer,
                         all_to_allv_buffer& agg_buffer,
                         int ra_counter);
};


class parallel_join_aggregate : public parallel_RA
{

public:
    relation* copy_aggregate_output_table;
    relation* copy_aggregate_target_table;
    relation* copy_aggregate_input_table;
    SpecialAggregator agg_type;
    local_agg_func_t local_func;
    reduce_agg_func_t reduce_func;
    global_agg_func_t global_func;
    std::vector<u64> reorder_mapping;

    parallel_join_aggregate(relation* output, relation* target_rel, relation* input,
                            int t_type, local_agg_func_t local_agg_func, 
                            SpecialAggregator special_agg, reduce_agg_func_t reduce_agg_func, 
                            global_agg_func_t global_agg_fun, std::vector<int>& reorder_mapping){
        copy_aggregate_output_table = output;
        copy_aggregate_input_table = input;
        copy_aggregate_target_table = target_rel;
        agg_type = special_agg;
        local_func = local_agg_func;
        reduce_func = reduce_agg_func;
        global_func = global_agg_fun;
        RA_type = AGGREGATION;
        for (auto c : reorder_mapping) {
            this->reorder_mapping.push_back(c);
        }
    }
    
    void local_aggregate(u32 bucket, int *offset,
                         int input0_buffer_size, u64 *input0_buffer,
                         all_to_allv_buffer& agg_buffer,
                         int ra_counter);
};
