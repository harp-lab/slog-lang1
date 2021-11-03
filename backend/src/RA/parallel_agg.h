/*
 * join
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */

#pragma once
#include "../ds.h"

using local_agg_res_t = u64;
// struct _BTree {
//   virtual bool has_key(const u64* key) = 0;
// };

typedef local_agg_res_t *local_agg_func_t (shmap_relation* agg_rel, const u64* data);

typedef local_agg_res_t *reduce_agg_func_t (local_agg_res_t x, local_agg_res_t y);

typedef int *global_agg_func_t (u64* data, local_agg_res_t agg_data, int agg_data_count, u64* output); 


class parallel_join_negate : public parallel_RA
{
private:
    /* data */
    relation* copy_negation_input0_table;
    relation* copy_negation_target_table;
    relation* copy_negation_output_table;
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
        : copy_negation_output_table(dest), copy_negation_input0_table(src), src_type(t_type),
          copy_negation_target_table(target_rel), projection_reorder_index_array(projection_reorder_index_array)
    {
        // std::cout << "init negate ..." << std::endl;
        RA_type = NEGATION;
    }

    relation* get_negation_input() {return copy_negation_input0_table;}
    relation* get_negation_output() {return copy_negation_output_table;}
    relation* get_negation_target() {return copy_negation_target_table;}
    void get_negation_projection_index(std::vector<int>* projection_reorder_index_array)    {*projection_reorder_index_array = this->projection_reorder_index_array; }

    int get_src_graph_type() {return src_type;}

    bool local_negation(int threshold, int* offset,
                                  int join_order,
                                  u32 buckets,
                                  int input0_buffer_size, int input0_buffer_width, u64 *input0_buffer,
                                  shmap_relation *input0,
                                  shmap_relation *input1, u32 i1_size, int input1_buffer_width,
                                  std::vector<int> reorder_map_array,
                                  relation* output,
                                  all_to_allv_buffer& join_buffer,
                                  int counter,
                                  int join_column_count,
                                  u32* local_join_duplicates,
                                  u32* local_join_inserts);

};
