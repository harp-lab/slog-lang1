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


class parallel_copy_aggregate : public parallel_RA
{
private:
    /* data */
    relation* copy_aggregate_input0_table;
    relation* copy_aggregate_target_table;
    relation* copy_aggregate_output_table;
    std::vector<int> joined_cols;

    int copy_generate_input0_graph_type;

public:
    parallel_copy_aggregate()
    {
        RA_type = COPY_AGGREGATE;
    }

    parallel_copy_aggregate(relation* dest, relation* src, relation* target_rel, 
                            vector<int> cols)
        : copy_aggregate_output_table(dest), copy_aggregate_input0_table(src),
          copy_aggregate_target_table(target_rel), joined_cols(cols)
    {
        RA_type = COPY_AGGREGATE;
    }

    relation* get_copy_aggregate_input() {return copy_aggregate_input0_table;}
    relation* get_copy_aggregate_output() {return copy_aggregate_output_table;}
    relation* get_copy_aggregate_target() {return copy_aggregate_target_table;}
    relation* get_joined_column(std::vector<int>* joined_col) {*joined_cols = this->joined_cols;}

    void local_aggregate_generate(int threshold, int* offset,
                                  int join_order,
                                  u32 buckets,
                                  int input0_buffer_size, int input0_buffer_width, u64 *input0_buffer,
                                  shmap_relation *input1, u32 i1_size, int input1_buffer_width,
                                  std::vector<int> reorder_map_array,
                                  relation* output,
                                  all_to_allv_buffer& join_buffer,
                                  int counter,
                                  int join_colun_count,
                                  u32* local_join_duplicates,
                                  u32* local_join_inserts);

};
