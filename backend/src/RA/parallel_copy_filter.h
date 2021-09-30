/*
 * copy
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#pragma once
#include "../ds.h"

class parallel_copy_filter: public parallel_RA
{

private:
    relation* copy_filter_input0_table;
    int copy_filter_input0_graph_type;

    relation* copy_filter_output_table;

    std::vector<int> copy_filter_reorder_index_array;
    bool(*lambda)(const u64* const);

public:
    parallel_copy_filter()
    {
        RA_type = COPY_FILTER;
    }

    parallel_copy_filter(relation* dest, relation* src, int src_version, std::vector<int> reorder_index_array, bool(*func)(const u64* const))
        : copy_filter_input0_table(src), copy_filter_input0_graph_type(src_version), copy_filter_output_table(dest), copy_filter_reorder_index_array(reorder_index_array), lambda(func)
    {
        RA_type = COPY_FILTER;
    }

    relation* get_copy_filter_input(){return copy_filter_input0_table;}
    relation* get_copy_filter_output() {return copy_filter_output_table; }
    int get_copy_filter_input0_graph_type() {return copy_filter_input0_graph_type;}
    void get_copy_filter_rename_index(std::vector<int>* projection_reorder_index_array) {*projection_reorder_index_array = this->copy_filter_reorder_index_array;}
#ifdef GOOGLE_MAP
    void local_copy_filter(u32 buckets, google_relation* input, u32* input_bucket_map, relation* output, std::vector<int> reorder_map, u32 arity, u32 join_column_count, all_to_allv_buffer& copy_filter_buffer, int ra_counter);
#else
    void local_copy_filter(u32 buckets, shmap_relation* input, u32* input_bucket_map, relation* output, std::vector<int> reorder_map, u32 arity, u32 join_column_count, all_to_allv_buffer& copy_filter_buffer, int ra_counter);
#endif
};
