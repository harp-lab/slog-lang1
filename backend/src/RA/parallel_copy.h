/*
 * copy
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#pragma once
#include "../ds.h"

class parallel_copy: public parallel_RA
{

private:
    relation* copy_input0_table;
    int copy_input0_graph_type;

    relation* copy_output_table;

    std::vector<int> copy_reorder_index_array;

public:
    parallel_copy()
    {
        RA_type = COPY;
    }

    parallel_copy(relation* dest, relation* src, int src_version, std::vector<int> reorder_index_array)
        : copy_input0_table(src), copy_input0_graph_type(src_version), copy_output_table(dest), copy_reorder_index_array(reorder_index_array)
    {
        RA_type = COPY;
    }

    relation* get_copy_input(){return copy_input0_table;}
    relation* get_copy_output() {return copy_output_table; }
    int get_copy_input0_graph_type() {return copy_input0_graph_type;}
    void get_copy_rename_index(std::vector<int>* projection_reorder_index_array) {*projection_reorder_index_array = this->copy_reorder_index_array;}

 #ifdef GOOGLE_MAP
    void local_copy(u32 buckets, google_relation* input, u32* input_bucket_map, relation* output, std::vector<int> reorder_map, u32 arity, u32 join_column_count, all_to_allv_buffer& copy_buffer, int ra_counter);
#else
    void local_copy(u32 buckets, shmap_relation* input, u32* input_bucket_map, relation* output, std::vector<int> reorder_map, u32 arity, u32 join_column_count, all_to_allv_buffer& copy_buffer, int ra_counter);

#endif
};
