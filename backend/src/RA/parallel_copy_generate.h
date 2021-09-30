/*
 * copy
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#pragma once
#include "../ds.h"

class parallel_copy_generate: public parallel_RA
{

private:
    relation* copy_generate_input0_table;
    int copy_generate_input0_graph_type;

    relation* copy_generate_output_table;

    //std::vector<int> copy_generate_reorder_index_array;
    int(*lambda)(const u64* const, u64* const);

public:
    parallel_copy_generate()
    {
        RA_type = COPY_GENERATE;
    }

    parallel_copy_generate(relation* dest, relation* src, int src_version, int(*func)(const u64* const, u64* const))
        : copy_generate_input0_table(src), copy_generate_input0_graph_type(src_version), copy_generate_output_table(dest), lambda(func)
    {
        RA_type = COPY_GENERATE;
    }

    relation* get_copy_generate_input(){return copy_generate_input0_table;}
    relation* get_copy_generate_output() {return copy_generate_output_table; }
    int get_copy_generate_input0_graph_type() {return copy_generate_input0_graph_type;}
    //void get_copy_generate_rename_index(std::vector<int>* projection_reorder_index_array) {*projection_reorder_index_array = this->copy_generate_reorder_index_array;}
#ifdef GOOGLE_MAP
    void local_copy_generate(u32 buckets, google_relation* input, u32* input_bucket_map, relation* output, std::vector<int> reorder_map, u32 arity, u32 join_column_count, all_to_allv_buffer& copy_generate_buffer, int ra_counter);
#else
    void local_copy_generate(u32 buckets, shmap_relation* input, u32* input_bucket_map, relation* output, u32 arity, u32 join_column_count, all_to_allv_buffer& copy_generate_buffer, int ra_counter);
#endif
};
