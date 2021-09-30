/*
 * acopy
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#pragma once
#include "../ds.h"

class parallel_acopy: public parallel_RA
{

private:

    relation* acopy_input0_table;
    int acopy_input0_graph_type;

    relation* acopy_output_table;

    std::vector<int> acopy_reorder_index_array;

public:
    parallel_acopy()
    {
        RA_type = ACOPY;
    }

    /// Example: scc13237->add_rule(new parallel_acopy(rel_path_2_1, rel_path_2_1_2, DELTA, {0, 2, 1}));
    /// rel_edge_2_1: destination relation
    /// rel_path_2_1_2: source relation (copy from DELTA of source)
    /// {0, 2, 1}:
    ///             rel_edge_2_1[0] = rel_edge_2_1_2[0]
    ///             rel_edge_2_1[1] = rel_edge_2_1_2[2]
    ///             rel_edge_2_1[2] = rel_edge_2_1_2[1]
    ///
    ///
    /// Example: scc13238->add_rule(new parallel_acopy(rel_edge_2_2, rel_edge_2_1_2, DELTA, {1, 2, 0}));
    /// rel_edge_2_2: destination relation
    /// rel_edge_2_1_2: source relation (copy from DELTA of source)
    /// {0, 2, 1}:
    ///             rel_edge_2_1[0] = rel_edge_2_1_2[1]
    ///             rel_edge_2_1[1] = rel_edge_2_1_2[2]
    ///             rel_edge_2_1[2] = rel_edge_2_1_2[0]
    parallel_acopy(relation* dest, relation* src, int src_version, std::vector<int> reorder_index_array)
        : acopy_input0_table(src), acopy_input0_graph_type(src_version), acopy_output_table(dest), acopy_reorder_index_array(reorder_index_array)
    {
        RA_type = ACOPY;
    }

    relation* get_acopy_input(){return acopy_input0_table;}
    relation* get_acopy_output() {return acopy_output_table;}
    int get_acopy_input0_graph_type() {return acopy_input0_graph_type;}
    void get_acopy_rename_index(std::vector<int>* projection_reorder_index_array) {*projection_reorder_index_array = this->acopy_reorder_index_array;}

#ifdef GOOGLE_MAP
    void local_acopy(u32 buckets, google_relation* input, u32* input_bucket_map, relation* output, std::vector<int> reorder_map, u32 arity, u32 join_column_count, all_to_allv_buffer& acopy_buffer, int ra_counter);
#else
    void local_acopy(u32 buckets, shmap_relation* input, u32* input_bucket_map, relation* output, std::vector<int> reorder_map, u32 arity, u32 join_column_count, all_to_allv_buffer& acopy_buffer, int ra_counter);
#endif
};

