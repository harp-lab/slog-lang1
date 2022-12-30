/*
 * join
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#pragma once
#include "../parallel_RA_inc.h"
#include "../ds.h"
#include <vector>

class parallel_join: public parallel_RA {

private:

    relation* join_input0_table;
    int join_input0_graph_type;

    relation* join_input1_table;
    int join_input1_graph_type;

    relation* join_output_table;

    std::vector<int> projection_reorder_index_array;
    int projection_reorder_index_array_length;

    // a function used to generate new tuple based on join input, target tuple (optional)
    // if this is provided, it will make join works similar to `copy_generate`
    join_generator_func_t generator_func;
    bool generator_mode = false;

public:
    parallel_join()
    {
        RA_type = JOIN;
    }

    /// Example scc13237->add_rule(new parallel_join(rel_path_2_1_2, rel_path_2_1, DELTA, rel_edge_2_2, FULL, {4, 2}));
    /// rel_path_2_1_2: destination relation
    /// rel_edge_2_1_2 has an arity of 2 (it therefore has three columns, one column is the intern column), this relation is hashed on its first (1) and second (2) column, if you look at the constructor it says it is hased on 2 columns as well. The name of the relation tells which order of column the relation is hashed in.
    ///
    /// rel_path_2_1: source 1 relation for join
    /// DELTA: source 1
    /// rel_path_2_1 has an arity of 2 (it therefore has three columns, one column is the intern column), this relation is hashed on its first (1) column
    /// In memory, in the nested b-tree the ordering is 1sr column, 2nd column, intern id column
    ///
    /// rel_edge_2_2: source 2 relation for join
    /// FULL: source 2
    /// rel_edge_2_2 has an arity of 2 (it therefore has three columns, one column is the intern column), this relation is hashed on its second (2) column
    /// In memory, in the nested b-tree the ordering is 2nd column, 1st column, intern id column
    ///
    ///
    /// Now, join between rel_path_2_1 (b, c, id) and rel_edge_2_2  (a, b, id)
    ///                   Example:     (2, 1, id1)                  (1, 2, id3)
    ///                                (3, 2, id2)                  (2, 3, id4)
    ///
    ///                   Join         (2, 1, id2, 3, id4), we want to get down to Join output:(3, 1)
    ///                                (1, 2, 3,   4, 5) ---> index
    /// we use the index {4, 2} to get the corect join output (3, 1), the id column is added in the later stage
    ///
    parallel_join(relation* output, relation* G, int G_type, relation* T, int T_type, std::vector<int> projection_reorder_index_array)
        : join_input0_table(G), join_input0_graph_type(G_type), join_input1_table(T), join_input1_graph_type(T_type), join_output_table(output), projection_reorder_index_array(projection_reorder_index_array)  {
        RA_type = JOIN;
    }


    relation* get_join_input0() {return join_input0_table;}
    int get_join_input0_graph_type()    {return join_input0_graph_type;}
    relation* get_join_input1() {return join_input1_table;}
    int get_join_input1_graph_type()    {return join_input1_graph_type;}
    relation* get_join_output() {return join_output_table;}
    void get_join_projection_index(std::vector<int>* projection_reorder_index_array)    {*projection_reorder_index_array = this->projection_reorder_index_array; }
    void set_generator_func(join_generator_func_t func) { generator_func = func; generator_mode = true; }

#ifdef GOOGLE_MAP
    bool local_join(int threshold, int* offset,
                    int join_order,
                    u32 buckets,
                    int input0_buffer_size, int input0_buffer_width, u64 *input0_buffer,
                    google_relation *input1, u32 i1_size, int input1_buffer_width,
                    std::vector<int> reorder_map_array,
                    relation* output,
                    all_to_allv_buffer& join_buffer,
                    int counter,
                    int join_column_count,
                    u32* local_join_duplicates,
                    u32* local_join_inserts);
#else
    bool local_join(int threshold, int* offset,
                    int join_order,
                    u32 buckets,
                    shmap_relation *input0,
                    int input0_buffer_size, int input0_buffer_width, u64 *input0_buffer,
                    shmap_relation *input1, u32 i1_size, int input1_buffer_width,
                    std::vector<int> reorder_map_array,
                    relation* output,
                    all_to_allv_buffer& join_buffer,
                    int counter,
                    int join_column_count,
                    u32* local_join_duplicates,
                    u32* local_join_inserts,
                    std::vector<double>& time_stat);

#endif
};
