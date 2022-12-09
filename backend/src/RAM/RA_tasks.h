/*
 * scc (tasks)
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#ifndef RAM_H
#define RAM_H



#include <vector>
class RAM
{

private:

    int ram_id;
    // bool init_status=false;

    int iteration_count = -1;                               /// Number of iterations in a fixed point

    bool logging = false;                                   /// If logging is enabled or not

    double refinement_factor = 4;                           /// For spatial balancing, a bucket is broken into 4 sub-buckets


    u32 ram_relation_count;

    std::vector<relation*> gc_relations;                     // the relation need to be gced after finish compute current SCC
    std::vector<relation*> ram_relations;
    //relation *ram_relations[1024];
    std::vector<bool> ram_relation_status;
    //bool ram_relation_status[1024];


    std::vector<parallel_RA*> RA_list;                      /// All relations of this SCC

    u64 *intra_bucket_buf_output_size;                      /// results of intra-bucket comm
    u64 **intra_bucket_buf_output;

    all_to_allv_buffer compute_buffer;                       /// result of compute

    all_to_all_buffer all_to_all_compute_buffer;                       /// result of compute

    u64 *cumulative_all_to_allv_buffer;                      /// result of all to all comm
    int* cumulative_all_to_allv_recv_process_count_array;

    u64 **cumulative_all_to_allv_buffer_cmp;                      /// result of all to all comm
    int* cumulative_all_to_allv_recv_process_size_array_cmp;

    mpi_comm mcomm;                                         /// comm related

    u32 loop_count_tracker;

public:

    double all_to_all_time = 0;

    ~RAM();
    RAM (bool ic, int ram_id);



    /// Set local task-level communicator
    void set_comm(mpi_comm& mcomm);


    /// For debugging purpose
    int get_id() {return ram_id;}


    /// add relations pertaining to this SCC
    /// void add_relation(relation* G) {ram_relations.insert(G);}



    //std::map<u32, std::map<relation*, bool>> get_RAM_relations()   {return ram_relations;}
    //relation** get_RAM_relations()   {return ram_relations;}
    std::vector<relation*> get_RAM_relations()   {return ram_relations;}
    std::vector<bool> get_RAM_relations_status()   {return ram_relation_status;}
    //bool* get_RAM_relations_status()   {return ram_relation_status;}


    /// add relations pertaining to this SCC
    void add_relation(relation*& G, bool i_status) { add_relation(G, i_status, false); }

    void add_relation(relation*& G, bool i_status, bool gc_flag);


    /// add rule to the SCC
    void add_rule(parallel_RA* pj) {RA_list.push_back(pj);}


    /// Load balancing related
    void set_refinement_factor(double rf)   {refinement_factor = rf;}


    /// Iteration count set through the constructor
    void set_iteration_count (int ic)   {iteration_count = ic;}
    int get_iteration_count ()   {return iteration_count;}


    /// for debugging
    void enable_logging()   {logging = true;}
    void print_all_relation();


    void io_all_relation(int status);


    /// the buckets over which the SCC is spread across
    u32 get_bucket_count() {return mcomm.get_local_nprocs();}


    u32 get_ram_relation_count() {return ram_relation_count;}

    std::vector<relation*>& get_gc_relation() { return gc_relations; }


    /// Spatial balancing of all relations
    void load_balance();


    /// Intra bucket comm for sub-buckets
    u64 intra_bucket_comm_execute();


    /// Buffer to hold new tuples
    u32 allocate_compute_buffers();

    u32 get_loop_count_tracker()    {return loop_count_tracker;}

    void allocate_all_to_all_compute_buffers();


    /// Join/compy/acopy
    bool local_compute(int* offset);

    void local_comm();

    /// Free intermediate buffers
    void free_compute_buffers();
    void free_all_to_all_compute_buffers();


    /// Update the head relation with new tuples
    void local_insert_in_newt(std::map<u64, u64>& intern_map);

    void local_insert_in_newt_comm_compaction(std::map<u64, u64>& intern_map);

    /// insert delta in full, copy newt pointer to delta
    void local_insert_in_full();


    void insert_delta_in_full();

    /// has fixed point reached
    void check_for_fixed_point(std::vector<u32>& history);

    // check whether a relation in this scc
    bool contains_relation(int tag);

    /// Start running this SCC (task) for "batck_size" iterations
    void execute_in_batches(std::string name, int batch_size, std::vector<u32>& history, std::map<u64, u64>& intern_map, int *loop_counter,int task_id, std::string output_dir, bool all_to_all_record, int sloav_mode, int* rotate_index_array, int** send_indexes, int *sendb_num);

    void execute_in_batches_comm_compaction(std::string name, int batch_size, std::vector<u32>& history, std::map<u64, u64>& intern_map, int* loop_counter, int task_id, std::string output_dir, bool all_to_all_record, int sloav_mode, int* rotate_index_array, int** send_indexes, int *sendb_num, std::vector<double>& runtime_vector);
};

#endif
