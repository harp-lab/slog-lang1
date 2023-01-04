/*
 * Relation class
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#pragma once

#include "../ds.h"
#include "../parallel_RA_inc.h"
#include <algorithm>
#include <functional>
#include <optional>
#include <string>
#include <vector>

enum {LEFT=0, RIGHT};
enum {DELTA=0, FULL, FULL_AND_DELTA};
enum {COPY=0, COPY_FILTER, COPY_GENERATE, ACOPY, JOIN, FACT, NEGATION, AGGREGATION, UPDATE};
enum {STATIC=0, DYNAMIC};
enum {INSERT_SUCCESS=0, INSERT_FAIL, INSERT_UPDATED};

using tuple_formator_t = std::function<void(const std::vector<u64>&)>;

// this is update function for column has functional dependence
// the size of vector arguments must have exactly same size as dependent_column_indices

class relation
{

private:

    u32 join_column_count;                      /// Number of join column counts
    bool is_canonical;
    u32 arity;                                  /// Arity of relation
    u32 intern_tag;                             /// id of relation (to be used for interning)

    std::string debug_id;
    int initialization_type = -1;               /// used when task balancing is required
    std::string filename = NULL;                /// Name of file to open


    int last_rank;                              /// Used to store last rank


#ifdef GOOGLE_MAP
    google_relation *newt;                     /// Newt
#else
    shmap_relation *newt;                     /// Newt
#endif
    u32 newt_element_count;
    u32 **newt_sub_bucket_element_count;
    u32 *newt_bucket_element_count;

#ifdef GOOGLE_MAP
    google_relation *full;                     /// Full
#else
    shmap_relation *full;                     /// Full
#endif
    u32 full_element_count;
    u32 **full_sub_bucket_element_count;
    u32 *full_bucket_element_count;

#ifdef GOOGLE_MAP
    google_relation *delta;                    /// Delta
#else
    shmap_relation *delta;                     /// Delta
#endif
    u32 delta_element_count;
    u32 **delta_sub_bucket_element_count;
    u32 *delta_bucket_element_count;

    u32 default_sub_bucket_per_bucket_count;    /// 1
    u32 *sub_bucket_per_bucket_count;           /// sub_bucket_per_bucket_count[i] holds the total number of sub-buckets at bucket index i
    u32** sub_bucket_rank;                      /// target rank of a subbucket

    int** distinct_sub_bucket_rank;             /// used for intra-bucket comm
    int* distinct_sub_bucket_rank_count;

    u32 *bucket_map;                            /// list the buckets which exists on a process

    mpi_comm mcomm;                             /// comm related
    parallel_io file_io;                        /// to handle parallel IO

    bool offset_io;
    bool share_io;
    bool separate_io;
    bool restart_flag;
    //bool fact_load=false;
    //std::vector<u64> init_val;
    std::vector<int> dependent_column_indices;
    update_partial_compare_func_t update_compare_func;

    // This is only used when this relation need to be reused in another computation loop
    bool init_flag = true;

public:

    /// Example: relation* rel_path_2_1_2 = new relation(2, true, 2, 257, "rel_path_2_1_2", "../data/g5955/path_2_1_2", FULL);
    /// 2: arity (Internally one extra id (intern id) column is added to every relation)
    /// true: arity == join column count
    /// 2: join column count
    /// 257: index id
    /// "rel_path_2_1_2": name of relation
    /// "/var/tmp/g13236/path_2_1_2": location of data file that gets loaded in the relation
    /// FULL: load in FULL (other option is to loadin DELTA, but we alwys load in FULL)
    relation (u32 jcc, bool is_c, u32 ar, u32 tg, std::string fname, int version)
        :join_column_count(jcc), is_canonical(is_c), arity(ar), intern_tag(tg), initialization_type(version), filename(fname)
    {
        //fact_load = false;
        full_element_count=0;
        delta_bucket_element_count=0;
    }

    relation (u32 jcc, bool is_c, u32 ar, u32 tg, std::string did, std::string fname, int version)
        :join_column_count(jcc), is_canonical(is_c), arity(ar), intern_tag(tg), debug_id(did), initialization_type(version), filename(fname)
    {
        //fact_load = false;
        full_element_count=0;
        delta_bucket_element_count=0;
    }

    relation (u32 jcc, bool is_c, u32 ar, u32 tg, int version)
        :join_column_count(jcc), is_canonical(is_c), arity(ar), intern_tag(tg), initialization_type(version), filename("")
    {
        //fact_load = false;
        full_element_count=0;
        delta_bucket_element_count=0;
    }

    void set_restart_flag(bool restart)    {restart_flag = restart;}

    void set_offset_io(bool offset)   {offset_io = offset;}

    void set_share_io(bool share)   {share_io = share;}

    void set_separate_io(bool separate)   {separate_io = separate;}

    std::string get_filename()       {return filename;}

    void set_filename(std::string file)       {filename =file;}

    /// set comm
    void set_mcomm(mpi_comm& mc)    {mcomm = mc;}

    //void set_fact_load() {fact_load = true;}

    //void set_init_val(std::vector<u64> temp_init_val)   {init_val = temp_init_val;}

    void set_dependent_column_update(std::vector<int> idx, update_partial_compare_func_t f) {
        dependent_column_indices = idx;
        update_compare_func= f;
        // for (int i = 0; i < get_bucket_count(); i++) {
        //     delta[i].dependent_column_indices = dependent_column_indices;
        //     delta[i].update_compare_func = update_compare_func;
        //     full[i].dependent_column_indices = dependent_column_indices;
        //     full[i].update_compare_func = update_compare_func;
        //     newt[i].dependent_column_indices = dependent_column_indices;
        //     newt[i].update_compare_func = update_compare_func;
        // }
    }
    std::vector<int> get_dependent_column() { return dependent_column_indices; }
    update_partial_compare_func_t get_update_compare_func() { return update_compare_func; }

    /// used for load balancing
    void set_last_rank(int lr)   {last_rank = lr;}
    int get_last_rank() {   return last_rank;}

    /// used for task-level parallelism
    void set_initialization_type(int x) { initialization_type = x;  }


    bool get_is_canonical() {return is_canonical;}


    u32 get_arity ()    {return arity;}
    int get_join_column_count ()    {return (int)join_column_count;}


    std::string get_debug_id()  {   return debug_id; }

    int get_bucket_count()  {   return mcomm.get_local_nprocs(); }
    u32* get_bucket_map()   {return bucket_map;}


    int* get_distinct_sub_bucket_rank_count()   {return distinct_sub_bucket_rank_count;}
    int** get_distinct_sub_bucket_rank()    {return distinct_sub_bucket_rank;}


    void set_full_element_count(int val)   {full_element_count = val;}
    int get_full_element_count()    {
        u64 res = 0;
        for (int i = 0; i < get_bucket_count();  i++) {
            res += full[i].size();
        }
        return res;
    }
    u32** get_full_sub_bucket_element_count()   {return full_sub_bucket_element_count;}
    u32 get_global_full_element_count();


    int get_new_element_count() {return newt_element_count;}
    u32** get_new_sub_bucket_element_count()    {return newt_sub_bucket_element_count;}


    u32* get_sub_bucket_per_bucket_count() {return sub_bucket_per_bucket_count;}
    u32** get_sub_bucket_rank() {return sub_bucket_rank;}


    u32 get_intern_tag()    {return intern_tag;}

#ifdef GOOGLE_MAP
    google_relation* get_full() {return full;}
    google_relation* get_newt() {return newt;}
    google_relation* get_delta()    {return delta;}
#else
    shmap_relation* get_full() {return full;}
    shmap_relation* get_newt() {return newt;}
    shmap_relation* get_delta()    {return delta;}
#endif

    void set_delta_element_count(int val)   {delta_element_count = val;}
    int get_delta_element_count()   {
        u64 res = 0;
        for (int i = 0; i < get_bucket_count();  i++) {
            res += delta[i].size();
        }
        return res;
    }
    u32** get_delta_sub_bucket_element_count()  {return delta_sub_bucket_element_count;}
    u32 get_global_delta_element_count();


    void set_default_sub_bucket_per_bucket_count(u32 sbc)    {default_sub_bucket_per_bucket_count = sbc;}
    u32 get_default_sub_bucket_per_bucket_count()    {return default_sub_bucket_per_bucket_count;}


    /// print all tuples of newt, delta and full
    void print();
    void print(tuple_formator_t ft);


    void serial_IO(std::string filename_template);
    void parallel_IO(std::string filename_template);


    /// used for initialization of dynamic relations
    //void flush_full();
    //void read_from_relation(relation* input, int full_deta);


    /// initialize and finalize relation
    void initialize_relation(mpi_comm& mcomm, std::map<u64, u64>& intern_map);
    void populate_full(int buffer_size, u64* buffer);
    void populate_delta (int buffer_size, u64* buffer);
    //void populate_delta_with_keys (int buffer_size, u64* buffer, std::map<u64, u64>& intern_map);
    void finalize_relation();


    /// load data from file into full or delta buffer
    void load_data_from_file();
    void load_data_from_file_with_offset();
    void load_data_from_separate_files();


    /// for task parallelism, copying relation from exiting comm to output_comm
    void copy_relation(relation*& recv_rel, mpi_comm output_comm, int target_cumulative_rank, int tuples_per_task, u32 ib,  u32 ob);


    /// Find
    bool find_in_full(u64* t, int length);
    bool find_in_delta(u64* t, int length);
    bool find_in_newt(u64* t, int length);


    /// Insert
    bool insert_in_delta(u64* t);
    bool insert_in_newt(u64* t);
    bool insert_in_full(u64* t);


    /// part of semi-naive evaluation
    int insert_delta_in_full();
    int insert_full_in_delta();
    void local_insert_in_delta();
    void copy_newt_to_delta()   {delta = newt;}

    // lattice value check
    bool check_dependent_value_insert_avalible(const std::vector<u64>& tuple);

    /// for load balancing (implemented in relation_load_balance.cpp)
    bool load_balance_merge_full_and_delta(float rf);
    bool load_balance_split_full_and_delta(float rf);

    // check if a relation is intermediate relation (which means either non-canonical index or relation generated 
    // by slog compiler)
    bool is_intermediate_relation() {
        auto found_doller = debug_id.find('$');
        if (found_doller != std::string::npos) {
            return true;
        }
        return !is_canonical;
    }

    // skip initialization/loading facts
    void disable_initialization() { init_flag = false; }
    void enable_initialization() { init_flag = true; }
    bool need_init_huh() { return init_flag; }

    void test_calc_hash_rank(u64 rank_n);
};
