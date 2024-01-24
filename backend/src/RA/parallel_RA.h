/*
 * parallel RA
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#pragma once


class parallel_RA
{

protected:
    u32 RA_type;
    mpi_comm mcomm;

public:

    parallel_RA ()  {}
    virtual ~parallel_RA()    {}


    /// Join functions
    virtual relation* get_join_input0() {return NULL;}
    virtual int get_join_input0_graph_type() {return 0;}
    virtual relation* get_join_input1() {return NULL;}
    virtual int get_join_input1_graph_type() {return 0;}
    virtual relation* get_join_output() {return NULL;}
    virtual void get_join_projection_index(int** projection_reorder_index_array, int* projection_reorder_index_array_length) {return;}
    virtual int get_join_column_count () {return 0;}



    /// acopy functions
    virtual relation* get_acopy_input(){return NULL;}
    virtual int get_acopy_input0_graph_type(){return 0;}
    virtual relation* get_acopy_output(){return NULL;}
    virtual void get_acopy_rename_index(int** projection_reorder_index_array, int* projection_reorder_index_array_length) {return;}



    /// copy functions
    virtual relation* get_copy_input(){return NULL;}
    virtual int get_copy_input0_graph_type(){return 0;}
    virtual relation* get_copy_output(){return NULL;}
    virtual void get_copy_rename_index(int** projection_reorder_index_array, int* projection_reorder_index_array_length) {return;}


    /// filter copy functions
    virtual relation* get_copy_fiter_input(){return NULL;}
    virtual int get_copy_filter_input0_graph_type(){return 0;}
    virtual relation* get_copy_filter_output(){return NULL;}
    virtual void get_copy_filter_rename_index(int** projection_reorder_index_array, int* projection_reorder_index_array_length) {return;}

    // negation function
    virtual relation* get_negation_input() {return NULL;}
    virtual relation* get_negation_output() {return NULL;}
    virtual relation* get_negation_target() {return NULL;}
    virtual void get_negation_projection_index(std::vector<int>* projection_reorder_index_array) {return;}



    /// copy generate functions
    virtual relation* get_copy_generate_input(){return NULL;}
    virtual int get_copy_generate_input0_graph_type(){return 0;}
    virtual relation* get_copy_generate_output(){return NULL;}
    virtual void get_copy_generate_rename_index(int** projection_reorder_index_array, int* projection_reorder_index_array_length) {return;}


    virtual relation* get_relation(){return NULL;}

    /// MPI comm
    void set_comm(mpi_comm& mcomm)  {this->mcomm = mcomm;}



    /// JOIN, COPY, ACOPY
    int get_RA_type()   {return RA_type;}
};
