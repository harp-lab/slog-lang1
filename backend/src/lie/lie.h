/*
 * Logical Inferencing Engine (LIE)
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */


#ifndef LIE_H
#define LIE_H


class LIE
{
private:

    u32 batch_size=10;                                              /// Number of iterations between two fixed-point checks
    double batch_time;                                              /// Wallclock time between two fixed-point checks
    mpi_comm mcomm;                                                 /// MPI class

    std::string app_name;
    u32 lie_relation_count;
    std::vector<relation*> lie_relations;

    //u32 lie_relations_key;
    //std::map<u32, relation*> lie_relations;                    /// List of all relations

    u32 lie_sccs_count;
    std::vector<RAM*> lie_sccs;
    //RAM *lie_sccs[2048];
    //int iteration_count[2048];

    //u32 lie_sccs_key;
    //std::map<u32, RAM*> lie_sccs;                                 /// List of all tasks

    std::map<RAM*, std::set<RAM*>> taskgraph;  /// List of all edges in a task (this is an adjacency list)

    std::map<u64, u64> intern_map;                        /// Intern table

    bool enable_data_io;

    bool enable_io;

    bool share_io;                                        /// whether using MPI collective IO to write file

    //bool offset_io;										 /// whether read checkpoint dump with offset

    //bool separate_io;                                    /// whether write checkpoint dump separately for each process

    std::string output_dir;

    std::map<int, std::tuple<u64, int , bool>> rel_size_map;   // {rel_tag |-> (size, arity, intermediate?)}

public:

    ~LIE();

    LIE()
    {
    	enable_data_io = false;
        enable_io = false;
        lie_relation_count = 0;
        lie_sccs_count = 0;
        taskgraph = {{},{}};
        intern_map = {{},{}};
    }

    void set_output_dir(std::string output)   {output_dir = output;}

    void enable_share_io()    {share_io = true;}

    void enable_IO()    {enable_io = true;}
  
    void enable_data_IO()    {enable_data_io = true;}

    void print_all_relation_size();

    void print_relation_size(relation* rel);

    /// Sets the communicator object
    void set_comm(mpi_comm comm)   { mcomm = comm;  }


    void set_name(std::string name)   { app_name = name;  }


    /// Batch size
    void set_batch_size (u32 bs)    {batch_size = bs;}
    u32 get_batch_size ()    {return batch_size;}


    /// Batch time
    void set_batch_time (double bt)    {batch_time = bt;}
    double get_batch_time ()    {return batch_time;}


    /// Adds a new relation to the LIE
    void add_relation(relation* rel);


    /// Adds a new SCC to the LIE
    void add_scc(RAM* ra);


    /// Returns a list of infiished tasks
    RAM* one_runnable_tasks();


    /// Removes tasks from the task edge list and tas list
    void update_task_graph(RAM* removable_tasks);


    /// Populates the adjacency list of tasks
    void add_scc_dependance (RAM* src_task, RAM* destination_task);


    /// Runs all tasks within the LIE, following the dependence as set by taskgraph
    bool execute();

    void write_final_checkpoint_dump();

    void write_final_checkpoint_dump(relation* rel);

    void write_checkpoint_dump(int loop_counter, std::vector<int> executed_scc_id, int scc_id);

    // print some infomation about how the overhead of intermediate relation
    void stat_intermediate();
};

#endif