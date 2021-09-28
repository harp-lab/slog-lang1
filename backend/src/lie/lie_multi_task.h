#ifndef LIE_mt_H
#define LIE_mt_H



class LIE_mt
{
private:
    int mode=1;
    u32 batch_size;
    double batch_time;
    float task_threshold=1.1;
    mpi_comm mcomm;

    std::unordered_set<relation*> lie_relations;
    std::unordered_set<RAM*> tasks;
    std::unordered_map<RAM*, std::unordered_set<RAM*>> taskgraph1;
    std::unordered_map<u64, u64> intern_map;


public:
    void add_scc(RAM* ra)    {    tasks.insert(ra);    }

    void add_relation(relation* rel)    {    lie_relations.insert(rel);    }

    void set_comm(mpi_comm comm)   { mcomm = comm;  }

    void set_batch_size (u32 bs)    {batch_size = bs;}
    u32 get_batch_size ()    {return batch_size;}

    void set_batch_time (double bt)    {batch_time = bt;}
    double get_batch_time ()    {return batch_time;}

    void set_task_threshold (float th)    {task_threshold = th;}
    float get_task_threshold ()    {return task_threshold;}

    void set_mode (int md)    {mode = md;}
    float get_mode ()    {return mode;}

    std::unordered_set<RAM*> list_of_runnable_tasks(std::unordered_set<RAM*> tasks, std::unordered_map<RAM*, std::unordered_set<RAM*>> taskgraph1);

    void update_task_graph(std::unordered_set<RAM*> executable_tasks);
    void add_scc_dependance (RAM* src_task, RAM* destination_task);

    bool execute();
};

#endif
