#include "../parallel_RA_inc.h"

static void initialize_relations(std::unordered_set<relation*>& lie_relations, mpi_comm& mcomm);

void LIE_mt::update_task_graph(std::unordered_set<RAM*> executable_tasks)
{
    for ( auto itg = executable_tasks.begin(); itg != executable_tasks.end(); ++itg )
    {
        tasks.erase(*itg);
        taskgraph1.erase(*itg);
    }
}



std::unordered_set<RAM*> LIE_mt::list_of_runnable_tasks(std::unordered_set<RAM*> tasks, std::unordered_map<RAM*, std::unordered_set<RAM*>> taskgraph1)
{
    std::unordered_set<RAM*> runnable_tasks;
    for ( auto itg = tasks.begin(); itg != tasks.end(); ++itg )
    {
        RAM* task = (*itg);
        bool break_loop = false;

        for (auto it = taskgraph1.begin(); it != taskgraph1.end(); it++)
        {
            std::unordered_set<RAM*> it2 = it->second;
            for (auto dit2 = it2.begin(); dit2 != it2.end(); dit2++)
            {
                if (*itg == *dit2)
                {
                    break_loop=true;
                    break;
                }
            }
            if (break_loop==true)
                break;
        }
        if (break_loop==false)
            runnable_tasks.insert(task);

        if (runnable_tasks.size() == 1)
            break;
    }
    return runnable_tasks;
}



static void initialize_relations(std::unordered_set<relation*>& lie_relations, mpi_comm& mcomm)
{
    for (std::unordered_set<relation*>::iterator it = lie_relations.begin() ; it != lie_relations.end(); ++it)
        (*it)->initialize_relation(mcomm);
}



void rebalance_data(RAM*& send_task, RAM*& recv_task, int rank_offset, mpi_comm new_mcomm, int tuples_per_task, int ib, int ob)
{
    //std::unordered_set<relation*> send_rm = send_task->get_relation_manager();
    //std::unordered_set<relation*> recv_rm = recv_task->get_relation_manager();

    //for (auto [it1, it2] = std::tuple{send_rm.begin(), recv_rm.begin()}; it1 != send_rem.end(); ++it1, ++it2)
    //{
    //}

    //for (std::unordered_set<relation*>::iterator it = rm.begin() ; it != rm.end(); ++it)

    //for (u32 r = 0; r < send_rm.size(); r++)
    //{
    //    relation* send_rel =  send_rm[r];
    //    relation* recv_rel =  recv_rm[r];
    //    send_rel->copy_relation(recv_rel, new_mcomm, rank_offset, tuples_per_task, ib, ob);
    //}
}



bool rebalance_comm(std::vector<u64>& history, int number_of_parallel_tasks, u64*& current_tuple_count_per_task, int*& current_ranks_per_task, int*& current_cumulative_ranks_per_task, mpi_comm& new_mcomm, mpi_comm& mcomm, int *current_color, int *next_color, int* finished_task_count, int mode, float task_threshold)
{
    // Check if balancing is required
    u64 *next_tuple_count_per_task_local = new u64[number_of_parallel_tasks];
    u64 *next_tuple_count_per_task = new u64[number_of_parallel_tasks];
    memset(next_tuple_count_per_task_local, 0, number_of_parallel_tasks * sizeof(u64));
    memset(next_tuple_count_per_task, 0, number_of_parallel_tasks * sizeof(u64));

    // D F D F D F D F D F
    // 0 1 2 3 4 5 6 7 8 9
    u64 full_in_scc = 0;
    u64 delta_in_scc = 0;

    if (mode == 1)
    {
        full_in_scc = history[history.size()-1];
        delta_in_scc = history[history.size()-2];
        if (delta_in_scc == 0)  full_in_scc = 0;

        next_tuple_count_per_task_local[*current_color] = full_in_scc;
        MPI_Allreduce(next_tuple_count_per_task_local, next_tuple_count_per_task, number_of_parallel_tasks, MPI_UNSIGNED_LONG_LONG, MPI_BOR, mcomm.get_comm());

        (*finished_task_count) = 0;
        for (int j=0; j < number_of_parallel_tasks; j++)
        {
            if (next_tuple_count_per_task[j] == 0)
                (*finished_task_count)++;
        }

        delete[] next_tuple_count_per_task_local;
        delete[] next_tuple_count_per_task;
        return false;
    }
    else if (mode == 2)
    {
        full_in_scc = history[history.size()-1];
        delta_in_scc = history[history.size()-2];
        if (delta_in_scc == 0)  full_in_scc = 0;

        next_tuple_count_per_task_local[*current_color] = full_in_scc;
        MPI_Allreduce(next_tuple_count_per_task_local, next_tuple_count_per_task, number_of_parallel_tasks, MPI_UNSIGNED_LONG_LONG, MPI_BOR, mcomm.get_comm());

        int prev_finished_task_count = 0;
        for (int j=0; j < number_of_parallel_tasks; j++)
        {
            if (current_tuple_count_per_task[j] == 0)
                prev_finished_task_count++;
        }

        (*finished_task_count) = 0;
        for (int j=0; j < number_of_parallel_tasks; j++)
        {
            if (next_tuple_count_per_task[j] == 0)
                (*finished_task_count)++;
        }

        if (prev_finished_task_count == *finished_task_count)
        {
            delete[] next_tuple_count_per_task_local;
            delete[] next_tuple_count_per_task;
            return false;
        }
    }
    else if (mode == 3)
    {
        full_in_scc = history[history.size()-1];
        delta_in_scc = history[history.size()-2];
        if (delta_in_scc == 0)  full_in_scc = 0;

        next_tuple_count_per_task_local[*current_color] = full_in_scc;
        MPI_Allreduce(next_tuple_count_per_task_local, next_tuple_count_per_task, number_of_parallel_tasks, MPI_UNSIGNED_LONG_LONG, MPI_BOR, mcomm.get_comm());

        if (mcomm.get_rank() == 0)
        {
            for (int j=0; j < number_of_parallel_tasks; j++)
                std::cout << next_tuple_count_per_task[j] << " ";
            std::cout << std::endl;
        }
    }
    else
    {
        full_in_scc = history[history.size()-1];
        delta_in_scc = history[history.size()-2];
        if (delta_in_scc == 0)  full_in_scc = 0;

        next_tuple_count_per_task_local[*current_color] = full_in_scc;
        MPI_Allreduce(next_tuple_count_per_task_local, next_tuple_count_per_task, number_of_parallel_tasks, MPI_UNSIGNED_LONG_LONG, MPI_BOR, mcomm.get_comm());


        // x = c + (c-p)/2
        for (int j=0; j < number_of_parallel_tasks; j++)
        {
            if ((3*next_tuple_count_per_task[j]) <= current_tuple_count_per_task[j])
                next_tuple_count_per_task[j] = 0;
            else
                next_tuple_count_per_task[j] = ((3*next_tuple_count_per_task[j])-current_tuple_count_per_task[j])/2;
        }
    }
    delete[] next_tuple_count_per_task_local;


    u64 total_tuple_count = 0;

    (*finished_task_count) = 0;
    for (int j=0; j < number_of_parallel_tasks; j++)
    {
        total_tuple_count = total_tuple_count + next_tuple_count_per_task[j];
        if (next_tuple_count_per_task[j] == 0)
            (*finished_task_count)++;
    }

    int* next_cumulative_ranks_per_task = new int[number_of_parallel_tasks + 1];
    memset(next_cumulative_ranks_per_task, 0, (number_of_parallel_tasks + 1) * sizeof(int));

    int* next_ranks_per_task = new int[number_of_parallel_tasks];

#if 1
    u32 temp_rank_count1 = 0;
    u32 temp_rank_count2 = 0;
    for (int j=0; j < number_of_parallel_tasks; j++)
        temp_rank_count1 = temp_rank_count1 + ceil(mcomm.get_nprocs() * (float)((float)next_tuple_count_per_task[j]/total_tuple_count));
    u32 deficiet = temp_rank_count1 - mcomm.get_nprocs();

    next_cumulative_ranks_per_task[0] = 0;
    for (int j=0; j < number_of_parallel_tasks; j++)
    {
        if (next_tuple_count_per_task[j] == 0)
        {
            next_ranks_per_task[j] = 0;
            next_cumulative_ranks_per_task[j+1] = (next_cumulative_ranks_per_task[j]);
        }
        else if (deficiet != 0)
        {
            int temp = ceil(mcomm.get_nprocs() * (float)((float)next_tuple_count_per_task[j]/total_tuple_count));
            if (temp == 1)
            {
                next_ranks_per_task[j] = (ceil(mcomm.get_nprocs() * (float)((float)next_tuple_count_per_task[j]/total_tuple_count)));
                next_cumulative_ranks_per_task[j+1] = (next_cumulative_ranks_per_task[j] + next_ranks_per_task[j]);
                temp_rank_count2 = temp_rank_count2 + next_ranks_per_task[j];
            }
            else
            {
                next_ranks_per_task[j] = (floor(mcomm.get_nprocs() * (float)((float)next_tuple_count_per_task[j]/total_tuple_count)));
                next_cumulative_ranks_per_task[j+1] = (next_cumulative_ranks_per_task[j] + next_ranks_per_task[j]);
                temp_rank_count2 = temp_rank_count2 + next_ranks_per_task[j];
                deficiet--;
            }
        }
        else
        {
            next_ranks_per_task[j] = (ceil(mcomm.get_nprocs() * (float)((float)next_tuple_count_per_task[j]/total_tuple_count)));
            next_cumulative_ranks_per_task[j+1] = (next_cumulative_ranks_per_task[j] + next_ranks_per_task[j]);
            temp_rank_count2 = temp_rank_count2 + next_ranks_per_task[j];
        }
    }
#else
    next_cumulative_ranks_per_task[0] = 0;
    for (int j=0; j < number_of_parallel_tasks; j++)
    {
        next_ranks_per_task[j] = (ceil(mcomm.get_nprocs() * (float)((float)next_tuple_count_per_task[j]/total_tuple_count)));
        next_cumulative_ranks_per_task[j+1] = (next_cumulative_ranks_per_task[j] + next_ranks_per_task[j]);

        if (next_cumulative_ranks_per_task[j+1] > mcomm.get_nprocs())
        {
            next_ranks_per_task[j] = mcomm.get_nprocs() - next_cumulative_ranks_per_task[j];
            next_cumulative_ranks_per_task[j+1] = next_cumulative_ranks_per_task[j] + next_ranks_per_task[j];

            if (j != number_of_parallel_tasks-1) std::cout << "J is " << j << std::endl;
            assert(j == number_of_parallel_tasks-1);
        }
    }
#endif

    int count=0;
    for (int j=0; j < number_of_parallel_tasks; j++)
        if (current_ranks_per_task[j] == next_ranks_per_task[j])
            count++;

    float max = -10.0f;
    int num, den;
    for (int i=0; i < number_of_parallel_tasks; i++)
    {
        if (next_ranks_per_task[i] == 0 || current_ranks_per_task[i] == 0)
            continue;

        if (max < (float)((float)current_ranks_per_task[i]/(float)next_ranks_per_task[i]))
        {
            max = (float)((float)current_ranks_per_task[i]/(float)next_ranks_per_task[i]);
            num = current_ranks_per_task[i];
            den = next_ranks_per_task[i];
        }

        if (max < (float)((float)next_ranks_per_task[i]/(float)current_ranks_per_task[i]))
        {
            max = (float)((float)next_ranks_per_task[i]/(float)current_ranks_per_task[i]);
            den = current_ranks_per_task[i];
            num = next_ranks_per_task[i];
        }
    }

    if (total_tuple_count == 0)
    {
        if (mcomm.get_rank() == 0)
            std::cout << "-------------------All Tasks finished-------------------" << std::endl;

        delete[] next_ranks_per_task;
        delete[] next_cumulative_ranks_per_task;
        return false;
    }

    if (count == number_of_parallel_tasks)
    {
        if (mcomm.get_rank() == 0)
        {
            std::cout << "-------------------No change in rank distribution-------------------" << std::endl;
            for (int j=0; j < number_of_parallel_tasks; j++)
                std::cout << "[" << current_ranks_per_task[j] << " " << next_ranks_per_task[j] << "] ";
            std::cout << std::endl;
        }

        delete[] next_ranks_per_task;
        delete[] next_cumulative_ranks_per_task;
        return false;
    }

    if (max < task_threshold)
    {
        if (mcomm.get_rank() == 0)
        {
            std::cout << "Does not cross threshold of 1.1 current threshold is = " << max << " " << num << " " << den << std::endl;
            for (int j=0; j < number_of_parallel_tasks; j++)
                std::cout << "[" << current_ranks_per_task[j] << " " << next_ranks_per_task[j] << "] ";
            std::cout << std::endl;
        }
        delete[] next_cumulative_ranks_per_task;
        return false;
    }
    else
    {
        if (mcomm.get_rank() == 0){
            std::cout << " -----------------Crosses the threshold of 1.1 current threshold is = " << max << " " << num << " " << den << "-----------------" << std::endl;
            for (int j=0; j < number_of_parallel_tasks; j++)
                std::cout << "[" << current_ranks_per_task[j] << " " << next_ranks_per_task[j] << "] ";
            std::cout << std::endl;
        }
    }

    *next_color = -1;
    for (int j=0; j < number_of_parallel_tasks; j++)
    {
        if (mcomm.get_rank() < next_cumulative_ranks_per_task[j+1])
        {
            *next_color = j;
            break;
        }
    }

    MPI_Comm newcomm;
    MPI_Comm_split(mcomm.get_comm(), *next_color, mcomm.get_rank(), &newcomm);
    new_mcomm.set_local_comm(newcomm);

    for (int j=0; j < number_of_parallel_tasks; j++)
    {
        current_ranks_per_task[j] = next_ranks_per_task[j];
        current_tuple_count_per_task[j] = next_tuple_count_per_task[j];
        current_cumulative_ranks_per_task[j] = next_cumulative_ranks_per_task[j];
    }
    current_cumulative_ranks_per_task[number_of_parallel_tasks] = next_cumulative_ranks_per_task[number_of_parallel_tasks];

    delete[] next_ranks_per_task;
    delete[] next_cumulative_ranks_per_task;
    delete[] next_tuple_count_per_task;

    return true;
}



bool LIE_mt::execute ()
{
    std::unordered_set<RAM*> executable_tasks = list_of_runnable_tasks(tasks, taskgraph1);
    std::cout << "Number of executable tasks: " << executable_tasks.size() << std::endl;

    mcomm.set_local_comm(MPI_COMM_WORLD);
    initialize_relations(lie_relations, mcomm);
    while (executable_tasks.size() != 0)
    {
        RAM** task_list = new RAM*[executable_tasks.size()];
        for(u32 i=0; i < executable_tasks.size(); i++)
            task_list[i] = new RAM();

        u32 index=0;
        for ( auto itg = executable_tasks.begin(); itg != executable_tasks.end(); ++itg )
        {
            std::cout << "RAM NAME: " << (*itg)->get_name() << std::endl;
            task_list[index] = (*itg);
            index++;
        }

        int color = -1;
        int number_of_parallel_tasks = executable_tasks.size();
        u64 *tuple_count_per_task = new u64[number_of_parallel_tasks];
        int *ranks_per_task = new int[number_of_parallel_tasks];
        int *cumulative_ranks_per_task = new int[number_of_parallel_tasks + 1];
        memset(cumulative_ranks_per_task, 0, (number_of_parallel_tasks + 1) * sizeof(int));

        cumulative_ranks_per_task[0] = 0;
        for (int j=0; j < number_of_parallel_tasks; j++)
        {
            tuple_count_per_task[j] = 0;
            ranks_per_task[j] = mcomm.get_nprocs()/number_of_parallel_tasks;
            cumulative_ranks_per_task[j+1] = (cumulative_ranks_per_task[j]) + ranks_per_task[j];

            //std::cout << "Ranks per task " << j << " " << ranks_per_task[j] << std::endl;
        }

        MPI_Comm newcomm;
        for (int j=0; j < number_of_parallel_tasks; j++)
        {
            if (mcomm.get_rank() < cumulative_ranks_per_task[j+1])
            {
                color = j;
                break;
            }
        }

        //std::cout << "Rank " << mcomm.get_rank() << " Color " << color << std::endl;
        MPI_Comm_split(mcomm.get_comm(), color, mcomm.get_rank(), &newcomm);
        mcomm.set_local_comm(newcomm);

        for (int j=0; j < number_of_parallel_tasks; j++)
            task_list[color]->set_comm(mcomm);

        std::vector<u32> history;
        if (task_list[color]->get_iteration_count() == 1)
            task_list[color]->execute_in_batches(batch_size, history, intern_map);
        else
        {
            u64 delta_in_scc = 0;
            do
            {
                task_list[color]->execute_in_batches(batch_size, history, intern_map);
                delta_in_scc = history[history.size()-2];
                //std::cout << "History " << delta_in_scc << std::endl;
            }
            while (delta_in_scc != 0);
        }


#if 0
        u32 loop_count=0;
        int finished_task_count =0;
        mpi_comm new_mcomm(mcomm);
        while (finished_task_count != number_of_parallel_tasks)
        {
            task_list[color]->execute(batch_size, history, color);

            int new_color = -1;
            int* prev_ranks_per_task = new int[number_of_parallel_tasks];
            memcpy(prev_ranks_per_task, ranks_per_task, number_of_parallel_tasks * sizeof(int));

            bool check_for_rebalance = rebalance_comm(history, number_of_parallel_tasks, tuple_count_per_task, ranks_per_task, cumulative_ranks_per_task, new_mcomm, mcomm, &color, &new_color, &finished_task_count, mode, task_threshold);

            if (check_for_rebalance == true)
            {
                rebalance_data(task_list[color], task_list[new_color], cumulative_ranks_per_task[color], new_mcomm, tuple_count_per_task[color], prev_ranks_per_task[color], ranks_per_task[color]);
                task_list[new_color]->set_comm(new_mcomm);
                color = new_color;
            }
            delete[] prev_ranks_per_task;

            loop_count++;
        }
#endif
        update_task_graph(executable_tasks);
        executable_tasks = list_of_runnable_tasks(tasks, taskgraph1);

    }

    return true;
}



void LIE_mt::add_scc_dependance (RAM* src_task, RAM* destination_task)
{
    auto it = taskgraph1.find(src_task);
    if( it != taskgraph1.end() )
    {
        auto it2 = (it->second).find(destination_task);
        if( it2 == (it->second).end() )
        {
            (it->second).insert(destination_task);
            taskgraph1[src_task] = it->second;
        }
    }
    else
    {
        std::unordered_set<RAM*> k;
        k.insert(destination_task);
        taskgraph1.insert(std::make_pair(src_task, k));
    }

    std::unordered_set<RAM*> temp = taskgraph1[src_task];
    temp.insert(destination_task);

    return;
}
