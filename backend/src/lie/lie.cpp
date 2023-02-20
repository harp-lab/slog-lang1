/*
 * Logical Inferencing Engine (LIE)
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#include "../parallel_RA_inc.h"
#include <algorithm>
#include <cstddef>
#include <filesystem>
#include <iostream>
#include <ostream>
#include <tuple>
#include <utility>
#include <mpi.h>
//#include <experimental/filesystem>


void LIE::add_relation(relation* rel)
{
    lie_relations.push_back(rel);
    //lie_relations[lie_relation_count] = rel;
    lie_relation_count++;
}

void LIE::add_scc(RAM* ra)
{
    //lie_sccs[lie_sccs_count] = ra;
    lie_sccs.push_back(ra);
    lie_sccs_count++;
}


/// This function currentl only returns one runnable task
/// size of returned list is always going to be 1
RAM* LIE::one_runnable_tasks()
{
    u32 counter = 0;
    for (u32 i=0; i < lie_sccs_count; i++)
    {
        if (lie_sccs[i] == NULL)
        {
            counter++;
            continue;
        }
        if (counter == lie_sccs_count)
            return NULL;

        bool break_loop = false;
        for (auto it = taskgraph.begin(); it != taskgraph.end(); it++)
        {
            std::set<RAM*> it2 = it->second;
            for (auto dit2 = it2.begin(); dit2 != it2.end(); dit2++)
            {
                if (lie_sccs[i] == *dit2)
                {
                    break_loop=true;
                    break;
                }
            }
            if (break_loop==true)
                break;
        }
        if (break_loop==false)
            return lie_sccs[i];
    }

    return NULL;
}


void LIE::update_task_graph(RAM* executable_task)
{
    for (u32 i=0; i < lie_sccs.size(); i++)
    {
        if (lie_sccs[i] == executable_task)
        {
            taskgraph.erase(lie_sccs[i]);
            // check if relation in this scc need gc
            auto gc_rels = executable_task->get_gc_relation();
            for (int j=0; j < gc_rels.size(); j++) {
                auto pos = std::find(lie_relations.begin(), lie_relations.end(), gc_rels[j]);
                if (pos != lie_relations.end()) {
                    lie_relations.erase(pos);
                }
                // before finalize, print rel size and dump to disk first
                write_final_checkpoint_dump(gc_rels[j]);
                std::cout << "relation : " << gc_rels[j]->get_intern_tag() << " GCed" << std::endl;
                gc_rels[j]->finalize_relation();
                delete gc_rels[j];
                gc_rels[j] = NULL;
            }
            delete lie_sccs[i];
            lie_sccs[i] = NULL;
        }
    }
}



void LIE::add_scc_dependance (RAM* src_task, RAM* destination_task)
{
    auto it = taskgraph.find(src_task);
    if( it != taskgraph.end() )
    {
        auto it2 = (it->second).find(destination_task);
        if( it2 == (it->second).end() )
        {
            (it->second).insert(destination_task);
            taskgraph[src_task] = it->second;
        }
    }
    else
    {
        std::set<RAM*> k;
        k.insert(destination_task);
        taskgraph.insert(std::make_pair(src_task, k));
    }

    std::set<RAM*> temp = taskgraph[src_task];
    temp.insert(destination_task);

    return;
}



void LIE::print_all_relation_size()
{
    u64 total_facts=0;
    u64 local_facts[lie_relations.size()];
    for (u32 i = 0 ; i < lie_relations.size(); i++)
    {
        relation* curr_relation = lie_relations[i];
        local_facts[i] = curr_relation->get_full_element_count();
        //local_facts[i] = local_facts[i] + curr_relation->get_delta_element_count();
    }

    u64 global_total_facts[lie_relations.size()];
    MPI_Allreduce(local_facts, global_total_facts, lie_relations.size(), MPI_UNSIGNED_LONG_LONG, MPI_SUM, mcomm.get_local_comm());

    for (u32 i = 0 ; i < lie_relations.size(); i++)
    {
        relation* curr_relation = lie_relations[i];
        if (mcomm.get_local_rank() == 0)
            std::cout << curr_relation->get_debug_id() << ": {" << curr_relation->get_arity() << "}. (" << global_total_facts[i] << " total facts)" << std::endl;
        total_facts = total_facts + global_total_facts[i];
        rel_size_map[curr_relation->get_intern_tag()] = std::make_tuple(global_total_facts[i], curr_relation->get_arity(), curr_relation->is_intermediate_relation());
    }
    // if (mcomm.get_local_rank() == 0)
    //     std::cout << "Total facts across all relations " << total_facts << std::endl << std::endl;
}

void LIE::stat_intermediate()
{
    u64 intermediate_tuple_bytes = 0;
    u64 actual_tuple_bytes = 0;
    for (auto & tp : rel_size_map) {
        bool is_interm = std::get<2>(tp.second);
        u64 tuple_count = std::get<0>(tp.second);
        int arity = std::get<1>(tp.second);
        if (is_interm) {
            intermediate_tuple_bytes += tuple_count * arity * 8;
        } else {
            actual_tuple_bytes += tuple_count * arity * 8;
        }
    }
    if (mcomm.get_local_rank() == 0) {
        std::cout << "Total actual facts: " <<  actual_tuple_bytes / (1024*1024) << " MB." << std::endl;
        std::cout << "Total intermediate facts: " <<  intermediate_tuple_bytes / (1024*1024) << " MB." << std::endl;
        std::cout << "Intermediate overhead ratio: " << intermediate_tuple_bytes * 1.0 / (actual_tuple_bytes + intermediate_tuple_bytes) << std::endl;
    }
}

void LIE::print_relation_size(relation* rel) {
    u64 local_count = rel->get_full_element_count();
    u64 global_count = 0;
    MPI_Reduce(&local_count, &global_count, 1, MPI_UNSIGNED_LONG_LONG, MPI_SUM, 0, mcomm.get_local_comm());
    if (mcomm.get_local_rank() == 0) {
        std::cout << "Tag: " << rel->get_intern_tag() << " " << rel->get_debug_id() << ": {" << rel->get_arity() << "}. (" << global_count << " total facts)" << std::endl;
    }
    rel_size_map[rel->get_intern_tag()] = std::make_tuple(global_count, rel->get_arity(), rel->is_intermediate_relation());
}


void LIE::write_checkpoint_dump(int loop_counter, std::vector<int> executed_scc_id, int scc_id)
{
    std::string dir_name;
    dir_name = output_dir + "/checkpoint-" + std::to_string(scc_id) + "-" + std::to_string(loop_counter);
    for (u32 i = 0 ; i < lie_relations.size(); i++)
    {
        if (lie_relations[i]->get_is_canonical())
            lie_relations[i]->parallel_IO(dir_name);
    }
}


void LIE::write_final_checkpoint_dump()
{
    std::string dir_name;
    dir_name = output_dir + "/checkpoint-final";
    if (mcomm.get_local_rank() == 0)
        std::filesystem::create_directories(dir_name.c_str());
    MPI_Barrier(mcomm.get_local_comm());
    for (u32 i = 0 ; i < lie_relations.size(); i++)
    {
        if (lie_relations[i]->get_is_canonical())
            lie_relations[i]->parallel_IO(dir_name);
    }
}

void LIE::write_final_checkpoint_dump(relation* rel) {
    std::string dir_name;
    dir_name = output_dir + "/checkpoint-final";
    std::filesystem::path dir_path(dir_name);
    if (mcomm.get_local_rank() == 0 && (!std::filesystem::exists(dir_path))) {
        std::filesystem::create_directories(dir_path);
    }
    MPI_Barrier(mcomm.get_local_comm());
    if (rel->get_is_canonical())
        rel->parallel_IO(dir_name);
}



bool LIE::execute ()
{
    /// Main : Execute : init : start
    mcomm.set_local_comm(MPI_COMM_WORLD);

    /// Initialize all relations
    for (u32 i = 0 ; i < lie_relations.size(); i++)
    {
        lie_relations[i]->set_share_io(share_io);
        lie_relations[i]->initialize_relation(mcomm, intern_map);
    }

    /// Executable task
    RAM* executable_task = one_runnable_tasks();

    /// Running one task at a time
    while (executable_task != NULL)
    {
        int loop_counter = 0;
        executable_task->set_comm(mcomm);

        /// Initialize all relations
        std::vector<relation*> scc_relation = executable_task->get_RAM_relations();
        std::vector<bool> scc_relation_status = executable_task->get_RAM_relations_status();;
        u32 scc_relation_count = executable_task->get_ram_relation_count();

        for (u32 i=0; i < scc_relation_count; i++)
        {
            if (scc_relation_status[i] == true)
                scc_relation[i]->insert_full_in_delta();
        }
        

        std::vector<u32> history;

        /// if case is for rules (acopy and copy) that requires only one iteration
        /// else case is for join rules
        /// For SCCs that runs for only one iteration
        if (executable_task->get_iteration_count() == 1)
        {
            executable_task->fixed_point_loop(app_name, batch_size, history, intern_map);
            loop_counter++;
        }
        /// For SCCs that runs till fixed point is reached
        else
        {
            u64 delta_in_scc = 0;
            do
            {
                executable_task->fixed_point_loop(app_name, batch_size, history, intern_map);
                delta_in_scc = history[history.size()-2];
            }
            while (delta_in_scc != 0);
        }

        if (mcomm.get_rank() == 0)
            std::cout << "<<<<<<<<<<< SCC " << executable_task->get_id() << " finish, " << loop_counter << " iteration in total." << std::endl;


        executable_task->insert_delta_in_full();

        /// marks executable_task as finished
        update_task_graph(executable_task);

        /// loads new runnable task
        executable_task = one_runnable_tasks();
    }

    write_final_checkpoint_dump();
    return true;
}


LIE::~LIE ()
{
    for (u32 i = 0 ; i < lie_relations.size(); i++)
    {
        lie_relations[i]->finalize_relation();
        delete (lie_relations[i]);
    }
}
