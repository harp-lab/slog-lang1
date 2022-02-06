/*
 * Logical Inferencing Engine (LIE)
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#include "../parallel_RA_inc.h"
#include <algorithm>
#include <cstddef>
#include <filesystem>
#include <iostream>
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
                print_relation_size(gc_rels[j]);
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


#if 0
void LIE::print_all_relation()
{
    u64 total_facts=0;
    for (u32 i = 0 ; i < lie_relation_count; i++)
    {
        relation* curr_relation = lie_relations[i];
        u64 local_facts = curr_relation->get_full_element_count();
        u64 global_total_facts = 0;
        MPI_Allreduce(&local_facts, &global_total_facts, 1, MPI_UNSIGNED_LONG_LONG, MPI_SUM, mcomm.get_local_comm());

        if (mcomm.get_local_rank() == 0)
            std::cout << curr_relation->get_debug_id() << ": {" << curr_relation->get_arity() << "}. (" << global_total_facts << " total facts)" << std::endl;

        total_facts = total_facts + global_total_facts;
    }
    if (mcomm.get_local_rank() == 0)
        std::cout << "Total facts across all relations " << total_facts << std::endl;
}
#endif



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
#if 1
    for (u32 i = 0 ; i < lie_relations.size(); i++)
    {
        relation* curr_relation = lie_relations[i];
        if (mcomm.get_local_rank() == 0)
            std::cout << curr_relation->get_debug_id() << ": {" << curr_relation->get_arity() << "}. (" << global_total_facts[i] << " total facts)" << std::endl;
        total_facts = total_facts + global_total_facts[i];
        rel_size_map[curr_relation->get_intern_tag()] = std::make_tuple(global_total_facts[i], curr_relation->get_arity(), curr_relation->is_intermediate_relation());
    }
#endif
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
        std::cout << rel->get_debug_id() << ": {" << rel->get_arity() << "}. (" << global_count << " total facts)" << std::endl;
    }
    rel_size_map[rel->get_intern_tag()] = std::make_tuple(global_count, rel->get_arity(), rel->is_intermediate_relation());
}


void LIE::write_checkpoint_dump(int loop_counter, std::vector<int> executed_scc_id, int scc_id)
{
#if 0
    std::string dir_name;
    dir_name = output_dir + "/checkpoint-" + std::to_string(scc_id) + "-" + std::to_string(loop_counter);

    std::string scc_metadata;
    scc_metadata = dir_name + "/scc_metadata";
    if (mcomm.get_local_rank() == 0)
    {
        std::cout << "XXXXXXXXXXXXx " << dir_name << " " << (int)executed_scc_id.size() << " " << scc_metadata.c_str() << " " << executed_scc_id[0] << std::endl;
        //mkdir(dir_name.c_str(), S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);
        FILE *fp;
        fp = fopen(scc_metadata.c_str(), "w");
        for (int i = 0; i < (int)executed_scc_id.size(); i++)
            fprintf (fp, "%d\n", executed_scc_id[i]);
        fclose(fp);
    }
    MPI_Barrier(mcomm.get_local_comm());
#endif

    std::string dir_name;
    dir_name = output_dir + "/checkpoint-" + std::to_string(scc_id) + "-" + std::to_string(loop_counter);
    //std::cout << " output_dir name "<< dir_name << std::endl;
    for (u32 i = 0 ; i < lie_relations.size(); i++)
    {
        //std::cout << "Name " << lie_relations[i]->get_filename() << std::endl;
        //if(lie_relations[i]->get_debug_id() == "rel_path_2_1_2")
        if (lie_relations[i]->get_is_canonical() && lie_relations[i]->get_arity() != 0)
            lie_relations[i]->parallel_IO(dir_name);
    }
}


void LIE::write_final_checkpoint_dump()
{
#if 1
    std::string dir_name;
    dir_name = output_dir + "/checkpoint-final";
    if (mcomm.get_local_rank() == 0)
        std::filesystem::create_directories(dir_name.c_str());
    MPI_Barrier(mcomm.get_local_comm());
    for (u32 i = 0 ; i < lie_relations.size(); i++)
    {
        if (lie_relations[i]->get_is_canonical() && lie_relations[i]->get_arity() != 0)
            lie_relations[i]->parallel_IO(dir_name);
    }
#endif
}

void LIE::write_final_checkpoint_dump(relation* rel) {
    std::string dir_name;
    dir_name = output_dir + "/checkpoint-final";
    std::filesystem::path dir_path(dir_name);
    if (mcomm.get_local_rank() == 0 && (!std::filesystem::exists(dir_path))) {
        std::filesystem::create_directories(dir_path);
    }
    MPI_Barrier(mcomm.get_local_comm());
    if (rel->get_is_canonical() && rel->get_arity() != 0)
        rel->parallel_IO(dir_name);
}


void LIE::create_checkpoint_dump(int loop_counter, int scc_id)
{
#if 0
    std::string dir_name;
    dir_name = output_dir + "/checkpoint-" + std::to_string(scc_id) + "-" + std::to_string(loop_counter);

    //std::cout << "Dir name " << dir_name.c_str() << std::endl;
    std::string scc_metadata;
    scc_metadata = dir_name + "/scc_metadata";
    if (mcomm.get_local_rank() == 0)
        std::filesystem::create_directories(dir_name.c_str());
    MPI_Barrier(mcomm.get_local_comm());
#endif
}



bool LIE::execute ()
{
    /// Main : Execute : init : start
    mcomm.set_local_comm(MPI_COMM_WORLD);

#ifdef GOOGLE_MAP
    if (mcomm.get_local_rank() == 0)
        std::cout << "Using Google maps"  << std::endl;
#else
    //if (mcomm.get_local_rank() == 0)
    //    std::cout << "Using Tom's Shmap " << lie_relation_count <<  std::endl;
#endif

    /// Initialize all relations
    for (u32 i = 0 ; i < lie_relations.size(); i++)
    {
        lie_relations[i]->set_restart_flag(restart_flag);
        lie_relations[i]->set_share_io(share_io);
        lie_relations[i]->set_separate_io(separate_io);
        lie_relations[i]->set_offset_io(offset_io);
        lie_relations[i]->initialize_relation(mcomm, intern_map);

#if DEBUG_OUTPUT
        //lie_relations[i]->print();
#endif
    }

    //if (mcomm.get_local_rank() == 0)
    //    std::cout << "Done initializing " << lie_relation_count <<  std::endl;

#if 0
    /// create output directory for checkpoint dumps
    if (enable_io == true && mcomm.get_local_rank() == 0)
    {
        std::cout << "Output folder" << std::endl;
        mkdir(output_dir.c_str(), S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);
    }
#endif

#if DEBUG_OUTPUT
    if (mcomm.get_local_rank() == 0)
    {
        if (restart_flag == true)
        {
            if (separate_io == true)
                std::cout << "Read Local Data without MetaData" << std::endl;
            else if (offset_io == true)
                std::cout << "Read Global Data with Offset MetaData" << std::endl;
            else
                std::cout << "Read Global Data with Size MetaData" << std::endl;
        }
        else
        {
            if (separate_io == true)
                std::cout << "Write Local Data without MetaData" << std::endl;
            else if (offset_io == true)
                std::cout << "Write Global Data with Offset MetaData" << std::endl;
            else
                std::cout << "Write Global Data with Size MetaData" << std::endl;
        }
        std::cout << "----------------- Initialization Complete ---------------------" << std::endl << std::endl;
    }
#endif

    /// Executable task
    RAM* executable_task = one_runnable_tasks();


#if 0
    std::vector<int> executed_scc_id;  /* the sccs that have been executed */
    /* Read scc metadate if it restarts from checkpoint */
    if (restart_flag == true)
    {
        int a;
        std::string scc_metadata;
        scc_metadata = restart_dir_name + "/scc_metadata";
        std::ifstream file(scc_metadata);
        if (file.is_open())
        {
            while (file >> a)
                executed_scc_id.push_back(a);
        }
        file.close();
    }
#endif



    int rank = mcomm.get_local_rank();
    int nprocs = mcomm.get_local_nprocs();
    int* rotate_index_array = new int[nprocs];
    memset(rotate_index_array, 0, nprocs * sizeof(int));

    for (int i = 0; i < nprocs; i++)
        rotate_index_array[i] = (2*rank - i + nprocs) % nprocs;

    int** send_indexes = new int*[nprocs];
    for (int i=0; i < nprocs; i++)
    {
        send_indexes[i] = new int[(nprocs + 1)/2];
        memset(send_indexes[i], 0, ((nprocs + 1)/2) * sizeof(int));
    }

    int* sendb_num  = new int[nprocs];
    memset(sendb_num, 0, nprocs * sizeof(int));

    for (int k = 1; k < nprocs; k <<= 1)
    {
        for (int i = 1; i < nprocs; i++)
        {
            if (i & k)
            {
                send_indexes[k][sendb_num[k]] = (rank+i)%nprocs;
                sendb_num[k] = sendb_num[k]+1;
            }
        }
    }

    //int c = 0;
    /// Running one task at a time
    while (executable_task != NULL)
    {
        //std::cout << "Loop: " << c << std::endl;
        //c++;
        int loop_counter = 0;

#if 1
        /* Skip the scc if it has been executed before */
        int scc_id = executable_task->get_id();
        std::vector<int>::iterator it;
        it = find (executed_scc_id.begin(), executed_scc_id.end(), scc_id);
        if (it != executed_scc_id.end())
        {
            update_task_graph(executable_task);
            executable_task = one_runnable_tasks();
            continue;
        }
#endif
        executable_task->set_comm(mcomm);

        /// Initialize all relations
        //relation** scc_relation = executable_task->get_RAM_relations();
        std::vector<relation*> scc_relation = executable_task->get_RAM_relations();
        //bool* scc_relation_status = executable_task->get_RAM_relations_status();;
        std::vector<bool> scc_relation_status = executable_task->get_RAM_relations_status();;
        u32 scc_relation_count = executable_task->get_ram_relation_count();
        if (restart_flag == false)
        {
            for (u32 i=0; i < scc_relation_count; i++)
            {
                if (scc_relation_status[i] == true)
                    scc_relation[i]->insert_full_in_delta();
            }
        }
        else
        {
            for (u32 i = 0 ; i < scc_relation_count; i++)
            {
                std::string delta_filename;
                delta_filename = restart_dir_name + "/" + scc_relation[i]->get_debug_id().c_str() + "_delta";

                if (separate_io == true)
                    delta_filename = delta_filename + "_" + std::to_string(mcomm.get_local_rank());

                scc_relation[i]->set_filename(delta_filename);
                scc_relation[i]->set_initailization_type(0);

                int is_access = access(delta_filename.c_str(), F_OK);
                int access_sum = 0;
                MPI_Allreduce(&is_access, &access_sum, 1, MPI_INT, MPI_SUM, mcomm.get_local_comm());

                if (access_sum + mcomm.get_local_nprocs() > 0)
                {
                    if (separate_io == true)
                        scc_relation[i]->load_data_from_separate_files();
                    else if (offset_io == true)
                        scc_relation[i]->load_data_from_file_with_offset();
                    else
                    {
                        scc_relation[i]->load_data_from_file();
                    }
                }
            }
        }

        std::vector<u32> history;

#if DEBUG_OUTPUT
        if (mcomm.get_local_rank() == 0)
            std::cout << "-------------------Executing SCC " << executable_task->get_id() << "------------------" << std::endl;
#endif

        /// if case is for rules (acopy and copy) that requires only one iteration
        /// else case is for join rules




        /// For SCCs that runs for only one iteration
        if (executable_task->get_iteration_count() == 1)
        {
            if (enable_data_io == true && loop_counter % cp_iteration == 0)
                create_checkpoint_dump(loop_counter, executable_task->get_id());

            if (comm_compaction == 0)
                executable_task->execute_in_batches(app_name, batch_size, history, intern_map, &loop_counter, executable_task->get_id(), output_dir, all_to_all_meta_data_dump, sloav_mode, rotate_index_array, send_indexes, sendb_num);
            else
                executable_task->execute_in_batches_comm_compaction(app_name, batch_size, history, intern_map, &loop_counter, executable_task->get_id(), output_dir, all_to_all_meta_data_dump, sloav_mode, rotate_index_array, send_indexes, sendb_num);

            //executed_scc_id.push_back(executable_task->get_id());
#if 0
            //int tlc = loop_counter - 1;
            if (enable_data_io == true && (loop_counter - 1) % cp_iteration == 0)
            {
                double writing_checkpoint_dump_time = 0;
                int checkpoint_dumps_num = 0;
                double write_cp_start = MPI_Wtime();
                write_checkpoint_dump(loop_counter - 1, executed_scc_id, executable_task->get_id());
                double write_cp_end = MPI_Wtime();
                writing_checkpoint_dump_time = (write_cp_end - write_cp_start);
                double max_write_cp_time = 0;
                MPI_Reduce(&writing_checkpoint_dump_time, &max_write_cp_time, 1, MPI_DOUBLE, MPI_MAX, 0, mcomm.get_comm());
                //if (mcomm.get_local_rank() == 0)
                //	std::cout << "Writing checkpoint dump " << checkpoint_dumps_num << " takes " << max_write_cp_time << "(s)" << std::endl;
                checkpoint_dumps_num++;
            }
#endif
            //loop_counter++;
            //iteration_count[executable_task->get_id()] = loop_counter;

#if DEBUG_OUTPUT
            //for (u32 i = 0 ; i < scc_relation_count; i++)
            //    scc_relation[i]->print();
            print_all_relation_size();
#endif
            //print_all_relation_size();
        }
        /// For SCCs that runs till fixed point is reached
        else
        {
            //if (mcomm.get_rank() == 0)
            //    std::cout << "name\tnprocs\tmin\tmax\tmean\tIteration#\tBuffer_creation_time\tComputation_time\tAll_to_all_time\tBuffer_free_time\tInsert_in_newt_time\tIntra_comm_time\tInsert_in_full_time\tTotal_time" << std::endl;
            u64 delta_in_scc = 0;
            do
            {
                if (enable_data_io == true && (loop_counter - 1) % cp_iteration == 0)
                    create_checkpoint_dump(loop_counter, executable_task->get_id());

                if (comm_compaction == 0)
                    executable_task->execute_in_batches(app_name, batch_size, history, intern_map, &loop_counter, executable_task->get_id(), output_dir, all_to_all_meta_data_dump, sloav_mode, rotate_index_array, send_indexes, sendb_num);
                else
                    executable_task->execute_in_batches_comm_compaction(app_name, batch_size, history, intern_map, &loop_counter, executable_task->get_id(), output_dir, all_to_all_meta_data_dump, sloav_mode, rotate_index_array, send_indexes, sendb_num);

                //executable_task->print_all_relation();

                delta_in_scc = history[history.size()-2];
                //if (delta_in_scc == 0)
                //    executed_scc_id.push_back(executable_task->get_id());
#if 0
                if (enable_data_io == true && loop_counter % cp_iteration == 0)
                {
                    double writing_checkpoint_dump_time = 0;
                    int checkpoint_dumps_num = 0;
                    double write_cp_start = MPI_Wtime();
                    write_checkpoint_dump(loop_counter - 1, executed_scc_id, executable_task->get_id());
                    double write_cp_end = MPI_Wtime();
                    writing_checkpoint_dump_time = (write_cp_end - write_cp_start);
                    double max_write_cp_time = 0;
                    MPI_Reduce(&writing_checkpoint_dump_time, &max_write_cp_time, 1, MPI_DOUBLE, MPI_MAX, 0, mcomm.get_comm());
                    //if (mcomm.get_local_rank() == 0)
                    //    std::cout << "Writing checkpoint dump " << checkpoint_dumps_num << " takes " << max_write_cp_time << "(s)" << std::endl;
                    checkpoint_dumps_num++;
                }
#endif
                //loop_counter++;
                //iteration_count[executable_task->get_id()] = loop_counter;


#if DEBUG_OUTPUT
                //for (u32 i = 0 ; i < scc_relation_count; i++)
                //    scc_relation[i]->print();
                print_all_relation_size();
#endif
                //print_all_relation_size();
            }
            while (delta_in_scc != 0);
        }

        //set_executed_scc_id(executed_scc_id);
        set_loop_counter(loop_counter);


        executable_task->insert_delta_in_full();

        /// marks executable_task as finished
        update_task_graph(executable_task);

        /// loads new runnable task
        executable_task = one_runnable_tasks();
    }
    // print_all_relation_size();

    write_final_checkpoint_dump();


    delete[] rotate_index_array;
    for (int i=0; i < nprocs; i++)
        delete[] send_indexes[i];
    delete[] send_indexes;
    delete[] sendb_num;






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
