/*
 * Logical Inferencing Engine (LIE)
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#include "../parallel_RA_inc.h"
//#include <experimental/filesystem>


void LIE::add_relation(relation* rel)
{
    lie_relations[lie_relation_count] = rel;
    lie_relation_count++;
}

void LIE::add_scc(RAM* ra)
{
    lie_sccs[lie_sccs_count] = ra;
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
    for (u32 i=0; i < lie_sccs_count; i++)
    {
        if (lie_sccs[i] == executable_task)
        {
            taskgraph.erase(lie_sccs[i]);
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
    u64 local_facts[lie_relation_count];
    for (u32 i = 0 ; i < lie_relation_count; i++)
    {
        relation* curr_relation = lie_relations[i];
        local_facts[i] = curr_relation->get_full_element_count();
        //local_facts[i] = local_facts[i] + curr_relation->get_delta_element_count();
    }

    u64 global_total_facts[lie_relation_count];
    MPI_Allreduce(local_facts, global_total_facts, lie_relation_count, MPI_UNSIGNED_LONG_LONG, MPI_SUM, mcomm.get_local_comm());
#if 1
    for (u32 i = 0 ; i < lie_relation_count; i++)
    {
        relation* curr_relation = lie_relations[i];
        if (mcomm.get_local_rank() == 0)
            std::cout << curr_relation->get_debug_id() << ": {" << curr_relation->get_arity() << "}. (" << global_total_facts[i] << " total facts)" << std::endl;
        total_facts = total_facts + global_total_facts[i];
    }
#endif
    if (mcomm.get_local_rank() == 0)
        std::cout << "Total facts across all relations " << total_facts << std::endl << std::endl;
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
    for (u32 i = 0 ; i < lie_relation_count; i++)
    {
        //std::cout << "Name " << lie_relations[i]->get_filename() << std::endl;
        //if(lie_relations[i]->get_debug_id() == "rel_path_2_1_2")
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
    for (u32 i = 0 ; i < lie_relation_count; i++)
        lie_relations[i]->parallel_IO(dir_name);
#endif
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
    if (mcomm.get_local_rank() == 0)
        std::cout << "Using Tom's Shmap " << lie_relation_count <<  std::endl;
#endif

    /// Initialize all relations
    for (u32 i = 0 ; i < lie_relation_count; i++)
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


    /// create output directory for checkpoint dumps
    if (enable_io == true && mcomm.get_local_rank() == 0)
    {
        std::cout << "Output folder" << std::endl;
        mkdir(output_dir.c_str(), S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);
    }

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

#if 0
    all_to_all_buffer_size = new int**[lie_sccs_count];
    memset(all_to_all_buffer_size, 0, sizeof(int**) * lie_sccs_count);
    for (u32 i =0; i <lie_sccs_count; i++)
    {
        all_to_all_buffer_size[i] = new int*[MAX_LOOP_COUNT];
        memset(all_to_all_buffer_size[i], 0, sizeof(int*) * MAX_LOOP_COUNT);
        for (u32 j =0; j <MAX_LOOP_COUNT; j++)
        {
            all_to_all_buffer_size[i][j] = new int[mcomm.get_local_nprocs()];
            memset(all_to_all_buffer_size[i][j], 0, sizeof(int) * mcomm.get_local_nprocs());
        }
    }
#endif

    compute_size1 = new int*[lie_sccs_count];
    for (u32 i =0; i <lie_sccs_count; i++)
    {
        compute_size1[i] = new int[MAX_LOOP_COUNT];
        memset(compute_size1[i], 0, MAX_LOOP_COUNT * sizeof(u32));
    }

    compute_size2 = new int*[lie_sccs_count];
    for (u32 i =0; i <lie_sccs_count; i++)
    {
        compute_size2[i] = new int[MAX_LOOP_COUNT];
        memset(compute_size2[i], 0, MAX_LOOP_COUNT * sizeof(u32));
    }

    double** running_time = new double*[lie_sccs_count];
    double** running_intra_bucket_comm = new double*[lie_sccs_count];
    double** running_buffer_allocate = new double*[lie_sccs_count];
    double** running_local_compute = new double*[lie_sccs_count];
    double** running_all_to_all = new double*[lie_sccs_count];
    double** running_buffer_free = new double*[lie_sccs_count];
    double** running_insert_newt = new double*[lie_sccs_count];
    double** running_insert_in_full = new double*[lie_sccs_count];
    double** running_fp = new double*[lie_sccs_count];

    double **running_a2a_find_count_time = new double*[lie_sccs_count];
    double **running_a2a_create_rindex_time = new double*[lie_sccs_count];
    double **running_a2a_total_find_blocks_time = new double*[lie_sccs_count];
    double **running_a2a_total_pre_time = new double*[lie_sccs_count];
    double **running_a2a_total_send_meda_time = new double*[lie_sccs_count];
    double **running_a2a_total_comm_time = new double*[lie_sccs_count];
    double **running_a2a_total_replace_time = new double*[lie_sccs_count];
    double **running_a2a_exchange_time = new double*[lie_sccs_count];
    double **running_a2a_filter_time = new double*[lie_sccs_count];

    for (u32 i =0; i <lie_sccs_count; i++)
    {
        running_time[i] = new double[MAX_LOOP_COUNT];
        running_intra_bucket_comm[i] = new double[MAX_LOOP_COUNT];
        running_buffer_allocate[i] = new double[MAX_LOOP_COUNT];
        running_local_compute[i] = new double[MAX_LOOP_COUNT];
        running_all_to_all[i] = new double[MAX_LOOP_COUNT];
        running_buffer_free[i] = new double[MAX_LOOP_COUNT];
        running_insert_newt[i] = new double[MAX_LOOP_COUNT];
        running_insert_in_full[i] = new double[MAX_LOOP_COUNT];
        running_fp[i] = new double[MAX_LOOP_COUNT];

        memset(running_time[i], 0, MAX_LOOP_COUNT * sizeof(double));
        memset(running_intra_bucket_comm[i], 0, MAX_LOOP_COUNT * sizeof(double));
        memset(running_buffer_allocate[i], 0, MAX_LOOP_COUNT * sizeof(double));
        memset(running_local_compute[i], 0, MAX_LOOP_COUNT * sizeof(double));
        memset(running_all_to_all[i], 0, MAX_LOOP_COUNT * sizeof(double));
        memset(running_buffer_free[i], 0, MAX_LOOP_COUNT * sizeof(double));
        memset(running_insert_newt[i], 0, MAX_LOOP_COUNT * sizeof(double));
        memset(running_insert_in_full[i], 0, MAX_LOOP_COUNT * sizeof(double));
        memset(running_fp[i], 0, MAX_LOOP_COUNT * sizeof(double));

        running_a2a_find_count_time[i] = new double[MAX_LOOP_COUNT];
        running_a2a_create_rindex_time[i] = new double[MAX_LOOP_COUNT];
        running_a2a_total_find_blocks_time[i] = new double[MAX_LOOP_COUNT];
        running_a2a_total_pre_time[i] = new double[MAX_LOOP_COUNT];
        running_a2a_total_send_meda_time[i] = new double[MAX_LOOP_COUNT];
        running_a2a_total_comm_time[i] = new double[MAX_LOOP_COUNT];
        running_a2a_total_replace_time[i] = new double[MAX_LOOP_COUNT];
        running_a2a_exchange_time[i] = new double[MAX_LOOP_COUNT];
        running_a2a_filter_time[i] = new double[MAX_LOOP_COUNT];

        memset(running_a2a_find_count_time[i], 0, MAX_LOOP_COUNT * sizeof(double));
        memset(running_a2a_create_rindex_time[i], 0, MAX_LOOP_COUNT * sizeof(double));
        memset(running_a2a_total_find_blocks_time[i], 0, MAX_LOOP_COUNT * sizeof(double));
        memset(running_a2a_total_pre_time[i], 0, MAX_LOOP_COUNT * sizeof(double));
        memset(running_a2a_total_send_meda_time[i], 0, MAX_LOOP_COUNT * sizeof(double));
        memset(running_a2a_total_comm_time[i], 0, MAX_LOOP_COUNT * sizeof(double));
        memset(running_a2a_total_replace_time[i], 0, MAX_LOOP_COUNT * sizeof(double));
        memset(running_a2a_exchange_time[i], 0, MAX_LOOP_COUNT * sizeof(double));
        memset(running_a2a_filter_time[i], 0, MAX_LOOP_COUNT * sizeof(double));
    }

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


    /// Running one task at a time
    while (executable_task != NULL)
    {
        int loop_counter = 0;
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

        executable_task->set_comm(mcomm);

        /// Initialize all relations
        relation** scc_relation = executable_task->get_RAM_relations();
        bool* scc_relation_status = executable_task->get_RAM_relations_status();;
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
                        scc_relation[i]->load_data_from_file();
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
                executable_task->execute_in_batches(app_name, batch_size, history, intern_map, running_time, running_intra_bucket_comm, running_buffer_allocate, running_local_compute, running_all_to_all, running_buffer_free, running_insert_newt, running_insert_in_full, running_fp, running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, &loop_counter, executable_task->get_id(), output_dir, all_to_all_meta_data_dump, /*all_to_all_buffer_size*/NULL, compute_size1, compute_size2, sloav_mode, rotate_index_array, send_indexes, sendb_num);
            else
                executable_task->execute_in_batches_comm_compaction(app_name, batch_size, history, intern_map, running_time, running_intra_bucket_comm, running_buffer_allocate, running_local_compute, running_all_to_all, running_buffer_free, running_insert_newt, running_insert_in_full, running_fp, running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, &loop_counter, executable_task->get_id(), output_dir, all_to_all_meta_data_dump, /*all_to_all_buffer_size*/NULL, compute_size1, compute_size2, sloav_mode, rotate_index_array, send_indexes, sendb_num);
            //executable_task->execute_in_batches_comm_compaction(app_name, batch_size, history, intern_map, running_time, running_intra_bucket_comm, running_buffer_allocate, running_local_compute, running_all_to_all, running_buffer_free, running_insert_newt, running_insert_in_full, running_fp, running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, &loop_counter, executable_task->get_id(), output_dir, all_to_all_meta_data_dump, /*all_to_all_buffer_size*/NULL, compute_size1, compute_size2, sloav_mode, rotate_index_array, send_indexes, sendb_num);

            executed_scc_id.push_back(executable_task->get_id());
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
            iteration_count[executable_task->get_id()] = loop_counter;

#if DEBUG_OUTPUT
            //for (u32 i = 0 ; i < scc_relation_count; i++)
            //    scc_relation[i]->print();
            print_all_relation_size();
#endif
        }
        /// For SCCs that runs till fixed point is reached
        else
        {
            if (mcomm.get_rank() == 0)
                std::cout << "name\tnprocs\tmin\tmax\tmean\tIteration#\tBuffer_creation_time\tComputation_time\tAll_to_all_time\tBuffer_free_time\tInsert_in_newt_time\tIntra_comm_time\tInsert_in_full_time\tTotal_time" << std::endl;
            u64 delta_in_scc = 0;
            do
            {
                if (enable_data_io == true && (loop_counter - 1) % cp_iteration == 0)
                    create_checkpoint_dump(loop_counter, executable_task->get_id());

                if (comm_compaction == 0)
                    executable_task->execute_in_batches(app_name, batch_size, history, intern_map, running_time, running_intra_bucket_comm, running_buffer_allocate, running_local_compute, running_all_to_all, running_buffer_free, running_insert_newt, running_insert_in_full, running_fp, running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, &loop_counter, executable_task->get_id(), output_dir, all_to_all_meta_data_dump, /*all_to_all_buffer_size*/NULL, compute_size1, compute_size2, sloav_mode, rotate_index_array, send_indexes, sendb_num);
                else
                    executable_task->execute_in_batches_comm_compaction(app_name, batch_size, history, intern_map, running_time, running_intra_bucket_comm, running_buffer_allocate, running_local_compute, running_all_to_all, running_buffer_free, running_insert_newt, running_insert_in_full, running_fp, running_a2a_find_count_time, running_a2a_create_rindex_time, running_a2a_total_find_blocks_time, running_a2a_total_pre_time, running_a2a_total_send_meda_time, running_a2a_total_comm_time, running_a2a_total_replace_time, running_a2a_exchange_time, running_a2a_filter_time, &loop_counter, executable_task->get_id(), output_dir, all_to_all_meta_data_dump, /*all_to_all_buffer_size*/NULL, compute_size1, compute_size2, sloav_mode, rotate_index_array, send_indexes, sendb_num);

                //executable_task->print_all_relation();

                delta_in_scc = history[history.size()-2];
                if (delta_in_scc == 0)
                    executed_scc_id.push_back(executable_task->get_id());
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
                iteration_count[executable_task->get_id()] = loop_counter;


#if DEBUG_OUTPUT
                //for (u32 i = 0 ; i < scc_relation_count; i++)
                //    scc_relation[i]->print();
                print_all_relation_size();
#endif
            }
            while (delta_in_scc != 0);
        }

        set_executed_scc_id(executed_scc_id);
        set_loop_counter(loop_counter);


        executable_task->insert_delta_in_full();

        /// marks executable_task as finished
        update_task_graph(executable_task);

        /// loads new runnable task
        executable_task = one_runnable_tasks();
    }

    write_final_checkpoint_dump();

#if 1
    if (all_to_all_meta_data_dump == true)
    {
        double local_total_time = 0;
        double max_total_time = 0;
        for (u32 l=0; l < lie_sccs_count; l++)
            for (u32 il=0; il < iteration_count[l]; il++)
                local_total_time = local_total_time + running_time[l][il];

        MPI_Allreduce(&local_total_time, &max_total_time, 1, MPI_DOUBLE, MPI_MAX, mcomm.get_local_comm());
        if (local_total_time == max_total_time)
        {
            std::string time_file;
            time_file = output_dir + "/MAX_TIME_" + std::to_string(mcomm.get_rank());
            FILE *fpm;
            fpm = fopen(time_file.c_str(), "w");

            fprintf(fpm, "CC\tTaskID\tIteration#\tBuffer_creation_time\tComputation_time\tAll_to_all_time\tBuffer_free_time\tInsert_in_newt_time\tIntra_comm_time\tInsert_in_full_time\tFixed_point_check\tTotal_time\tmode\ta2a_find_count_time\tcreate_rindex_time\ttotal_find_blocks_time\ttotal_pre_time\ttotal_send_meda_time\ttotal_comm_time\ttotal_replace_time\texchange_time\tfilter_time\n");

            for (u32 l=0; l < lie_sccs_count; l++)
            {
                for (u32 il=0; il < iteration_count[l]; il++)
                {
                    fprintf(fpm, "%d\t%d\t%d\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%d\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\n", comm_compaction, l, il,
                            running_buffer_allocate[l][il], running_local_compute[l][il], running_all_to_all[l][il], running_buffer_free[l][il], running_insert_newt[l][il], running_intra_bucket_comm[l][il], running_insert_in_full[l][il], running_fp[l][il], running_time[l][il],
                            sloav_mode,
                            running_a2a_find_count_time[l][il], running_a2a_create_rindex_time[l][il], running_a2a_total_find_blocks_time[l][il], running_a2a_total_pre_time[l][il], running_a2a_total_send_meda_time[l][il], running_a2a_total_comm_time[l][il], running_a2a_total_replace_time[l][il], running_a2a_exchange_time[l][il], running_a2a_filter_time[l][il]);
                }
            }
            fclose(fpm);
        }


        if (mcomm.get_local_rank() == 1)
        {
            std::string time_file;
            time_file = output_dir + "/RANK_1_TIME_" + std::to_string(mcomm.get_rank());
            FILE *fpm;
            fpm = fopen(time_file.c_str(), "w");

            fprintf(fpm, "CC\tTaskID\tIteration#\tBuffer_creation_time\tComputation_time\tAll_to_all_time\tBuffer_free_time\tInsert_in_newt_time\tIntra_comm_time\tInsert_in_full_time\tFixed_point_check\tTotal_time\tmode\ta2a_find_count_time\tcreate_rindex_time\ttotal_find_blocks_time\ttotal_pre_time\ttotal_send_meda_time\ttotal_comm_time\ttotal_replace_time\texchange_time\tfilter_time\n");

            for (u32 l=0; l < lie_sccs_count; l++)
                for (u32 il=0; il < iteration_count[l]; il++)
                    fprintf(fpm, "%d\t%d\t%d\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%d\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\n", comm_compaction , l, il,
                            running_buffer_allocate[l][il], running_local_compute[l][il], running_all_to_all[l][il], running_buffer_free[l][il], running_insert_newt[l][il], running_intra_bucket_comm[l][il], running_insert_in_full[l][il], running_fp[l][il], running_time[l][il],
                            sloav_mode,
                            running_a2a_find_count_time[l][il], running_a2a_create_rindex_time[l][il], running_a2a_total_find_blocks_time[l][il], running_a2a_total_pre_time[l][il], running_a2a_total_send_meda_time[l][il], running_a2a_total_comm_time[l][il], running_a2a_total_replace_time[l][il], running_a2a_exchange_time[l][il], running_a2a_filter_time[l][il]);


            fclose(fpm);
        }

        if (mcomm.get_local_rank() == 0)
        {
            std::string nprocs_file;
            nprocs_file = output_dir + "/metadata";
            FILE *fpn;
            fpn = fopen(nprocs_file.c_str(), "w");
            fprintf(fpn, "(nprocs)\n%d\n", mcomm.get_local_nprocs());
            fprintf(fpn, "(task count)\n%d\n", lie_sccs_count);
            for (u32 l=0; l < lie_sccs_count; l++)
                fprintf(fpn, "(task %d)\n%d\n", l, iteration_count[l]);
            fclose(fpn);
        }

#if 0
        if (app_name == "./TC")
        {
            for (u32 m=0; m <= 2500; m=m+500)
            //for (u32 m=0; m <= 40; m=m+5)
            {
                int *temp_buffer0 = new int[nprocs];
                int min0 = 0, max0= 0, median0 = 0, mean0 = 0;
                memcpy(temp_buffer0, all_to_all_buffer_size[1][m], nprocs * sizeof(int));
                std::sort(temp_buffer0, temp_buffer0+nprocs);
                min0 = temp_buffer0[0];
                max0 = temp_buffer0[nprocs-1];
                median0 = temp_buffer0[nprocs/2];
                int sum0 = 0;
                for (int i = 0; i <nprocs; i++)
                    sum0 = sum0+temp_buffer0[i];
                mean0 = sum0/nprocs;
                float var0 = 0;
                for( int n = 0; n < nprocs; n++ )
                    var0 += (temp_buffer0[n] - mean0) * (temp_buffer0[n] - mean0);
                var0 /= nprocs;
                float sd0 = sqrt(var0);
                delete[] temp_buffer0;

                int *min_buf0 = new int[nprocs];
                int *max_buf0 = new int[nprocs];
                int *mean_buf0 = new int[nprocs];
                int *median_buf0 = new int[nprocs];
                float *sda0 = new float[nprocs];
                memset(min_buf0, 0, nprocs * sizeof(int));
                memset(max_buf0, 0, nprocs * sizeof(int));
                memset(mean_buf0, 0, nprocs * sizeof(int));
                memset(median_buf0, 0, nprocs * sizeof(int));
                memset(sda0, 0, nprocs * sizeof(float));
                MPI_Gather(&min0, 1, MPI_INT, min_buf0, 1,MPI_INT, 0, mcomm.get_local_comm());
                MPI_Gather(&max0, 1, MPI_INT, max_buf0, 1,MPI_INT, 0, mcomm.get_local_comm());
                MPI_Gather(&median0, 1, MPI_INT, median_buf0, 1,MPI_INT, 0, mcomm.get_local_comm());
                MPI_Gather(&mean0, 1, MPI_INT, mean_buf0, 1,MPI_INT, 0, mcomm.get_local_comm());
                MPI_Gather(&sd0, 1, MPI_FLOAT, sda0, 1,MPI_FLOAT, 0, mcomm.get_local_comm());
                if (mcomm.get_local_rank() == 0)
                {
                    std::string a2a_0;
                    a2a_0 = output_dir + "/a2a_"+ std::to_string(m);
                    FILE *fpn0;
                    fpn0 = fopen(a2a_0.c_str(), "w");
                    fprintf (fpn0, "rank\tmin\tmax\tmedian\tmean\tstddev\n");
                    for (int i=0; i < nprocs; i++)
                        fprintf (fpn0, "%d\t%d\t%d\t%d\t%d\t%f\n",i, min_buf0[i], max_buf0[i], median_buf0[i], mean_buf0[i], sda0[i]);
                    fclose(fpn0);
                }
                delete[] min_buf0;
                delete[] max_buf0;
                delete[] mean_buf0;
                delete[] median_buf0;
                delete[] sda0;
            }
        }




        int total_iteration_count = 0;
        for (u32 l=0; l < lie_sccs_count; l++)
            for (u32 il=0; il < iteration_count[l]; il++)
                total_iteration_count++;

        int *all_to_all_global_buffer_all_iteration_size = new int[mcomm.get_local_nprocs() * mcomm.get_local_nprocs() * total_iteration_count];
        memset(all_to_all_global_buffer_all_iteration_size, 0, sizeof(int)*mcomm.get_local_nprocs()*mcomm.get_local_nprocs()*total_iteration_count);

        int counter = 0;
        for (u32 l=0; l < lie_sccs_count; l++)
        {
            for (u32 il=0; il < iteration_count[l]; il++)
            {
                MPI_Gather(all_to_all_buffer_size[l][il], mcomm.get_local_nprocs(), MPI_INT, all_to_all_global_buffer_all_iteration_size + (mcomm.get_local_nprocs() * mcomm.get_local_nprocs() * counter), mcomm.get_local_nprocs(), MPI_INT, 0, mcomm.get_local_comm());
                counter++;
            }
        }


        double *total_time = new double[23 * total_iteration_count];
        double *total_time_global = new double[23 * total_iteration_count * mcomm.get_local_nprocs()];
        counter = 0;
        for (u32 l=0; l < lie_sccs_count; l++)
        {
            for (u32 il=0; il < iteration_count[l]; il++)
            {
                total_time[23 * counter + 0] = mcomm.get_local_rank();
                total_time[23 * counter + 1] = l;
                total_time[23 * counter + 2] = il;
                total_time[23 * counter + 3] = running_buffer_allocate[l][il];
                total_time[23 * counter + 4] = running_local_compute[l][il];
                total_time[23 * counter + 5] = running_all_to_all[l][il];
                total_time[23 * counter + 6] = running_buffer_free[l][il];
                total_time[23 * counter + 7] = running_insert_newt[l][il];
                total_time[23 * counter + 8] = running_intra_bucket_comm[l][il];
                total_time[23 * counter + 9] = running_insert_in_full[l][il];
                total_time[23 * counter + 10] = running_fp[l][il];
                total_time[23 * counter + 11] = running_time[l][il];

                total_time[23 * counter + 12] = sloav_mode;
                total_time[23 * counter + 13] = running_a2a_find_count_time[l][il];
                total_time[23 * counter + 14] = running_a2a_create_rindex_time[l][il];
                total_time[23 * counter + 15] = running_a2a_total_find_blocks_time[l][il];
                total_time[23 * counter + 16] = running_a2a_total_pre_time[l][il];
                total_time[23 * counter + 17] = running_a2a_total_send_meda_time[l][il];
                total_time[23 * counter + 18] = running_a2a_total_replace_time[l][il];
                total_time[23 * counter + 19] = running_a2a_exchange_time[l][il];
                total_time[23 * counter + 20] = running_a2a_filter_time[l][il];

                total_time[23 * counter + 21] = compute_size1[l][il];
                total_time[23 * counter + 22] = compute_size2[l][il];

                counter++;
            }
        }
        MPI_Gather(total_time, 23 * total_iteration_count, MPI_DOUBLE, total_time_global, 23 * total_iteration_count, MPI_DOUBLE, 0, mcomm.get_local_comm());


        if (mcomm.get_rank() == 0)
        {
#if 0
            FILE *fp1;
            std::string all_time_file;
            all_time_file = output_dir + "/ALL_TIME";
            fp1 = fopen(all_time_file.c_str(), "w");
            fprintf(fp1, "rank\tTaskID\tIteration#\tBuffer_creation_time\tComputation_time\tAll_to_all_time\tBuffer_free_time\tInsert_in_newt_time\tIntra_comm_time\tInsert_in_full_time\tFixed_point_check\tTotal_time\tSLOAV_mode\tA2a_find_count_time\tA2a_create_rindex_time\tA2a_total_find_blocks_time\tA2a_total_pre_time\tA2a_total_send_meta_time\tA2a_total_replace_time\tA2a_exchange_time\tA2a_filter_Time\tcomp1\tcomp2\n");
            for (int j=0; j < 23 * total_iteration_count * mcomm.get_local_nprocs(); j++)
            {
                if (j % 23 == 0 && j != 0)
                    fprintf (fp1, "\n");
                fprintf (fp1, "%f\t", total_time_global[j]);

            }
            fclose(fp1);
#endif

            std::string all_time;
            all_time = output_dir + "/ALL_TIME.bin";
            int fpb = open(all_time.c_str(), O_CREAT | O_WRONLY, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
            u32 write_size1 = write(fpb, total_time_global, 23 * total_iteration_count * mcomm.get_local_nprocs() * sizeof(int));
            if (write_size1 != 23 * total_iteration_count * mcomm.get_local_nprocs() * sizeof(int))
            {
                std::cout << all_time <<  " Wrong IO: rank: " << " " << write_size1  << std::endl;
                MPI_Abort(mcomm.get_local_comm(), -1);
            }
            close(fpb);

            std::string all_to_all_time;
            all_to_all_time = output_dir + "/all_to_all";
            int fp = open(all_to_all_time.c_str(), O_CREAT | O_WRONLY, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
            u32 write_size = write(fp, all_to_all_global_buffer_all_iteration_size, mcomm.get_local_nprocs() * mcomm.get_local_nprocs() * sizeof(int));
            if (write_size != mcomm.get_local_nprocs() * mcomm.get_local_nprocs() * sizeof(int))
            {
                std::cout << all_to_all_time <<  " Wrong IO: rank: " << " " << write_size  << std::endl;
                MPI_Abort(mcomm.get_local_comm(), -1);
            }
            close(fp);
        }

        delete[] total_time_global;
        delete[] total_time;

        delete[] all_to_all_global_buffer_all_iteration_size;
#endif
    }

#if 0
    for (u32 i =0; i <lie_sccs_count; i++)
    {
        for (u32 j =0; j <MAX_LOOP_COUNT; j++)
            delete[] all_to_all_buffer_size[i][j];
        delete[] all_to_all_buffer_size[i];
    }
    delete[] all_to_all_buffer_size;
#endif


    for (u32 i =0; i <lie_sccs_count; i++)
        delete[] compute_size1[i];
    delete[] compute_size1;

    for (u32 i =0; i <lie_sccs_count; i++)
        delete[] compute_size2[i];
    delete[] compute_size2;

    for (u32 i =0; i <lie_sccs_count; i++)
    {
        delete[] running_time[i];
        delete[] running_intra_bucket_comm[i];
        delete[] running_buffer_allocate[i];
        delete[] running_local_compute[i];
        delete[] running_all_to_all[i];
        delete[] running_buffer_free[i];
        delete[] running_insert_newt[i];
        delete[] running_insert_in_full[i];
        delete[] running_fp[i];

        delete[] running_a2a_find_count_time[i];
        delete[] running_a2a_create_rindex_time[i];
        delete[] running_a2a_total_find_blocks_time[i];
        delete[] running_a2a_total_pre_time[i];
        delete[] running_a2a_total_send_meda_time[i];
        delete[] running_a2a_total_comm_time[i];
        delete[] running_a2a_total_replace_time[i];
        delete[] running_a2a_exchange_time[i];
        delete[] running_a2a_filter_time[i];
    }

    delete[] running_time;
    delete[] running_intra_bucket_comm;
    delete[] running_buffer_allocate;
    delete[] running_local_compute;
    delete[] running_all_to_all;
    delete[] running_buffer_free;
    delete[] running_insert_newt;
    delete[] running_insert_in_full;
    delete[] running_fp;

    delete[] running_a2a_find_count_time;
    delete[] running_a2a_create_rindex_time;
    delete[] running_a2a_total_find_blocks_time;
    delete[] running_a2a_total_pre_time;
    delete[] running_a2a_total_send_meda_time;
    delete[] running_a2a_total_comm_time;
    delete[] running_a2a_total_replace_time;
    delete[] running_a2a_exchange_time;
    delete[] running_a2a_filter_time;

    delete[] rotate_index_array;
    for (int i=0; i < nprocs; i++)
        delete[] send_indexes[i];
    delete[] send_indexes;
    delete[] sendb_num;
#endif





    return true;
}


LIE::~LIE ()
{
    for (u32 i = 0 ; i < lie_relation_count; i++)
    {
        lie_relations[i]->finalize_relation();
        delete (lie_relations[i]);
    }
}
