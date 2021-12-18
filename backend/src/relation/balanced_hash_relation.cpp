/*
 * Relation class
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#include "../parallel_RA_inc.h"
#include "balanced_hash_relation.h"
#include "btree_relation.h"
#include "trie_relation.h"
#include <cstddef>
#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <iterator>
#include <memory>

u32 relation::get_global_delta_element_count()
{
    int dec = (int)delta_element_count;
    int global_delta_element_count;
    MPI_Allreduce(&dec, &global_delta_element_count, 1, MPI_INT, MPI_SUM, mcomm.get_local_comm());
    return global_delta_element_count;
}


u32 relation::get_global_full_element_count()
{
    u32 global_full_element_count;
    MPI_Allreduce(&full_element_count, &global_full_element_count, 1, MPI_INT, MPI_SUM, mcomm.get_local_comm());
    return global_full_element_count;
}




void relation::serial_IO(std::string filename_template)
{
    std::string full_rel_name;
    full_rel_name = filename_template + "/" + get_debug_id() + "_full";

    std::string delta_rel_name;
    delta_rel_name = filename_template + "/" + get_debug_id() + "_delta";

    u32 buckets = get_bucket_count();
    assert(buckets == 1);
    if (mcomm.get_rank() == 0)
    {
        vector_buffer *vb_full = new vector_buffer[buckets];
        for (u32 i=0; i < buckets; i++)
        {
            vb_full[i].vector_buffer_create_empty();
            std::vector<u64> prefix = {};
            full[i].as_vector_buffer_recursive(&(vb_full[i]), prefix);

            if (vb_full[i].size != 0)
            {
                int fp = open(full_rel_name.c_str(), O_CREAT | O_WRONLY, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
                u32 write_size = write(fp, vb_full[i].buffer, vb_full[i].size);
                if (write_size != vb_full[i].size)
                {
                    std::cout << full_rel_name <<  " Wrong IO: rank: " << " " << vb_full[i].size  << std::endl;
                    MPI_Abort(mcomm.get_local_comm(), -1);
                }
                close(fp);
            }

            vb_full[i].vector_buffer_free();
        }
        delete[] vb_full;


        vector_buffer *vb_delta = new vector_buffer[buckets];
        for (u32 i=0; i < buckets; i++)
        {
            vb_delta[i].vector_buffer_create_empty();
            std::vector<u64> prefix = {};
            delta[i].as_vector_buffer_recursive(&(vb_delta[i]), prefix);

            if (vb_delta[i].size != 0)
            {
                int fp = open(delta_rel_name.c_str(), O_CREAT | O_WRONLY, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
                u32 write_size = write(fp, vb_delta[i].buffer, vb_delta[i].size);
                if (write_size != vb_delta[i].size)
                {
                    std::cout << delta_rel_name <<  " Wrong IO: rank: " << vb_full[i].size  << std::endl;
                    MPI_Abort(mcomm.get_local_comm(), -1);
                }
                close(fp);
            }

            vb_delta[i].vector_buffer_free();
        }
        delete[] vb_delta;
    }
}



void relation::parallel_IO(std::string  filename_template)
{
    std::string full_rel_name;
    std::string delta_rel_name;
    std::string full_file_offset;
    std::string delta_file_offset;

	if (separate_io == false)
	{
        full_rel_name = filename_template + "/" + get_debug_id() + "_full";
        delta_rel_name = filename_template + "/" + get_debug_id() + "_delta";
	}
	else
	{
        full_rel_name = filename_template + "/" + get_debug_id() + "_full_" + std::to_string(mcomm.get_rank());
        delta_rel_name = filename_template + "/" + get_debug_id() + "_delta_" + std::to_string(mcomm.get_rank());
	}

    full_file_offset = full_rel_name + ".offset";
    delta_file_offset= delta_rel_name + ".offset";


	/// timing
	double populate_metadata_time_full = 0;
	double write_metadata_time_full = 0;
	double write_full_data_time = 0;
	double populate_metadata_time_delta = 0;
	double write_metadata_time_delta = 0;
	double write_delta_data_time = 0;
	double total_time = 0;
	double polulate_buffer_full_time = 0;
	double polulate_buffer_delta_time = 0;

	double total_start = MPI_Wtime();
	/// Get the size for buffer and its size for each process
	double polulate_buffer_full_start =  MPI_Wtime();
	u32 buckets = get_bucket_count();
	vector_buffer *vb_full = new vector_buffer[buckets];
    u64 sizes[mcomm.get_local_nprocs()];
    u64 full_size = 0;
	unsigned char* full_buffer = NULL;
	for (u32 i=0; i < buckets; i++)
	{
		vb_full[i].vector_buffer_create_empty();
		std::vector<u64> prefix = {};
		full[i].as_vector_buffer_recursive(&(vb_full[i]), prefix);

		if (vb_full[i].size != 0)
		{
			full_size = vb_full[i].size;
			full_buffer = (unsigned char*)malloc(full_size);
			memcpy(full_buffer, vb_full[i].buffer, full_size);
		}
		vb_full[i].vector_buffer_free();
	}
	delete[] vb_full;
	double polulate_buffer_full_end = MPI_Wtime();
	polulate_buffer_full_time = polulate_buffer_full_end - polulate_buffer_full_start;


    u64 offset = 0;
    u64 offsets[mcomm.get_nprocs()];
	if (separate_io == false) /// write shared file
	{
		/// calculate the offset for each process

        uint64_t total_size_full = 0;
		double populate_metadata_start = MPI_Wtime();
		MPI_Allgather(&full_size, 1, MPI_LONG_LONG, sizes, 1, MPI_LONG_LONG, mcomm.get_comm());
		for (int i = 0; i < mcomm.get_nprocs(); i++)
		{
			offsets[i] = total_size_full;
			total_size_full += sizes[i];
		}
		offset = offsets[mcomm.get_rank()];
		double populate_metadata_end = MPI_Wtime();
		populate_metadata_time_full = populate_metadata_end - populate_metadata_start;
#if 1

        //std::cout << "Filename 2 : " << full_file_offset.c_str() << std::endl;
		/// write metadata out
		if (mcomm.get_rank() == 0 && total_size_full > 0)
		{
			double write_metadata_start = MPI_Wtime();
			FILE *fp;
			if (offset_io == true)  /// write offset metadata out
			{
                fp = fopen(full_file_offset.c_str(), "w");
				for (int i = 0; i < mcomm.get_nprocs(); i++)
                    fprintf (fp, "%d %lld %lld\n", i, (long long int)offsets[i], (long long int)sizes[i]);
				fclose(fp);
			}
			double write_metadata_end = MPI_Wtime();
			write_metadata_time_full = (write_metadata_end - write_metadata_start);
		}
#endif
		/// write data out
		double write_full_data_start = MPI_Wtime();
#if 1
		if (total_size_full > 0)
		{
			if (share_io == true)  /// MPI collective IO
			{
				MPI_File fp;
				MPI_Status stas;
				if (total_size_full < 1073741824)
				{
                    MPI_File_open(mcomm.get_comm(), full_rel_name.c_str(), MPI_MODE_CREATE | MPI_MODE_WRONLY, MPI_INFO_NULL, &fp);
					MPI_File_write_at_all(fp, offset, full_buffer, full_size, MPI_BYTE, &stas);
				}
				else
				{
					MPI_Info info;
					MPI_Info_create(&info);
					MPI_Info_set(info, "striping_factor", "48");
					MPI_Info_set(info, "striping_unit", "8388608");
					MPI_Info_set(info, "romio_cb_write" , "enable");
                    MPI_File_open(mcomm.get_comm(), full_rel_name.c_str(), MPI_MODE_CREATE | MPI_MODE_WRONLY, info, &fp);
					MPI_File_write_at_all(fp, offset, full_buffer, full_size, MPI_BYTE, &stas);
					MPI_Info_free(&info);
				}
				MPI_File_close(&fp);
			}
			else /// POSIX IO
			{
                int fp = open(full_rel_name.c_str(), O_CREAT | O_WRONLY, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
				u32 write_size = pwrite(fp, full_buffer, full_size, offset);
				if (write_size != full_size)
				{
					std::cout << full_rel_name <<  " Wrong IO: rank: " << " " << full_size  << std::endl;
					MPI_Abort(mcomm.get_local_comm(), -1);
				}
				close(fp);
			}
		}
#endif
		double write_full_data_end = MPI_Wtime();
		write_full_data_time = (write_full_data_end - write_full_data_start);
	}
	else /// write separated files
	{
		double write_full_data_start = MPI_Wtime();
		if (full_size > 0)
		{
            int fp = open(full_rel_name.c_str(), O_CREAT | O_WRONLY, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
			u32 write_size = write(fp, full_buffer, full_size);
			if (write_size != full_size)
			{
				std::cout << full_rel_name <<  " Wrong IO: rank: " << " " << full_size  << std::endl;
				MPI_Abort(mcomm.get_local_comm(), -1);
			}
			close(fp);
		}
		double write_full_data_end = MPI_Wtime();
		write_full_data_time = (write_full_data_end - write_full_data_start);
	}
	free(full_buffer);

#if 1
	/// get buffer and size for each process
	vector_buffer *vb_delta = new vector_buffer[buckets];
	memset(sizes, 0,  mcomm.get_nprocs()*sizeof(uint64_t));
	uint64_t delta_size = 0;
	unsigned char* delta_buffer = NULL;
	double polulate_buffer_delta_start = MPI_Wtime();
	for (u32 i=0; i < buckets; i++)
	{
		vb_delta[i].vector_buffer_create_empty();
		std::vector<u64> prefix = {};
		delta[i].as_vector_buffer_recursive(&(vb_delta[i]), prefix);

		if (vb_delta[i].size != 0)
		{
			delta_size = vb_delta[i].size;
			delta_buffer = (unsigned char*)malloc(delta_size);
			memcpy(delta_buffer, vb_delta[i].buffer, delta_size);
		}
		vb_delta[i].vector_buffer_free();
	}
	delete[] vb_delta;
	double polulate_buffer_delta_end = MPI_Wtime();
	polulate_buffer_delta_time = polulate_buffer_delta_end - polulate_buffer_delta_start;


	if (separate_io == false)
	{
		/// calculate offset for each process
		uint64_t total_size_delta = 0;
		memset(offsets, 0,  mcomm.get_nprocs()*sizeof(uint64_t));
		double populate_metadata_start = MPI_Wtime();
		MPI_Allgather(&delta_size, 1, MPI_LONG_LONG, sizes, 1, MPI_LONG_LONG, mcomm.get_comm());
		for (int i = 0; i < mcomm.get_nprocs(); i++)
		{
			offsets[i] = total_size_delta;
			total_size_delta += sizes[i];
		}
		offset = offsets[mcomm.get_rank()];
		double populate_metadata_end = MPI_Wtime();
		populate_metadata_time_delta = (populate_metadata_end - populate_metadata_start);

		if (mcomm.get_rank() == 0 && total_size_delta > 0)
		{
			double write_metadata_start = MPI_Wtime();
			FILE *fp;
			if (offset_io == true) /// write offset metadata out
			{
                fp = fopen(delta_file_offset.c_str(), "w");
				for (int i = 0; i < mcomm.get_nprocs(); i++)
                    fprintf (fp, "%d %lld %lld\n", i, (long long int)offsets[i], (long long int)sizes[i]);
				fclose(fp);
			}
			double write_metadata_end = MPI_Wtime();
			write_metadata_time_delta = (write_metadata_end - write_metadata_start);
		}

		double write_delta_data_start = MPI_Wtime();
		if (total_size_delta > 0)
		{
			if (share_io == true)
			{
				MPI_File fp;
				MPI_Status stas;
				if (total_size_delta < 1073741824)
				{
                    MPI_File_open(mcomm.get_comm(), delta_rel_name.c_str(), MPI_MODE_CREATE | MPI_MODE_WRONLY, MPI_INFO_NULL, &fp);
					MPI_File_write_at_all(fp, offset, delta_buffer, delta_size, MPI_BYTE, &stas);
				}
				else
				{
					MPI_Info info;
					MPI_Info_create(&info);
					MPI_Info_set(info, "striping_factor", "48");
					MPI_Info_set(info, "striping_unit", "8388608");
					MPI_Info_set(info, "romio_cb_write" , "enable") ;
                    MPI_File_open(mcomm.get_comm(), delta_rel_name.c_str(), MPI_MODE_CREATE | MPI_MODE_WRONLY, info, &fp);
					MPI_File_write_at_all(fp, offset, delta_buffer, delta_size, MPI_BYTE, &stas);
					MPI_Info_free(&info);
				}
				MPI_File_close(&fp);
			}
			else
			{
                int fp = open(delta_rel_name.c_str(), O_CREAT | O_WRONLY, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
				u32 write_size = pwrite(fp, delta_buffer, delta_size, offset);
				if (write_size != delta_size)
				{
					std::cout << delta_rel_name <<  " Wrong IO: rank: " << " " << delta_size << std::endl;
					MPI_Abort(mcomm.get_local_comm(), -1);
				}
				close(fp);
			}
		}
		double write_delta_data_end = MPI_Wtime();
		write_delta_data_time = (write_delta_data_end - write_delta_data_start);
	}
	else
	{
		double write_delta_data_start = MPI_Wtime();
		if (delta_size > 0)
		{
            int fp = open(delta_rel_name.c_str(), O_CREAT | O_WRONLY, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
			u32 write_size = write(fp, delta_buffer, delta_size);
			if (write_size != delta_size)
			{
				std::cout << delta_rel_name <<  " Wrong IO: rank: " << " " << delta_size << std::endl;
				MPI_Abort(mcomm.get_local_comm(), -1);
			}
			close(fp);
		}
		double write_delta_data_end = MPI_Wtime();
		write_delta_data_time = (write_delta_data_end - write_delta_data_start);
	}
	free(delta_buffer);
#endif
	double total_end = MPI_Wtime();
	total_time = total_end - total_start;

	double max_total_time = 0;
	MPI_Allreduce(&total_time, &max_total_time, 1, MPI_DOUBLE, MPI_MAX, mcomm.get_local_comm());

#if 0
	std::string write_io = (share_io == true)? "MPI IO": "POSIX IO";

	if (separate_io == true)
	{
		if (max_total_time == total_time)
		{
            fprintf(stderr, "%s, (%s) %f:\n FULL [S] [PB] [PM] [WM] [WD] %llu, %f, %f, %f, %f\n DELTA [S] [PB] [PM] [WM] [WD] %lld, %f, %f, %f, %f\n",
                    get_debug_id().c_str(), write_io.c_str(), total_time, (long long int)full_size, polulate_buffer_full_time, populate_metadata_time_full, write_metadata_time_full, write_full_data_time,
                    (long long int)delta_size, polulate_buffer_delta_time, populate_metadata_time_delta, write_metadata_time_delta, write_delta_data_time);
		}
	}
	else
	{
        if (mcomm.get_rank() == 0)
            std::cout << "Write " << get_debug_id() << " (" << write_io << ") " << total_time << ", " << total_time << " :\n  FULL [S] [PB] [PM] [WM] [WD], " << full_size << ", " << polulate_buffer_full_time<< ", " << populate_metadata_time_full << ", " <<
            write_metadata_time_full << ", " << write_full_data_time << "\n  DELTA [S] [PB] [PM] [WM] [WD], " <<  delta_size << ", " << polulate_buffer_delta_time << ", " << populate_metadata_time_delta
            << ", " <<  write_metadata_time_delta << ", " << write_delta_data_time << std::endl;
	}
#endif
}


void relation::print()
{
    u32 buckets = get_bucket_count();
//    if (mcomm.get_rank() == 0)
//    {
        vector_buffer *vb_full = new vector_buffer[buckets];
        for (u32 i=0; i < buckets; i++)
        {
            vb_full[i].vector_buffer_create_empty();
            std::vector<u64> prefix = {};
            full[i].as_vector_buffer_recursive(&(vb_full[i]), prefix);

            if (vb_full[i].size != 0)
            	std::cout << get_debug_id() << " " << mcomm.get_rank() << " FULL Rows " << vb_full[i].size/(sizeof(u64) * (arity + 1)) << " columns " << arity + 1 << std::endl;
            for (u32 j=0; j < vb_full[i].size/sizeof(u64); j = j + arity+1)
            {
                if (j % (arity+1) == 0)
                    std::cout << "F [" << j/(arity+1) << "] ";
                for (u32 k = 0; k < arity+1; k++)
                {
                    u64 temp;
                    memcpy(&temp, (vb_full[i].buffer) + (j + k)*sizeof(u64), sizeof(u64));
                    std::cout << temp << " ";
                }
                std::cout << std::endl;
            }

            vb_full[i].vector_buffer_free();
        }
        delete[] vb_full;


        vector_buffer *vb_delta = new vector_buffer[buckets];
        for (u32 i=0; i < buckets; i++)
        {
            vb_delta[i].vector_buffer_create_empty();
            std::vector<u64> prefix = {};
            delta[i].as_vector_buffer_recursive(&(vb_delta[i]), prefix);

            if (vb_delta[i].size != 0)
                std::cout << get_debug_id() << " " << mcomm.get_rank() << " DELTA Rows " << vb_delta[i].size/(sizeof(u64) * (arity + 1)) << " columns " << arity + 1 << std::endl;

            for (u32 j=0; j < vb_delta[i].size/sizeof(u64); j = j + arity+1)
            {
                if (j % (arity+1) == 0)
                    std::cout << "D ";

                for (u32 k = 0; k < arity+1; k++)
                {
                    u64 temp;
                    memcpy(&temp, (vb_delta[i].buffer) + (j + k)*sizeof(u64), sizeof(u64));
                    std::cout << temp << " ";
                }
                std::cout << std::endl;
            }

            vb_delta[i].vector_buffer_free();
        }
        delete[] vb_delta;

        /*
        vector_buffer *vb_newt = new vector_buffer[buckets];
        std::cout << "NEWT ";
        for (u32 i=0; i < buckets; i++)
        {
            vb_newt[i].vector_buffer_create_empty();
            std::vector<u64> prefix = {};
            newt[i].as_vector_buffer_recursive(&(vb_newt[i]), prefix);

            std::cout << vb_newt[i].size/sizeof(u64) << " arity " << arity+1 << std::endl;
            for (u32 j=0; j < vb_newt[i].size/sizeof(u64); j = j + arity+1)
            {
                if (j % (arity+1) == 0)
                    std::cout << "N ";

                for (u32 k = 0; k < arity+1; k++)
                {
                    u64 temp;
                    memcpy(&temp, (vb_newt[i].buffer) + (j + k)*sizeof(u64), sizeof(u64));
                    std::cout << temp << " ";
                }
                std::cout << std::endl;
            }

            vb_newt[i].vector_buffer_free();
        }
        delete[] vb_newt;
        */
//    }
}


#if 0
void relation::flush_full()
{
    full[mcomm.get_rank()]->remove_tuple();
    full_element_count = 0;
    for (int i =0; i < get_bucket_count(); i++)
    {
        full_bucket_element_count[i] = 0;
        for (u32 j=0; j < sub_bucket_per_bucket_count[i]; j++)
            full_sub_bucket_element_count[i][j] = 0;
    }
}



void relation::read_from_relation(relation* input, int full_delta)
{
    std::vector<u64> prefix = {};
    vector_buffer vb;
    vb.vector_buffer_create_empty();

    full[mcomm.get_rank()]->as_vector_buffer_recursive(&vb, prefix);

    if (full_delta == DELTA)
        populate_delta(vb.size/sizeof(u64), (u64*)vb.buffer);

    else if (full_delta == FULL)
        populate_full(vb.size/sizeof(u64), (u64*)vb.buffer);

    vb.vector_buffer_free();
}
#endif

void relation::load_data_from_separate_files()
{
	double read_data_start = MPI_Wtime();
	file_io.parallel_read_input_relation_from_separate_files(arity, filename, mcomm.get_local_comm());
	double read_data_end = MPI_Wtime();
	double read_data_time = read_data_end - read_data_start;

	if (initailization_type == DELTA)
		 populate_delta(file_io.get_hash_buffer_size(), file_io.get_hash_buffer());
	else if (initailization_type == FULL)
		populate_full(file_io.get_hash_buffer_size(), file_io.get_hash_buffer());

	file_io.delete_hash_buffers();

    double max_read_data_time = 0;
    MPI_Reduce(&read_data_time, &max_read_data_time, 1, MPI_DOUBLE, MPI_MAX, 0, mcomm.get_local_comm());

    std::string read_io = (share_io == true)? "MPI IO": "POSIX IO";
    std::string type = (initailization_type == DELTA)? "DELTA": "FULL";

    if (mcomm.get_rank() == 0 && restart_flag == true)
    	std::cout << "Read " << get_debug_id() << " (" << read_io << ") :\n  " << type << " [RD], " <<
		max_read_data_time << std::endl;
}


void relation::load_data_from_file_with_offset()
{
	double read_data_start = MPI_Wtime();
	file_io.parallel_read_input_relation_from_file_with_offset(arity, filename, mcomm.get_local_comm());
	double read_data_end = MPI_Wtime();
	double read_data_time = read_data_end - read_data_start;

	if (initailization_type == DELTA)
		 populate_delta(file_io.get_hash_buffer_size(), file_io.get_hash_buffer());
	else if (initailization_type == FULL)
		populate_full(file_io.get_hash_buffer_size(), file_io.get_hash_buffer());

	file_io.delete_hash_buffers();

    double max_read_data_time = 0;
    MPI_Reduce(&read_data_time, &max_read_data_time, 1, MPI_DOUBLE, MPI_MAX, 0, mcomm.get_local_comm());

    std::string read_io = (share_io == true)? "MPI IO": "POSIX IO";
    std::string type = (initailization_type == DELTA)? "DELTA": "FULL";

    if (mcomm.get_rank() == 0 && restart_flag == true)
    	std::cout << "Read " << get_debug_id() << " (" << read_io << ") :\n  " << type << " [RD], " <<
		max_read_data_time << std::endl;
}

void relation::load_data_from_file()
{
    if (!std::filesystem::exists(this->get_filename()))
    {
        // if file not exists don't IO
        return;
    }
    // std::cout << "relation with tag :" << this->get_intern_tag() << " "
    //           << "filename :" << this->get_filename() << " "
    //         //   << "c++ object " << this
    //           << "start normal IO" << std::endl;
    /// reading from file
    if (initailization_type != -1)
    {
        /// Main : Execute : init : io : end
    	double read_data_start = MPI_Wtime();
        //std::cout << "111111 Filename " << filename << std::endl;
        file_io.parallel_read_input_relation_from_file_to_local_buffer(arity, filename, mcomm.get_local_comm());
        double read_data_end = MPI_Wtime();
        double read_data_time = read_data_end - read_data_start;

        double all_to_all_start = MPI_Wtime();
        file_io.buffer_data_to_hash_buffer_col(arity, join_column_count, get_bucket_count(), sub_bucket_rank, sub_bucket_per_bucket_count, mcomm.get_local_comm());
        double all_to_all_end = MPI_Wtime();
        double all_to_all_time = all_to_all_end - all_to_all_start;

        file_io.delete_raw_buffers();

        /* Copy data from buffer to relation */
        if (initailization_type == DELTA)
            populate_delta(file_io.get_hash_buffer_size(), file_io.get_hash_buffer());

        else if (initailization_type == FULL)
            populate_full(file_io.get_hash_buffer_size(), file_io.get_hash_buffer());

        file_io.delete_hash_buffers();

        double max_read_data_time = 0;
        double max_all_to_all_time = 0;
        MPI_Reduce(&read_data_time, &max_read_data_time, 1, MPI_DOUBLE, MPI_MAX, 0, mcomm.get_local_comm());
        MPI_Reduce(&all_to_all_time, &max_all_to_all_time, 1, MPI_DOUBLE, MPI_MAX, 0, mcomm.get_local_comm());

        std::string read_io = (share_io == true)? "MPI IO": "POSIX IO";
        std::string type = (initailization_type == DELTA)? "DELTA": "FULL";

        if (mcomm.get_rank() == 0 && restart_flag == true)
        	std::cout << "Read " << get_debug_id() << " (" << read_io << ") :\n " << type << " [RD] [AC], " <<
			max_read_data_time << ", " << max_all_to_all_time << std::endl;

        //int f_size = get_full_element_count();
        //u32 g_f_size = 0;
        //MPI_Allreduce(&f_size, &g_f_size, 1, MPI_INT, MPI_SUM, mcomm.get_local_comm());
        //if (rank == 0)
        //    std::cout << "After Init " << filename << " " << g_f_size << std::endl;
        //std::cout << "Writing " << file_io.get_hash_buffer_size() << " bytes" << std::endl;

    }
}

void relation::initialize_relation(mpi_comm& mcomm, std::map<u64, u64>& intern_map)
{
    /// Main : Execute : init : buffer_init : start
    this->mcomm = mcomm;

    u32 buckets = mcomm.get_local_nprocs();

    default_sub_bucket_per_bucket_count = 1;
    int rank = mcomm.get_local_rank();
    int nprocs = mcomm.get_local_nprocs();

    newt_element_count = 0;
    full_element_count = 0;
    delta_element_count = 0;

#ifdef GOOGLE_MAP
    delta = new google_relation[buckets];
    full = new google_relation[buckets];
    newt = new google_relation[buckets];
#else
    if (data_structure_type == BTREE)
    {
        delta = new btree_relation[buckets];
        full = new btree_relation[buckets];
        newt = new btree_relation[buckets];
        for (size_t i=0; i < buckets; i++)
        {
            delta[i].arity = arity;
            full[i].arity = arity;
            newt[i].arity = arity;
        }
    }
    // else 
    // {
    //     std::cout << this->intern_tag << " creating  relation object!" << "buckect count " << buckets << std::endl;
    //     delta = new trie_relation[buckets];
    //     full = new trie_relation[buckets];
    //     newt = new trie_relation[buckets];
    //     for (size_t i=0; i < buckets; i++)
    //     {
    //         delta[i].arity = arity;
    //         full[i].arity = arity;
    //         newt[i].arity = arity;
    //     }
    // }
    
#endif

    sub_bucket_per_bucket_count = new u32[buckets];
    for (u32 b = 0; b < buckets; b++)
    {
        //delta[b] = new google_relation();
        //full[b] = new google_relation();
        //newt[b] = new google_relation();

        sub_bucket_per_bucket_count[b] = default_sub_bucket_per_bucket_count;
    }

    sub_bucket_rank = new u32*[buckets];
    distinct_sub_bucket_rank = new int*[buckets];
    distinct_sub_bucket_rank_count = new int[buckets];

    bucket_map = new u32[buckets];
    memset(bucket_map, 0, sizeof(u32) * buckets);

    full_sub_bucket_element_count = new u32*[buckets];
    memset(full_sub_bucket_element_count, 0, sizeof(u32*) * buckets);

    delta_sub_bucket_element_count = new u32*[buckets];
    memset(delta_sub_bucket_element_count, 0, sizeof(u32*) * buckets);

    newt_sub_bucket_element_count = new u32*[buckets];
    memset(newt_sub_bucket_element_count, 0, sizeof(u32*) * buckets);

    full_bucket_element_count = new u32[buckets];
    memset(full_bucket_element_count, 0, sizeof(u32) * buckets);

    delta_bucket_element_count = new u32[buckets];
    memset(delta_bucket_element_count, 0, sizeof(u32) * buckets);

    newt_bucket_element_count = new u32[buckets];
    memset(newt_bucket_element_count, 0, sizeof(u32) * buckets);

    int rcount = 0;
    for (u32 b = 0; b < buckets; b++)
    {
        sub_bucket_rank[b] = new u32[sub_bucket_per_bucket_count[b]];
        std::unordered_set<int> distinct_ranks;
        for (u64 x = 0; x < sub_bucket_per_bucket_count[b]; x++)
        {
            sub_bucket_rank[b][x] = rcount % nprocs;
            set_last_rank(rcount);

            if (sub_bucket_rank[b][x] == (u32)rank)
                bucket_map[b] = 1;

            distinct_ranks.insert(sub_bucket_rank[b][x]);
            rcount++;
        }

        distinct_sub_bucket_rank_count[b] = distinct_ranks.size();
        distinct_sub_bucket_rank[b] = new int[distinct_sub_bucket_rank_count[b]];
        u32 x  = 0;
        for ( auto it = distinct_ranks.begin(); it != distinct_ranks.end(); ++it, x++ )
            distinct_sub_bucket_rank[b][x] = *it;

        full_sub_bucket_element_count[b] = new u32[sub_bucket_per_bucket_count[b]];
        memset(full_sub_bucket_element_count[b], 0, sizeof(u32) * sub_bucket_per_bucket_count[b]);

        delta_sub_bucket_element_count[b] = new u32[sub_bucket_per_bucket_count[b]];
        memset(delta_sub_bucket_element_count[b], 0, sizeof(u32) * sub_bucket_per_bucket_count[b]);

        newt_sub_bucket_element_count[b] = new u32[sub_bucket_per_bucket_count[b]];
        memset(newt_sub_bucket_element_count[b], 0, sizeof(u32) * sub_bucket_per_bucket_count[b]);
    }
    /// Main : Execute : init : buffer_init : end
    file_io.set_share_io(share_io);

    /*
    // if no input file stop reading, just return
    // TODO: ww
    if ((!this->is_canonical) || (!std::filesystem::exists(this->filename)))
    {
        // std::cout << "relation :" << this->get_filename()  << "not exists!";
        return;
    }
    */

    /// read data from file
    if (restart_flag)
    {
		if (separate_io == true)
		{
            filename = filename + "_" + std::to_string(mcomm.get_local_rank());
			load_data_from_separate_files();
		}
		else if (offset_io == true)
	    	load_data_from_file_with_offset();
		else
	    	load_data_from_file();
    }
    else
    {
    	load_data_from_file();
    }

    //std::cout << filename << " " << fact_load << std::endl;
    //if (fact_load == true)
        //if (init_val.size() != 0)
            //populate_delta_with_keys(init_val.size(), reinterpret_cast<u64*>(init_val.data()), intern_map);
}




void relation::populate_full(int buffer_size, u64* buffer)
{
    u32 counter = 0;
    u64 t[arity+1];
    u32 buckets = get_bucket_count();

    for (int i = 0; i < buffer_size; i = i + (arity+1))
    {
        uint64_t bucket_id = tuple_hash(buffer + i, join_column_count) % buckets;

        for (u32 a = i; a < i + arity + 1; a++)
            t[a-i] = buffer[a];

        // std::cout << " bucket size " << buckets << " bucket_id " << bucket_id << std::endl;
        // u64 tmp[3] =  {1,1,1};
        // std::cout << "null? " << full[bucket_id].insert_tuple_from_array(tmp, 3) << std::endl;
        // TODO: why arity  + 1???? in insert function it always use arity-1!!
        if (full[bucket_id].insert_tuple_from_array(t, (arity+1)) == true)
        {
            // std::cout << " bucket size " << buckets << " bucket_id " << bucket_id << std::endl;
            full_element_count++;
            full_bucket_element_count[bucket_id]++;
            counter++;
        }
    }
    // for(size_t i=0; i < buckets ; i++)
    // {
    //     for(const auto &tp : full[i])
    //     {
    //         std::cout << "t>>>";
    //         for (const auto &v: tp)
    //         {
    //             std::cout << v << " ";
    //         }
    //         std::cout << std::endl;
    //     }
    // }
}



void relation::populate_delta (int buffer_size, u64* buffer)
{
    u64 t[arity+1];
    u32 buckets = get_bucket_count();

    for (int i = 0; i < buffer_size; i = i + (arity+1))
    {
        uint64_t bucket_id = tuple_hash(buffer + i, join_column_count) % buckets;

        for (u32 a = i; a < i + arity + 1; a++)
            t[a-i] = buffer[a];

        if (delta[bucket_id].insert_tuple_from_array(t, arity+1) == true)
        {
            delta_element_count++;
            delta_bucket_element_count[bucket_id]++;
        }
    }
}

/*
void relation::populate_delta_with_keys (int buffer_size, u64* buffer, std::map<u64, u64>& intern_map)
{
    u64 t[arity+1];
    u64 intern_key=0;
    u32 buckets = get_bucket_count();

    //std::cout << "arity " << arity << std::endl;
    //std::cout << "buffer_size " << buffer_size << std::endl;
    //std::cout << "join_column_count " << join_column_count << std::endl;
    //std::cout << "buffer[0] " << buffer[0] << std::endl;
    //std::cout << "buckets " << buckets << std::endl;

    if (buffer_size == 0)
    {
        uint64_t bucket_id = tuple_hash(0, join_column_count) % buckets;
        assert(buffer_size == 0);
        if (bucket_id == mcomm.get_local_rank())
        {
            bucket_id = bucket_id<<28;
            u64 intern_value=0;
            u64 relation_id = get_intern_tag();
            relation_id = relation_id<<46;
            intern_key = relation_id | bucket_id;

            std::map<u64 ,u64>::const_iterator it = intern_map.find(intern_key);
            if( it == intern_map.end() )
                intern_value=0;
            else
                intern_value = it->second + 1;

            intern_map[intern_key] = intern_value;
            t[arity] = intern_key | intern_value;    /// Intern here
            assert(arity == 0);



            //std::cout << "Inserting " << t[0] << " " << intern_key << std::endl;
            if (delta[bucket_id].insert_tuple_from_array(t, arity+1) == true)
            {
                delta_element_count++;
                delta_bucket_element_count[bucket_id]++;
            }
        }

    }
    else
    {
    for (int i = 0; i < buffer_size; i = i + (arity))
    {
        uint64_t bucket_id = tuple_hash(buffer + i, join_column_count) % buckets;
        if (bucket_id == mcomm.get_local_rank())
        {
            bucket_id = bucket_id<<28;
            u64 intern_value=0;
            u64 relation_id = get_intern_tag();
            relation_id = relation_id<<46;
            intern_key = relation_id | bucket_id;

            std::map<u64 ,u64>::const_iterator it = intern_map.find(intern_key);
            if( it == intern_map.end() )
                intern_value=0;
            else
                intern_value = it->second + 1;

            intern_map[intern_key] = intern_value;
            t[arity] = intern_key | intern_value;    /// Intern here

            for (u32 a = i; a < i + arity; a++)
                t[a-i] = buffer[a];

            //std::cout << "Inserting " << t[0] << " " << intern_key << std::endl;
            if (delta[bucket_id].insert_tuple_from_array(t, arity+1) == true)
            {
                delta_element_count++;
                delta_bucket_element_count[bucket_id]++;
            }
        }
    }
    }
}
*/



void relation::finalize_relation()
{
    u32 buckets = get_bucket_count();
    newt_element_count = 0;
    full_element_count = 0;
    delta_element_count = 0;

    initailization_type = -1;

    delete[] distinct_sub_bucket_rank_count;
    for (u64 b = 0; b < buckets; b++)
        delete[] distinct_sub_bucket_rank[b];
    delete[] distinct_sub_bucket_rank;

    for (u32 i = 0; i < buckets; i++)
    {

        full[i].remove_tuple();
        delta[i].remove_tuple();
        newt[i].remove_tuple();

        //delete[] full[i];
        //delete[] delta[i];
        //delete[] newt[i];

        delete[] sub_bucket_rank[i];
        delete[] full_sub_bucket_element_count[i];
        delete[] delta_sub_bucket_element_count[i];
        delete[] newt_sub_bucket_element_count[i];
    }

    delete[] delta_sub_bucket_element_count;
    delete[] delta_bucket_element_count;

    delete[] full_bucket_element_count;
    delete[] full_sub_bucket_element_count;

    delete[] newt_bucket_element_count;
    delete[] newt_sub_bucket_element_count;

    delete[] delta;
    delete[] full;
    delete[] newt;
    delete[] bucket_map;
    delete[] sub_bucket_per_bucket_count;
    delete[] sub_bucket_rank;
}



void relation::copy_relation(relation*& recv_rel, mpi_comm output_comm, int target_cumulative_rank, int tuples_per_task, u32 input_buckets, u32 output_buckets)
{
    u32 *output_sub_bucket_per_bucket_count = new u32[output_buckets];
    u32 **output_sub_bucket_rank = new u32*[output_buckets];

    for (u32 b = 0; b < output_buckets; b++)
        output_sub_bucket_per_bucket_count[b] = default_sub_bucket_per_bucket_count;

    int rcount = 0;
    for (u32 b = 0; b < output_buckets; b++)
    {
        output_sub_bucket_rank[b] = new u32[output_sub_bucket_per_bucket_count[b]];
        for (u64 x = 0; x < output_sub_bucket_per_bucket_count[b]; x++)
        {
            output_sub_bucket_rank[b][x] = rcount % output_buckets;
            rcount++;
        }
    }

    //google_relation* full = input->get_full();
    vector_buffer *full_input_buffer = new vector_buffer[input_buckets];
    for (u32 i = 0; i < input_buckets; i++)
        full_input_buffer[i].vector_buffer_create_empty();
    int full_process_size[output_comm.get_nprocs()];
    memset(full_process_size, 0, output_comm.get_nprocs() * sizeof(int));

    int full_output_size=0;
    vector_buffer* full_output = new vector_buffer[output_comm.get_nprocs()];
    for (int i = 0; i < output_comm.get_nprocs(); i++)
        full_output[i].vector_buffer_create_empty();

    int delta_output_size=0;
    vector_buffer *delta_input_buffer = new vector_buffer[input_buckets];
    for (u32 i = 0; i < input_buckets; i++)
        full_input_buffer[i].vector_buffer_create_empty();

    int delta_process_size[output_comm.get_nprocs()];
    memset(delta_process_size, 0, output_comm.get_nprocs() * sizeof(int));

    vector_buffer* delta_output = new vector_buffer[output_comm.get_nprocs()];
    for (int i = 0; i < output_comm.get_nprocs(); i++)
        delta_output[i].vector_buffer_create_empty();


    int fsize = 0;
    int dsize = 0;
    if (tuples_per_task != 0)
    {
        for (u32 i = 0; i < input_buckets; i++)
        {
            std::vector<u64> prefix = {};
            full[i].as_vector_buffer_recursive(&(full_input_buffer[i]), prefix);

            fsize = fsize + (full_input_buffer[i].size / sizeof(u64));
            for (u32 s = 0; s < full_input_buffer[i].size / sizeof(u64); s=s+arity)
            {
                u64 reordered_cur_path[arity];
                for (u32 j =0; j < arity; j++)
                    memcpy(reordered_cur_path + j, (&full_input_buffer[i])->buffer + ((s + j) * sizeof(u64)), sizeof(u64));

                uint64_t bucket_id = tuple_hash(reordered_cur_path, join_column_count) % output_buckets;
                uint64_t sub_bucket_id = tuple_hash(reordered_cur_path + join_column_count, arity-join_column_count) % output_sub_bucket_per_bucket_count[bucket_id];
                int index = output_sub_bucket_rank[bucket_id][sub_bucket_id] + target_cumulative_rank;

                full_process_size[index] = full_process_size[index] + arity;
                full_output[index].vector_buffer_append((const unsigned char*)reordered_cur_path, sizeof(u64)*arity);
                full_output_size = full_output_size + (arity+1);
            }
            full_input_buffer[i].vector_buffer_free();
        }

        for (u32 i = 0; i < input_buckets; i++)
        {
            std::vector<u64> prefix = {};
            delta[i].as_vector_buffer_recursive(&(delta_input_buffer[i]), prefix);

            dsize = dsize + (delta_input_buffer[i].size / sizeof(u64));
            for (u32 s = 0; s < delta_input_buffer[i].size / sizeof(u64); s=s+arity)
            {
                u64 reordered_cur_path[arity];
                for (u32 j =0; j < arity; j++)
                    memcpy(reordered_cur_path + j, (&delta_input_buffer[i])->buffer + ((s + j) * sizeof(u64)), sizeof(u64));

                uint64_t bucket_id = tuple_hash(reordered_cur_path, join_column_count) % output_buckets;
                uint64_t sub_bucket_id = tuple_hash(reordered_cur_path + join_column_count, arity-join_column_count) % output_sub_bucket_per_bucket_count[bucket_id];
                int index = output_sub_bucket_rank[bucket_id][sub_bucket_id] + target_cumulative_rank;

                delta_process_size[index] = delta_process_size[index] + arity;
                delta_output[index].vector_buffer_append((const unsigned char*)reordered_cur_path, sizeof(u64)*arity);
                delta_output_size = delta_output_size + (arity+1);
            }
            delta_input_buffer[i].vector_buffer_free();
        }
    }

    for (u32 b = 0; b < output_buckets; b++)
        delete[] output_sub_bucket_rank[b];
    delete[] output_sub_bucket_rank;

    delete[] output_sub_bucket_per_bucket_count;


    delete[] delta_input_buffer;
    delete[] full_input_buffer;


    int full_buffer_size;
    u64* full_buffer;
    all_to_all_comm(full_output, full_output_size, full_process_size, &full_buffer_size, &full_buffer, mcomm.get_comm());
    delete[] full_output;


    finalize_relation();
    recv_rel->set_initailization_type(-1);
    //recv_rel->initialize_relation(output_comm);


    u64 t[arity];
    for (int i=0; i < full_buffer_size; i=i+arity)
    {
        for (u32 j = 0; j < arity; j++)
            t[j] = full_buffer[i+j];
        recv_rel->insert_in_full(t);
    }


    int delta_buffer_size;
    u64* delta_buffer;
    all_to_all_comm(delta_output, delta_output_size, delta_process_size, &delta_buffer_size, &delta_buffer, mcomm.get_comm());
    delete[] delta_output;



    for (int i=0; i < delta_buffer_size; i=i+arity)
    {
        for (u32 j = 0; j < arity; j++)
            t[j] = full_buffer[i+j];
        recv_rel->insert_in_delta(t);
    }

    delete[] full_buffer;
    delete[] delta_buffer;
}



bool relation::find_in_full(u64* t, int length)
{
    uint64_t bucket_id = tuple_hash(t, join_column_count) % get_bucket_count();
    return full[bucket_id].find_tuple_from_array(t, length);
}



bool relation::find_in_delta(u64* t, int length)
{
    uint64_t bucket_id = tuple_hash(t, join_column_count) % get_bucket_count();
    return delta[bucket_id].find_tuple_from_array(t, length);
}



bool relation::find_in_newt(u64* t, int length)
{
    uint64_t bucket_id = tuple_hash(t, join_column_count) % get_bucket_count();
    return newt[bucket_id].find_tuple_from_array(t, length);
}



bool relation::insert_in_delta(u64* t)
{
    uint64_t bucket_id = tuple_hash(t, join_column_count) % get_bucket_count();
    u32 sub_bucket_id = 0;
    if (is_canonical == false   && arity != 0 && arity >= join_column_count)
        sub_bucket_id = tuple_hash(t + join_column_count, arity-join_column_count) % sub_bucket_per_bucket_count[bucket_id];

    //assert((int)bucket_id == mcomm.get_local_rank());
    if (delta[bucket_id].insert_tuple_from_array(t, arity+1) == true)
    {
        delta_element_count++;
        delta_bucket_element_count[bucket_id]++;
        delta_sub_bucket_element_count[bucket_id][sub_bucket_id]++;
        bucket_map[bucket_id] = 1;

        return true;
    }
    return false;
}



bool relation::insert_in_newt(u64* t)
{
    uint64_t bucket_id = tuple_hash(t, join_column_count) % get_bucket_count();
    u32 sub_bucket_id = 0;
    if (is_canonical == false && arity != 0 && arity >= join_column_count)
        sub_bucket_id = tuple_hash(t + join_column_count, arity-join_column_count) % sub_bucket_per_bucket_count[bucket_id];

    //assert((int)bucket_id == mcomm.get_local_rank());
    if (newt[bucket_id].insert_tuple_from_array(t, arity+1) == true)
    {
        newt_element_count++;
        newt_bucket_element_count[bucket_id]++;
        newt_sub_bucket_element_count[bucket_id][sub_bucket_id]++;
        bucket_map[bucket_id] = 1;

        return true;
    }
    return false;
}



bool relation::insert_in_full(u64* t)
{
    u32 buckets = get_bucket_count();
    uint64_t bucket_id = tuple_hash(t, join_column_count) % buckets;
    uint64_t sub_bucket_id=0;
    if (is_canonical == false  && arity != 0 && arity >= join_column_count)
        sub_bucket_id = tuple_hash(t + join_column_count, arity-join_column_count) % sub_bucket_per_bucket_count[bucket_id];

    //assert((int)bucket_id == mcomm.get_local_rank());

#if 0
    if (get_debug_id() == "rel_path_2_1")
    {
        std::cout << "Tuples to insert " << full << " ";
        for (u32 i=0; i < arity+1; i++)
            std::cout << t[i] << " ";
        std::cout << std::endl;
    }
#endif

    if (full[bucket_id].insert_tuple_from_array(t, arity+1) == true)
    {
        full_element_count++;
        full_bucket_element_count[bucket_id]++;
        full_sub_bucket_element_count[bucket_id][sub_bucket_id]++;
        bucket_map[bucket_id] = 1;

        //std::cout << "Full size " << full_element_count << std::endl;
        return true;
    }

    return false;
}




int relation::insert_delta_in_full()
{
    u32 insert_success = 0;
    u32 buckets = get_bucket_count();
    vector_buffer *input_buffer = new vector_buffer[buckets];

    for (u32 i = 0; i < buckets; i++)
    {
        input_buffer[i].vector_buffer_create_empty();
        if (bucket_map[i] == 1)
        {
            std::vector<u64> prefix = {};
            delta[i].as_vector_buffer_recursive(&(input_buffer[i]), prefix);
            for (u64 j = 0; j < (&input_buffer[i])->size / sizeof(u64); j=j+(arity+1))
            {
                if (insert_in_full ( (u64*)( (input_buffer[i].buffer) + (j*sizeof(u64)) )) == true)
                    insert_success++;
            }
            delta[i].remove_tuple();

            input_buffer[i].vector_buffer_free();
        }
    }

    set_delta_element_count(0);
    delete[] input_buffer;

    return insert_success;
}



int relation::insert_full_in_delta()
{
    u32 insert_success = 0;
    u32 buckets = get_bucket_count();
    vector_buffer *input_buffer = new vector_buffer[buckets];

    for (u32 i = 0; i < buckets; i++)
    {
        input_buffer[i].vector_buffer_create_empty();
        if (bucket_map[i] == 1)
        {
            std::vector<u64> prefix = {};
            full[i].as_vector_buffer_recursive(&(input_buffer[i]), prefix);
            for (u64 j = 0; j < (&input_buffer[i])->size / sizeof(u64); j=j+(arity+1))
            {
                if (insert_in_delta ( (u64*)( (input_buffer[i].buffer) + (j*sizeof(u64)) )) == true)
                    insert_success++;
            }
            full[i].remove_tuple();
            input_buffer[i].vector_buffer_free();
        }
    }

    set_full_element_count(0);
    delete[] input_buffer;

    return insert_success;
}



void relation::local_insert_in_delta()
{
    int rank;
    MPI_Comm_rank(mcomm.get_comm(), &rank);
    u32 buckets = get_bucket_count();

    // for (size_t i = 0; i < buckets; i++)
    // {
    //     delta[i].remove_tuple();
    // }
    delete[] delta;


    delta = newt;


    /*
    u32 i = mcomm.get_rank();
    vector_buffer *vb_newt = new vector_buffer[buckets];
    vb_newt[i].vector_buffer_create_empty();
    std::vector<u64> prefix = {};
    newt[i].as_vector_buffer_recursive(&(vb_newt[i]), prefix);

    if (i == 0)
        std::cout << "XX [" << get_debug_id() << "] Test " << mcomm.get_rank() << " DELTA " << vb_newt[i].size/(sizeof(u64) * (arity + 1)) << " arity " << arity + 1 << std::endl;

    vb_newt[i].vector_buffer_free();

    delete[] vb_newt;



    //u32 i = mcomm.get_rank();
    vector_buffer *vb_delta = new vector_buffer[buckets];
    vb_delta[i].vector_buffer_create_empty();
    //std::vector<u64> prefix = {};
    delta[i].as_vector_buffer_recursive(&(vb_delta[i]), prefix);

    if (i == 0)
        std::cout << "YY [" << get_debug_id() << "] Test " << mcomm.get_rank() << " DELTA " << vb_delta[i].size/(sizeof(u64) * (arity + 1)) << " arity " << arity + 1 << std::endl;

    vb_delta[i].vector_buffer_free();

    delete[] vb_delta;
    */


    delta_element_count = newt_element_count;
    //if (rank == 0)
    //    std::cout << "[" << get_debug_id() << "] copyng newt pointer to delta   " << delta_element_count << std::endl;

    memcpy(delta_bucket_element_count, newt_bucket_element_count, buckets * sizeof(u32));
    for (u32 b = 0; b < buckets; b++)
    {
        memcpy(delta_sub_bucket_element_count[b], newt_sub_bucket_element_count[b], sub_bucket_per_bucket_count[b] * sizeof(u32));
        memset(newt_sub_bucket_element_count[b], 0, sub_bucket_per_bucket_count[b] * sizeof(u32));
    }

#ifdef GOOGLE_MAP
    newt = new google_relation[buckets];
#else
    if (data_structure_type == BTREE)
    {
        newt = new btree_relation[buckets];
        for (size_t i=0; i < buckets; i++)
        {
            newt[i].arity = arity;
        }
    }
    else 
    {
        // newt = new trie_relation[buckets];
        // for (size_t i=0; i < buckets; i++)
        // {
        //     newt[i] = trie_relation(arity);
        // }
    }
#endif

    //for(u32 i=0; i<buckets; i++)
    //    newt[i] = new google_relation();
    newt_element_count = 0;
    memset(newt_bucket_element_count, 0, buckets * sizeof(u32));
}
