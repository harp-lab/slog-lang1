/*
 * Parallel IO of relations
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */


#include "../parallel_RA_inc.h"


parallel_io::parallel_io()
{
    col_count=0;
    entry_count=0;
    hash_buffer_size=0;
}

void parallel_io::parallel_read_input_relation_from_separate_files(u32 arity, std::string fname, MPI_Comm lcomm)
{
    int rank, nprocs;
    MPI_Comm_rank(lcomm, &rank);
    MPI_Comm_size(lcomm, &nprocs);
	file_name = fname;

    std::string data_filename;
    data_filename = file_name;

	uint64_t read_size = 0;
	hash_buffer_size = 0;

	FILE * fp;
    fp = fopen(data_filename.c_str(), "r");
	if (fp)
	{
		fseek(fp, 0, SEEK_END);
		read_size = ftell(fp);
		fseek(fp, 0, SEEK_SET);

	    hash_buffer_size = read_size/sizeof(u64);
		hash_buffer = new u64[hash_buffer_size];

		u32 rb_size = fread(hash_buffer, 1, read_size, fp);
		if (rb_size != read_size)
		{
			std::cout << data_filename <<  " Wrong IO: rank: " << rank << " " << rb_size << " " << read_size << std::endl;
			MPI_Abort(lcomm, -1);
		}
	}
	fclose(fp);
}


void parallel_io::parallel_read_input_relation_from_file_with_offset(u32 arity, std::string fname, MPI_Comm lcomm)
{
    int rank, nprocs;
    MPI_Comm_rank(lcomm, &rank);
    MPI_Comm_size(lcomm, &nprocs);
    file_name = fname;

    std::string offset_filename;
    offset_filename = file_name + ".offset";

    uint64_t offsets[nprocs];    /// the offset for each process
    uint64_t sizes[nprocs];      /// the size for each process

    if (rank == 0)
    {
		int a;
		uint64_t b, c;
		std::ifstream myfile (offset_filename);
		if (myfile.is_open())
		{
			while(myfile >> a >> b >> c)
			{
				offsets[a] = b;
				sizes[a] = c;
			}
			myfile.close();
		}
    }
    MPI_Bcast(offsets, nprocs, MPI_LONG_LONG, 0, lcomm);
    MPI_Bcast(sizes, nprocs, MPI_LONG_LONG, 0, lcomm);
    uint64_t read_offset = offsets[rank];
    uint64_t read_size = sizes[rank];

    hash_buffer_size = read_size/sizeof(u64);
    hash_buffer = new u64[hash_buffer_size];

    std::string data_filename;
    data_filename = file_name;
    if (share_io == true)
    {
    	MPI_Status stas;
    	MPI_File fp;
    	MPI_Info info;
    	MPI_Info_create(&info);
    	MPI_Info_set(info, "romio_cb_read" , "enable") ;
        MPI_File_open(lcomm, data_filename.c_str(), MPI_MODE_RDONLY, info, &fp);
    	MPI_File_read_at_all(fp, read_offset, hash_buffer, read_size, MPI_BYTE, &stas);
    	MPI_Info_free(&info);
    	MPI_File_close(&fp);
    }
    else
    {
        int fp = open(data_filename.c_str(), O_RDONLY);
		u32 rb_size = pread(fp, hash_buffer, read_size, read_offset);
		if (rb_size != read_size)
		{
			std::cout << data_filename <<  " Wrong IO: rank: " << rank << " " << rb_size << " " << read_size << " " << read_offset << std::endl;
			MPI_Abort(lcomm, -1);
		}
	    close(fp);
    }
}

void parallel_io::parallel_read_input_relation_from_file_to_local_buffer(u32 arity, std::string fname, MPI_Comm lcomm)
{
    int rank, nprocs;
    MPI_Comm_rank(lcomm, &rank);
    MPI_Comm_size(lcomm, &nprocs);
    file_name = fname;
    int global_row_count;

    /* Read the metadata file containing the total number of rows and columns */
    std::string meta_data_filename;
    meta_data_filename = file_name + ".size";

#if 0
    FILE *fp_in;
    fp_in = fopen(meta_data_filename, "r");
    if (fscanf (fp_in, "%d\n%d", &global_row_count, &col_count) != 2)
    {
        printf("Wrong input format (Meta Data)\n");
        MPI_Abort(lcomm, -1);
    }
    fclose(fp_in);
#endif

    std::string line1;
    std::string line2;
    if (rank == 0)
    {
        std::ifstream myfile (meta_data_filename.c_str());
        if (myfile.is_open())
        {
            getline (myfile,line1);
            global_row_count = std::stoi(line1);
            getline (myfile,line2);
            col_count = std::stoi(line2);
            myfile.close();
        }
    }


    /* Broadcast the total number of rows and column to all processes */
    MPI_Bcast(&global_row_count, 1, MPI_INT, 0, lcomm);
    MPI_Bcast(&col_count, 1, MPI_INT, 0, lcomm);


#if 1
    //if (rank == 1)
    //    std::cout << "Filename " << meta_data_filename << " Row Count " << global_row_count << " Column count " << col_count << std::endl;

    /* Read all data in parallel */
    uint64_t read_offset;
    read_offset = ceil((float)global_row_count / nprocs) * rank;

    if (read_offset > (uint64_t)global_row_count)
    {
        entry_count = 0;
        read_offset = 0;
    }
    else
    {
		if (read_offset + ceil((float)global_row_count / nprocs) > global_row_count)
			entry_count = global_row_count - read_offset;
		else
			entry_count = (int) ceil((float)global_row_count / nprocs);
    }

    assert((int)arity+1 == col_count);

    std::string data_filename;
    data_filename = file_name;

    //std::cout << "Buffer size " << entry_count << " " << col_count << std::endl;
	input_buffer = new u64[entry_count * col_count];
    uint64_t offset = read_offset * col_count * sizeof(u64);
    uint64_t read_size = entry_count * col_count * sizeof(u64);

    if (share_io == true)
    {
    	MPI_Status stas;
    	MPI_File fp;
    	MPI_Info info;
    	MPI_Info_create(&info);
    	MPI_Info_set(info, "romio_cb_read" , "enable") ;
        MPI_File_open(lcomm, data_filename.c_str(), MPI_MODE_RDONLY, info, &fp);
    	MPI_File_read_at_all(fp, offset, input_buffer, read_size, MPI_BYTE, &stas);
    	MPI_Info_free(&info);
    	MPI_File_close(&fp);
    }
    else    ///POSIX IO
    {
        int fp = open(data_filename.c_str(), O_RDONLY);
		u32 rb_size = pread(fp, input_buffer, read_size, offset);
		if (rb_size != read_size)
		{
			std::cout << data_filename <<  " Wrong IO: rank: " << rank << " " << rb_size << " " <<  entry_count << " " << col_count << " " << read_offset << std::endl;
			MPI_Abort(lcomm, -1);
		}
		close(fp);
    }

    //delete input_buffer;


    //u32 rb_g_size = 0;
    //MPI_Allreduce(&rb_size, &rb_g_size, 1, MPI_INT, MPI_SUM, lcomm);
    //if (rank == 0)
    //    std::cout << "Tuples in file " << file_name << " " << rb_g_size/(sizeof(u64) * (arity+1)) << std::endl;

#endif

    /*
    for (u32 u = 0; u < entry_count * col_count; u = u+col_count)
    {
        for (u32 v = 0; v < col_count; v++)
            std::cout << input_buffer[u+v] << " " ;
        std::cout << std::endl;
    }
    */


    return;
}



void parallel_io::buffer_data_to_hash_buffer_col(u32 arity, u32 join_column_count, u32 buckets, u32** sub_bucket_rank, u32* sub_bucket_count, MPI_Comm comm)
{

    int nprocs, rank;
    MPI_Comm_size(comm, &nprocs);
    MPI_Comm_rank(comm, &rank);

    /* process_size[i] stores the number of samples to be sent to process with rank i */
    int* process_size = new int[nprocs];
    memset(process_size, 0, nprocs * sizeof(int));

    /* process_data_vector[i] contains the data that needs to be sent to process i */
    vector_buffer* process_data_vector = (vector_buffer*)malloc(sizeof(vector_buffer) * nprocs);
    for (int i = 0; i < nprocs; i++)
        process_data_vector[i].vector_buffer_create_empty();

    int process_data_vector_size=0;
    /* Hashing and buffering data for all to all comm */
    u64 val[col_count];


    assert((int)arity+1 == col_count);
    for (int i = 0; i < entry_count * (col_count); i=i+(col_count))
    {
        uint64_t bucket_id = tuple_hash(input_buffer + i, join_column_count) % buckets;
        uint64_t sub_bucket_id = tuple_hash(input_buffer + i+ (join_column_count), arity+1-join_column_count) % sub_bucket_count[bucket_id];

        int index = sub_bucket_rank[bucket_id][sub_bucket_id];
        process_size[index] = process_size[index] + (col_count);

        for (int j = 0; j < col_count; j++)
        {
            val[j] = input_buffer[i + j];
            //std::cout << "V " << val[j] << " ";
        }
        //std::cout << std::endl;


        process_data_vector[index].vector_buffer_append((unsigned char *) val, sizeof(u64)*(col_count));
        process_data_vector_size = process_data_vector_size + (col_count);
    }


    /* Transmit the packaged data process_data_vector to all processes */
    all_to_all_comm(process_data_vector, process_data_vector_size, process_size, &hash_buffer_size, &hash_buffer, comm);

    //u32 g_hash_buffer_size = 0;
    //MPI_Allreduce(&hash_buffer_size, &g_hash_buffer_size, 1, MPI_INT, MPI_SUM, comm);
    //if (rank == 0)
    //    std::cout << "After Comm " << fname << " " << g_hash_buffer_size/((arity+1)) << std::endl;

    /* Free the data buffer after all to all */
    free (process_data_vector);

    /* Free the process size buffer */
    delete[] process_size;

    return;
}
