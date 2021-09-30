#include "../src/parallel_RA_inc.h"
#include <time.h>
#include <algorithm>
#include <iterator>
#include <bitset>
#include <random>
#include <math.h>
#include <chrono>

#define ITERATION_COUNT 25

static void uniform_benchmark(int ra_count, int nprocs, int epoch_count, u64 entry_count);
static void non_uniform_benchmark(int ra_count, int nprocs, u64 entry_count, int random_offset, int range);
static void all_to_allv_test(all_to_allv_buffer non_uniform_buffer, MPI_Comm comm, double* all_to_all_time, double* buffer_create_time, double* buffer_delete_time, double *all_to_allv_time, int it, int nprocs, u64 entry_count, int random_offset, int range);

static void complete_uniform_benchmark(int ra_count, int nprocs, u64 entry_count, int random_offset, int range);
static void three_phases_uniform_benchmark(int ra_count, int nprocs, u64 entry_count, int node_procs);
static void uniform_ptp_benchmark(int ra_count, int nprocs, u64 entry_count);

static void basic_bruck_uniform_benchmark(char *sendbuf, int sendcount, MPI_Datatype sendtype, char *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm);
static void datatype_bruck_uniform_benchmark(char *sendbuf, int sendcount, MPI_Datatype sendtype, char *recvbuf, int recvcount, MPI_Datatype recvtype,  MPI_Comm comm);
static void modified_basic_bruck_uniform_benchmark(char *sendbuf, int sendcount, MPI_Datatype sendtype, char *recvbuf, int recvcount, MPI_Datatype recvtype,  MPI_Comm comm);
static void modified_dt_bruck_uniform_benchmark(char *sendbuf, int sendcount, MPI_Datatype sendtype, char *recvbuf, int recvcount, MPI_Datatype recvtype,  MPI_Comm comm);
static void noRotation_bruck_uniform_benchmark(char *sendbuf, int sendcount, MPI_Datatype sendtype, char *recvbuf, int recvcount, MPI_Datatype recvtype,  MPI_Comm comm);
static void zerocopy_bruck_uniform_benchmark(char *sendbuf, int sendcount, MPI_Datatype sendtype, char *recvbuf, int recvcount, MPI_Datatype recvtype,  MPI_Comm comm);
static void zeroCopyRot_bruck_uniform_benchmark(char *sendbuf, int sendcount, MPI_Datatype sendtype, char *recvbuf, int recvcount, MPI_Datatype recvtype,  MPI_Comm comm);
static void rot_mod_naive_bruck_uniform_benchmark(char *sendbuf, int sendcount, MPI_Datatype sendtype, char *recvbuf, int recvcount, MPI_Datatype recvtype,  MPI_Comm comm);

static void padded_bruck_non_uniform_benchmark(int dist, int range, char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm);
static void padded_alltoall_non_uniform_benchmark(int dist, int range, char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm);
static void datatype_bruck_non_uniform_benchmark(int range, char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm);
static void modified_dt_bruck_non_uniform_benchmark(int range, char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm);
static void zeroCopy_bruck_non_uniform_benchmark(int range, char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm);
static void twophase_non_uniform_benchmark(int dist, int range, char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm);
static void ptp_non_uniform_benchmark(char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm);
static void new_twophase_non_uniform_benchmark(int dist, int range, char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm);
static void onephase_non_uniform_benchmark(int dist, int range, char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm);

static void run_non_uniform(int nprocs, int dist);
static void run_uniform(int nprocs);

int main(int argc, char **argv)
{
    mpi_comm mcomm;
    mcomm.create(argc, argv);
    srand (time(NULL));
    u32 ra_count = 1;

    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    int nprocs = mcomm.get_nprocs();

//	if (rank == 0)
//		std::cout << "******************** Uniform Experiments ********************\n";
//	run_uniform(nprocs);

    // Random Distribution Scheme
	if (rank == 0)
		std::cout << "******************** Random Distribution Scheme ********************\n";
	run_non_uniform(nprocs, 0);

    // Random Distribution Scheme
	if (rank == 0)
		std::cout << "******************** Random Distribution Scheme ********************\n";
	run_non_uniform(nprocs, 0);

    // Gaussian Normal Distribution Scheme
//	if (rank == 0)
//		std::cout << "**************** Gaussian Normal Distribution Scheme **************\n";
//    run_non_uniform(nprocs, 1);
//
//    // Power Law Distribution Scheme
//	if (rank == 0)
//		std::cout << "********************* Power Law Distribution Scheme ****************\n";
//    run_non_uniform(nprocs, 2);


    mcomm.destroy();
    return 0;
}


static void run_non_uniform(int nprocs, int dist)
{
    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    int range = 100;
    int random_offset = 100 - range;

	for (int entry_count=2; entry_count <= 4096; entry_count=entry_count*2)
	{
		int sendcounts[nprocs]; // the size of data each process send to other process
		memset(sendcounts, 0, nprocs*sizeof(int));
		int sdispls[nprocs];
		int soffset = 0;

		// Uniform random distribution
		if (dist == 0)
		{
			srand(time(NULL));
			for (int i=0; i < nprocs; i++)
			{
				int random = random_offset + rand() % range;
				sendcounts[i] = (entry_count * random) / 100;
			}
		}

		// Gausian normal distribution
		if (dist == 1)
		{
			std::default_random_engine generator;
			std::normal_distribution<double> distribution(nprocs/2, nprocs/3); // set mean and deviation

			while(true)
			{
				int p = int(distribution(generator));
				if (p >= 0 && p < nprocs)
				{
					if (++sendcounts[p] >= entry_count) break;
				}
			}
		}

		// Power law distribution
		if (dist == 2)
		{
			double x = (double)entry_count;

			for (int i=0; i<nprocs; ++i)
			{
				sendcounts[i] = (int)x;
				x = x * 0.999;
			}
		}

		// Random shuffling the sentcounts array
		unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
		std::shuffle(&sendcounts[0], &sendcounts[nprocs], std::default_random_engine(seed));


		// Initial send offset array
		for (int i=0; i<nprocs; ++i)
		{
			sdispls[i] = soffset;
			soffset += sendcounts[i];
		}

		// Initial receive counts and offset array
		int recvcounts[nprocs];
		MPI_Alltoall(sendcounts, 1, MPI_INT, recvcounts, 1, MPI_INT, MPI_COMM_WORLD);
		int rdispls[nprocs];
		int roffset = 0;
		for (int i=0; i < nprocs; i++)
		{
			rdispls[i] = roffset;
			roffset += recvcounts[i];
		}

		// Initial send buffer
//		u64* send_buffer = new u64[soffset];
//		u64* recv_buffer = new u64[roffset];
		char* send_buffer = (char*)malloc(soffset*sizeof(char));
		char* recv_buffer = (char*)malloc(roffset*sizeof(char));
		int scounts[nprocs]; // a copy of sendcounts for each iteration

		// MPI_alltoallv
		for (int it=0; it < ITERATION_COUNT; it++)
		{
			int index = 0;
			for (int i=0; i < nprocs; i++)
			{
				for (int j = 0; j < sendcounts[i]; j++)
					send_buffer[index++] = 'A' + (random() % 26);
//					send_buffer[index++] = i + rank * 10;
			}

			double comm_start = MPI_Wtime();
			MPI_Alltoallv(send_buffer, sendcounts, sdispls, MPI_CHAR, recv_buffer, recvcounts, rdispls, MPI_CHAR, MPI_COMM_WORLD);
			double comm_end = MPI_Wtime();

			double max_time = 0;
			double total_time = comm_end - comm_start;
			MPI_Allreduce(&total_time, &max_time, 1, MPI_DOUBLE, MPI_MAX, MPI_COMM_WORLD);
			if (total_time == max_time)
				std::cout << "[MPIAlltoallv]" << " [" << dist << " " << nprocs << " " << range << " " << entry_count << "] "<<  max_time << std::endl;
		}

		MPI_Barrier(MPI_COMM_WORLD);
		if (rank == 0)
			std::cout << "----------------------------------------------------------------" << std::endl<< std::endl;

		// Padded All-to-all algorithm
		for (int it=0; it < ITERATION_COUNT; it++)
		{
			int index = 0;
			for (int i=0; i < nprocs; i++)
			{
				for (int j = 0; j < sendcounts[i]; j++)
					send_buffer[index++] = 'A' + (random() % 26);
			}
			padded_alltoall_non_uniform_benchmark(dist, 0, (char*)send_buffer, sendcounts, sdispls, MPI_CHAR, (char*)recv_buffer, recvcounts, rdispls, MPI_CHAR, MPI_COMM_WORLD);
		}

		MPI_Barrier(MPI_COMM_WORLD);
		if (rank == 0)
			std::cout << "----------------------------------------------------------------" << std::endl<< std::endl;

		// Padded Bruck algorithm
		for (int it=0; it < ITERATION_COUNT; it++)
		{
			int index = 0;
			for (int i=0; i < nprocs; i++)
			{
				for (int j = 0; j < sendcounts[i]; j++)
					send_buffer[index++] = 'A' + (random() % 26);
			}
			padded_bruck_non_uniform_benchmark(dist, 0, (char*)send_buffer, sendcounts, sdispls, MPI_CHAR, (char*)recv_buffer, recvcounts, rdispls, MPI_CHAR, MPI_COMM_WORLD);
		}

		MPI_Barrier(MPI_COMM_WORLD);
		if (rank == 0)
			std::cout << "----------------------------------------------------------------" << std::endl<< std::endl;


		// two-phase algorithm
//		for (int it=0; it < ITERATION_COUNT; it++)
//		{
//			memcpy(&scounts, &sendcounts, nprocs*sizeof(int));
//
//			int index = 0;
//			for (int i=0; i < nprocs; i++)
//			{
//				for (int j = 0; j < sendcounts[i]; j++)
//					send_buffer[index++] = i + rank * 10;
//			}
//			twophase_non_uniform_benchmark(dist, 0, (char*)send_buffer, scounts, sdispls, MPI_UNSIGNED_LONG_LONG, (char*)recv_buffer, recvcounts, rdispls, MPI_UNSIGNED_LONG_LONG, MPI_COMM_WORLD);
//		}
//
//		MPI_Barrier(MPI_COMM_WORLD);
//		if (rank == 0)
//			std::cout << "----------------------------------------------------------------" << std::endl<< std::endl;


		// new two-phase algorithm
//		for (int it=0; it < ITERATION_COUNT; it++)
//		{
//			memcpy(&scounts, &sendcounts, nprocs*sizeof(int));
//
//			int index = 0;
//			for (int i=0; i < nprocs; i++)
//			{
//				for (int j = 0; j < sendcounts[i]; j++)
//					send_buffer[index++] = i + rank * 10;
//			}
//			new_twophase_non_uniform_benchmark(dist, 0, (char*)send_buffer, scounts, sdispls, MPI_UNSIGNED_LONG_LONG, (char*)recv_buffer, recvcounts, rdispls, MPI_UNSIGNED_LONG_LONG, MPI_COMM_WORLD);
//		}


//		MPI_Barrier(MPI_COMM_WORLD);
//		if (rank == 0)
//			std::cout << "----------------------------------------------------------------" << std::endl<< std::endl;

//		// one phase Bruck
//		for (int it=0; it < ITERATION_COUNT; it++)
//		{
//			memcpy(&scounts, &sendcounts, nprocs*sizeof(int));
//			int index = 0;
//			for (int i=0; i < nprocs; i++)
//			{
//				for (int j = 0; j < sendcounts[i]; j++)
//					send_buffer[index++] = i + rank * 10;
//			}
//			onephase_non_uniform_benchmark(dist, 0, (char*)send_buffer, scounts, sdispls, MPI_UNSIGNED_LONG_LONG, (char*)recv_buffer, recvcounts, rdispls, MPI_UNSIGNED_LONG_LONG, MPI_COMM_WORLD);
//		}
//
//		MPI_Barrier(MPI_COMM_WORLD);
//		if (rank == 0)
//			std::cout << "----------------------------------------------------------------" << std::endl<< std::endl;

//		if (rank == 1)
//		{
//			for(int i = 0; i < roffset; i++)
//				std::cout << recv_buffer[i] << "\n";
//		}

		free(send_buffer);
		free(recv_buffer);
//		delete[] send_buffer;
//		delete[] recv_buffer;

	}
}


static void run_uniform(int nprocs)
{
    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

	// Initial send buffer
    for (u64 entry_count=2; entry_count <= 4096; entry_count=entry_count*2)
    {
		u64* send_buffer = new u64[entry_count*nprocs];
		for (int i=0; i<entry_count*nprocs; i++)
		{
			u64 value = i/entry_count + rank * 10;
			send_buffer[i] = value;
		}

		u64* recv_buffer = new u64[entry_count*nprocs];

		for (int it=0; it < ITERATION_COUNT; it++)
			basic_bruck_uniform_benchmark((char*)send_buffer, entry_count, MPI_UNSIGNED_LONG_LONG, (char*)recv_buffer, entry_count, MPI_UNSIGNED_LONG_LONG, MPI_COMM_WORLD);

		MPI_Barrier(MPI_COMM_WORLD);
		if (rank == 0)
			std::cout << "----------------------------------------------------------------" << std::endl<< std::endl;

		for (int it=0; it < ITERATION_COUNT; it++)
			datatype_bruck_uniform_benchmark((char*)send_buffer, entry_count, MPI_UNSIGNED_LONG_LONG, (char*)recv_buffer, entry_count, MPI_UNSIGNED_LONG_LONG, MPI_COMM_WORLD);

		MPI_Barrier(MPI_COMM_WORLD);
		if (rank == 0)
			std::cout << "----------------------------------------------------------------" << std::endl<< std::endl;

		for (int it=0; it < ITERATION_COUNT; it++)
			modified_basic_bruck_uniform_benchmark((char*)send_buffer, entry_count, MPI_UNSIGNED_LONG_LONG, (char*)recv_buffer, entry_count, MPI_UNSIGNED_LONG_LONG, MPI_COMM_WORLD);

		MPI_Barrier(MPI_COMM_WORLD);
		if (rank == 0)
			std::cout << "----------------------------------------------------------------" << std::endl<< std::endl;

		for (int it=0; it < ITERATION_COUNT; it++)
			modified_dt_bruck_uniform_benchmark((char*)send_buffer, entry_count, MPI_UNSIGNED_LONG_LONG, (char*)recv_buffer, entry_count, MPI_UNSIGNED_LONG_LONG, MPI_COMM_WORLD);

		MPI_Barrier(MPI_COMM_WORLD);
		if (rank == 0)
			std::cout << "----------------------------------------------------------------" << std::endl<< std::endl;

		for (int it=0; it < ITERATION_COUNT; it++)
			noRotation_bruck_uniform_benchmark((char*)send_buffer, entry_count, MPI_UNSIGNED_LONG_LONG, (char*)recv_buffer, entry_count, MPI_UNSIGNED_LONG_LONG, MPI_COMM_WORLD);

		MPI_Barrier(MPI_COMM_WORLD);
		if (rank == 0)
			std::cout << "----------------------------------------------------------------" << std::endl<< std::endl;

		for (int it=0; it < ITERATION_COUNT; it++)
			zerocopy_bruck_uniform_benchmark((char*)send_buffer, entry_count, MPI_UNSIGNED_LONG_LONG, (char*)recv_buffer, entry_count, MPI_UNSIGNED_LONG_LONG, MPI_COMM_WORLD);


		delete[] send_buffer;
		delete[] recv_buffer;
    }
}



static void ptp_non_uniform_benchmark(char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm)
{
	int rank, nprocs;
	MPI_Comm_rank(comm, &rank);
	MPI_Comm_size(comm, &nprocs);

	int typesize;
	MPI_Type_size(sendtype, &typesize);

	MPI_Request* req = (MPI_Request*)malloc(2*nprocs*sizeof(MPI_Request));
	MPI_Status* stat = (MPI_Status*)malloc(2*nprocs*sizeof(MPI_Status));
	for (int i = 0; i < nprocs; i++)
	{
		int src = (rank + i) % nprocs; // avoid always to reach first master node
		MPI_Irecv(&recvbuf[rdispls[src]*typesize], recvcounts[src]*typesize, MPI_CHAR, src, 0, comm, &req[i]);
	}

	for (int i = 0; i < nprocs; i++)
	{
		int dst = (rank - i + nprocs) % nprocs;
		MPI_Isend(&sendbuf[sdispls[dst]*typesize], sendcounts[dst]*typesize, MPI_CHAR, dst, 0, comm, &req[i+nprocs]);
	}
	MPI_Waitall(2*nprocs, req, stat);
	free(req);
	free(stat);
}

static void padded_alltoall_non_uniform_benchmark(int dist, int range, char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm)
{
	double u_start = MPI_Wtime();

	int rank, nprocs;
	MPI_Comm_rank(comm, &rank);
	MPI_Comm_size(comm, &nprocs);

	int typesize;
	MPI_Type_size(sendtype, &typesize);

	// 1. Find max send count
	double find_count_start = MPI_Wtime();
	int local_max_count = 0;
	for (int i = 0; i < nprocs; i++)
	{
		if (sendcounts[i] > local_max_count)
			local_max_count = sendcounts[i];
	}
	int max_send_count = 0;
	MPI_Allreduce(&local_max_count, &max_send_count, 1, MPI_INT, MPI_MAX, comm);
	double find_count_end = MPI_Wtime();
	double find_count_time = find_count_end - find_count_start;

	// 2. local padding
	double copy_start = MPI_Wtime();
	char* temp_send_buffer = (char*)malloc(max_send_count*nprocs*typesize);
	for (int i = 0; i < nprocs; i++)
		memcpy(&temp_send_buffer[i*max_send_count*typesize], &sendbuf[sdispls[i]*typesize], sendcounts[i]*typesize);
	double copy_end = MPI_Wtime();
	double copy_time = copy_end - copy_start;

	// 3. all-to-all communication
	double comm_start = MPI_Wtime();
	char* temp_recv_buffer = (char*)malloc(max_send_count*nprocs*typesize);
	MPI_Alltoall(temp_send_buffer, max_send_count*typesize, MPI_CHAR, temp_recv_buffer, max_send_count*typesize, MPI_CHAR, comm);
	free(temp_send_buffer);
	double comm_end = MPI_Wtime();
	double comm_time = (comm_end - comm_start);

	// 4. filter
	double filter_start = MPI_Wtime();
	int offset = 0;
	for (int i = 0; i < nprocs; i++)
		memcpy(&recvbuf[rdispls[i]*typesize], &temp_recv_buffer[i*max_send_count*typesize], recvcounts[i]*typesize);
	free(temp_recv_buffer);
	double filter_end = MPI_Wtime();
	double filter_time = filter_end - filter_start;

	double u_end = MPI_Wtime();
	double max_u_time = 0;
	double total_u_time = u_end - u_start;
	MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, comm);
	if (total_u_time == max_u_time)
	{
		 std::cout << "[PaddedAlltoall] ["  << dist << " " << nprocs << " " << range << " " << max_send_count << "] " << total_u_time << " " << find_count_time << " " << copy_time << " "
				 << comm_time << " " << filter_time << std::endl;
	}
}

static void padded_bruck_non_uniform_benchmark(int dist, int range, char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm)
{
	double u_start = MPI_Wtime();

	int rank, nprocs;
	MPI_Comm_rank(comm, &rank);
	MPI_Comm_size(comm, &nprocs);

	int typesize;
	MPI_Type_size(sendtype, &typesize);

	// 1. Find max send count
	double find_count_start = MPI_Wtime();
	int local_max_count = 0;
	for (int i = 0; i < nprocs; i++)
	{
		if (sendcounts[i] > local_max_count)
			local_max_count = sendcounts[i];
	}
	int max_send_count = 0;
	MPI_Allreduce(&local_max_count, &max_send_count, 1, MPI_INT, MPI_MAX, comm);
	double find_count_end = MPI_Wtime();
	double find_count_time = find_count_end - find_count_start;

	// 2. local rotation
	double rotation_start = MPI_Wtime();
	char* temp_send_buffer = (char*)malloc(max_send_count*nprocs*typesize);
	memset(temp_send_buffer, 0, max_send_count*nprocs*typesize);
	int offset = 0;
	for (int i = 0; i < nprocs; i++)
	{
		int index = (i - rank + nprocs) % nprocs;
		memcpy(&temp_send_buffer[index*max_send_count*typesize], &sendbuf[offset], sendcounts[i]*typesize);
		offset += sendcounts[i]*typesize;
	}
	double rotation_end = MPI_Wtime();
	double rotation_time = rotation_end - rotation_start;

	// 3. exchange data with log(P) steps
	double exchange_start =  MPI_Wtime();
	u64 unit_size = max_send_count * typesize;
	double total_find_blocks_time = 0, total_copy_time = 0, total_comm_time = 0, total_replace_time = 0;
	char* temp_buffer = (char*)malloc(max_send_count*typesize*((nprocs+1)/2));
	char* temp_recv_buffer = (char*)malloc(max_send_count*typesize*((nprocs+1)/2));
	for (int k = 1; k < nprocs; k <<= 1)
	{
		// 1) find which data blocks to send
		double find_blocks_start = MPI_Wtime();
		int send_indexes[(nprocs+1)/2];
		int sendb_num = 0;
		for (int i = k; i < nprocs; i++)
		{
			if (i & k)
				send_indexes[sendb_num++] = i;
		}
		double find_blocks_end = MPI_Wtime();
		total_find_blocks_time += (find_blocks_end - find_blocks_start);

		// 2) copy blocks which need to be sent at this step
		double copy_start = MPI_Wtime();
		for (int i = 0; i < sendb_num; i++)
		{
			u64 offset = send_indexes[i] * unit_size;
			memcpy(temp_buffer+(i*unit_size), temp_send_buffer+offset, unit_size);
		}
		double copy_end = MPI_Wtime();
		total_copy_time += (copy_end - copy_start);

		// 3) send and receive
		double comm_start = MPI_Wtime();
		int recv_proc = (rank - k + nprocs) % nprocs; // receive data from rank - 2^step process
		int send_proc = (rank + k) % nprocs; // send data from rank + 2^k process
		u64 comm_size = sendb_num * unit_size;
		MPI_Sendrecv(temp_buffer, comm_size, MPI_CHAR, send_proc, 0, temp_recv_buffer, comm_size, MPI_CHAR, recv_proc, 0, comm, MPI_STATUS_IGNORE);
		double comm_end = MPI_Wtime();
		total_comm_time += (comm_end - comm_start);

		// 4) replace with received data
		double replace_start = MPI_Wtime();
		for (int i = 0; i < sendb_num; i++)
		{
			u64 offset = send_indexes[i] * unit_size;
			memcpy(temp_send_buffer+offset, temp_recv_buffer+(i*unit_size), unit_size);
		}
		double replace_end = MPI_Wtime();
		total_replace_time += (replace_end - replace_start);
	}
	free(temp_buffer);
	free(temp_recv_buffer);
	double exchange_end = MPI_Wtime();
	double exchange_time = exchange_end - exchange_start;

	// 4. second rotation
	double revs_rotation_start = MPI_Wtime();
	offset = 0;
	for (int i = 0; i < nprocs; i++)
	{
		int index = (rank - i + nprocs) % nprocs;
		memcpy(&recvbuf[rdispls[index]*typesize], &temp_send_buffer[i*unit_size], recvcounts[index]*typesize);
	}
	free(temp_send_buffer);
	double revs_rotation_end = MPI_Wtime();
	double revs_rotation_time = revs_rotation_end - revs_rotation_start;

	double u_end = MPI_Wtime();
	double max_u_time = 0;
	double total_u_time = u_end - u_start;
	MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, comm);
	if (total_u_time == max_u_time)
	{
		 std::cout << "[PaddedBruck] ["  << dist << " " << nprocs << " " << range << " " << max_send_count << "] " << total_u_time << " " << find_count_time << " " << rotation_time << " "
				 << exchange_time << " [" << total_find_blocks_time << " " << total_copy_time << " " << total_comm_time << " " << total_replace_time << "] "<< revs_rotation_time << std::endl;
	}
}


static void datatype_bruck_non_uniform_benchmark(int range, char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm)
{
	double u_start = MPI_Wtime();

	int rank, nprocs;
	MPI_Comm_rank(comm, &rank);
	MPI_Comm_size(comm, &nprocs);

	int typesize;
	MPI_Type_size(sendtype, &typesize);

	// 1. Find max send count
	double find_count_start = MPI_Wtime();
	int local_max_count = 0;
	for (int i = 0; i < nprocs; i++)
	{
		if (sendcounts[i] > local_max_count)
			local_max_count = sendcounts[i];
	}
	int max_send_count = 0;
	MPI_Allreduce(&local_max_count, &max_send_count, 1, MPI_INT, MPI_MAX, comm);
	double find_count_end = MPI_Wtime();
	double find_count_time = find_count_end - find_count_start;

	// 2. local rotation
	double rotation_start = MPI_Wtime();
	char* temp_send_buffer = (char*)malloc(max_send_count*nprocs*typesize);
	char* temp_recv_buffer = (char*)malloc(max_send_count*typesize*nprocs);
	memset(temp_send_buffer, 0, max_send_count*nprocs*typesize);
	int offset = 0;
	for (int i = 0; i < nprocs; i++)
	{
		int index = (i - rank + nprocs) % nprocs;
		memcpy(&temp_send_buffer[index*max_send_count*typesize], &sendbuf[offset], sendcounts[i]*typesize);
		offset += sendcounts[i]*typesize;
	}
	double rotation_end = MPI_Wtime();
	double rotation_time = rotation_end - rotation_start;

	// 3. exchange data with log(P) steps
	double exchange_start =  MPI_Wtime();
	u64 unit_size = max_send_count * typesize;
	double total_create_dt_time=0, total_replace_time=0, total_comm_time=0;
	for (int k = 1; k < nprocs; k <<= 1)
	{
		// 1) create data type
		double create_datatype_start = MPI_Wtime();
		int displs[(nprocs+1)/2];
		int sendb_num = 0;
		for (int i = 1; i < nprocs; i++)
		{
			if (i & k)
				displs[sendb_num++] = i*unit_size;
		}
		MPI_Datatype send_type;
		MPI_Type_create_indexed_block(sendb_num, unit_size, displs, MPI_CHAR, &send_type);
		MPI_Type_commit(&send_type);
		double create_datatype_end = MPI_Wtime();
		total_create_dt_time += create_datatype_end - create_datatype_start;

		// 2) exchange data
		double comm_start = MPI_Wtime();
		int recv_proc = (rank - k + nprocs) % nprocs; // receive data from rank - 2^step process
		int send_proc = (rank + k) % nprocs; // send data from rank + 2^k process
		MPI_Sendrecv(temp_send_buffer, 1, send_type, send_proc, 0, temp_recv_buffer, 1, send_type, recv_proc, 0, comm, MPI_STATUS_IGNORE);
		MPI_Type_free(&send_type);
		double comm_end = MPI_Wtime();
		total_comm_time += (comm_end - comm_start);

		// 3) replace time
		double replace_start = MPI_Wtime();
		for (int i = 0; i < sendb_num; i++)
			memcpy(temp_send_buffer+displs[i], temp_recv_buffer+displs[i], unit_size);
		double replace_end = MPI_Wtime();
		total_replace_time += (replace_end - replace_start);
	}
	free(temp_recv_buffer);
	double exchange_end = MPI_Wtime();
	double exchange_time = exchange_end - exchange_start;

	// 4. second rotation
	double revs_rotation_start = MPI_Wtime();
	offset = 0;
	for (int i = 0; i < nprocs; i++)
	{
		int index = (rank - i + nprocs) % nprocs;
		memcpy(&recvbuf[rdispls[index]*typesize], &temp_send_buffer[i*unit_size], recvcounts[index]*typesize);
	}
	free(temp_send_buffer);
	double revs_rotation_end = MPI_Wtime();
	double revs_rotation_time = revs_rotation_end - revs_rotation_start;

	double u_end = MPI_Wtime();
	double max_u_time = 0;
	double total_u_time = u_end - u_start;
	MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, comm);
	if (total_u_time == max_u_time)
	{
		 std::cout << "[DTBruckNoN] ["  << nprocs << " " << range << " " << max_send_count << "] " << total_u_time << " " << find_count_time << " " << rotation_time << " "
				 << exchange_time << " [" << total_create_dt_time << " " << total_comm_time << " " << total_replace_time << "] "<< revs_rotation_time << std::endl;
	}
}


static void modified_dt_bruck_non_uniform_benchmark(int range, char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm)
{
	double u_start = MPI_Wtime();

	int rank, nprocs;
	MPI_Comm_rank(comm, &rank);
	MPI_Comm_size(comm, &nprocs);

	int typesize;
	MPI_Type_size(sendtype, &typesize);

	// 1. Find max send count
	double find_count_start = MPI_Wtime();
	int local_max_count = 0;
	for (int i = 0; i < nprocs; i++)
	{
		if (sendcounts[i] > local_max_count)
			local_max_count = sendcounts[i];
	}
	int max_send_count = 0;
	MPI_Allreduce(&local_max_count, &max_send_count, 1, MPI_INT, MPI_MAX, comm);
	double find_count_end = MPI_Wtime();
	double find_count_time = find_count_end - find_count_start;

	// 2. local rotation
	double rotation_start = MPI_Wtime();
	char* temp_send_buffer = (char*)malloc(max_send_count*nprocs*typesize);
	char* temp_recv_buffer = (char*)malloc(max_send_count*typesize*nprocs);
	memset(temp_send_buffer, 0, max_send_count*nprocs*typesize);
	int offset = 0;
	for (int i = 0; i < nprocs; i++)
	{
		int index = (2*rank-i+nprocs)%nprocs;
		memcpy(&temp_send_buffer[index*max_send_count*typesize], &sendbuf[offset], sendcounts[i]*typesize);
		offset += sendcounts[i]*typesize;
	}
	double rotation_end = MPI_Wtime();
	double rotation_time = rotation_end - rotation_start;

    // 3. exchange data with log(P) steps
    double exchange_start =  MPI_Wtime();
    u64 unit_size = max_send_count * typesize;
 	double total_create_dt_time=0, total_comm_time=0, total_copy_time=0;
 	for (int k = 1; k < nprocs; k <<= 1)
 	{
 		// 1) create data type
		double create_datatype_start = MPI_Wtime();
		int displs[(nprocs+1)/2];
		int sendb_num = 0;
		for (int i = 1; i < nprocs; i++)
		{
			if (i & k)
				displs[sendb_num++] = ((rank+i)%nprocs)*unit_size;
		}
		MPI_Datatype send_type;
		MPI_Type_create_indexed_block(sendb_num, unit_size, displs, MPI_CHAR, &send_type);
		MPI_Type_commit(&send_type);
		double create_datatype_end = MPI_Wtime();
		total_create_dt_time += create_datatype_end - create_datatype_start;

		// 2) exchange data
		double comm_start = MPI_Wtime();
		int recv_proc = (rank + k) % nprocs; // receive data from rank + 2^k process
		int send_proc = (rank - k + nprocs) % nprocs; // send data from rank - 2^k process
		MPI_Sendrecv(temp_send_buffer, 1, send_type, send_proc, 0, temp_recv_buffer, 1, send_type, recv_proc, 0, comm, MPI_STATUS_IGNORE);
		MPI_Type_free(&send_type);
		double comm_end = MPI_Wtime();
		total_comm_time += (comm_end - comm_start);

		// 3) copy time
		double copy_start = MPI_Wtime();
		for (int i = 0; i < sendb_num; i++)
			memcpy(temp_send_buffer+displs[i], temp_recv_buffer+displs[i], unit_size);
		double copy_end = MPI_Wtime();
		total_copy_time += (copy_end - copy_start);
 	}
 	free(temp_recv_buffer);
 	double exchange_end = MPI_Wtime();
 	double exchange_time = exchange_end - exchange_start;

	// 4. remove padding
	double filter_start = MPI_Wtime();
	for (int i=0; i < nprocs; i++)
		memcpy(&recvbuf[rdispls[i]*typesize], &temp_send_buffer[i*max_send_count*typesize], recvcounts[i]*typesize);
 	free(temp_send_buffer);
	double filter_end = MPI_Wtime();
	double filter_time = filter_end - filter_start;

	double u_end = MPI_Wtime();
	double max_u_time = 0;
	double total_u_time = u_end - u_start;
	MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, comm);
	if (total_u_time == max_u_time)
	{
		 std::cout << "[ModDtBruckNoN] ["  << nprocs << " " << range << " " << max_send_count << "] " << total_u_time << " " << find_count_time << " " << rotation_time << " "
				 << exchange_time << " [" << total_create_dt_time << " " << total_comm_time << " " << total_copy_time << "] "<< filter_time << std::endl;
	}
}


static void zeroCopy_bruck_non_uniform_benchmark(int range, char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm)
{
	double u_start = MPI_Wtime();

	int rank, nprocs;
	MPI_Comm_rank(comm, &rank);
	MPI_Comm_size(comm, &nprocs);

	int typesize;
	MPI_Type_size(sendtype, &typesize);

	// 1. Find max send count
	double find_count_start = MPI_Wtime();
	int local_max_count = 0;
	for (int i = 0; i < nprocs; i++)
	{
		if (sendcounts[i] > local_max_count)
			local_max_count = sendcounts[i];
	}
	int max_send_count = 0;
	MPI_Allreduce(&local_max_count, &max_send_count, 1, MPI_INT, MPI_MAX, comm);
	double find_count_end = MPI_Wtime();
	double find_count_time = find_count_end - find_count_start;

	// 2. local rotation
	double rotation_start = MPI_Wtime();
	char* temp_send_buffer = (char*)malloc(max_send_count*nprocs*typesize);
	char* temp_recv_buffer = (char*)malloc(max_send_count*typesize*nprocs);
	memset(temp_send_buffer, 0, max_send_count*nprocs*typesize);
	int offset = 0;
	for (int i = 0; i < nprocs; i++)
	{
		int index = (2*rank-i+nprocs)%nprocs;
		memcpy(&temp_send_buffer[index*max_send_count*typesize], &sendbuf[offset], sendcounts[i]*typesize);
		offset += sendcounts[i]*typesize;
	}
	memcpy(temp_recv_buffer, temp_send_buffer, max_send_count*typesize*nprocs);
	double rotation_end = MPI_Wtime();
	double rotation_time = rotation_end - rotation_start;

	// 3. exchange data with log(P) steps
	double exchange_start =  MPI_Wtime();

	int bits[nprocs];
	bits[0] = 0; // the number of bits k' > k
	for (int j = 1; j < nprocs; j++) bits[j] = bits[j>>1]+(j&0x1);

	int commblocks[nprocs];
	MPI_Datatype commtypes[nprocs];
	MPI_Aint recvindex[nprocs];
	MPI_Aint sendindex[nprocs];

	char* inter_buffer = (char*)malloc(max_send_count*typesize*nprocs);
	u64 unit_size = max_send_count * typesize;
	double total_create_dt_time = 0, total_comm_time = 0;
	unsigned int mask = 0xFFFFFFFF;
	int b, j;
	for (int k = 1; k < nprocs; k <<= 1)
	{
		// 1) create struct data-type that send and receive data from two buffers
		double create_datatype_start = MPI_Wtime();
		b = 0; j = k;
		while (j < nprocs)
		{
			int index = (rank+j)%nprocs;
			commblocks[b] = unit_size;
			commtypes[b] = MPI_CHAR;

			if ((bits[j&mask]&0x1)==0x1) // send to recvbuf when the number of bits k' > k is odd
			{
				recvindex[b] = (MPI_Aint)((char*)temp_recv_buffer+index*unit_size);

				if ((j & mask) == j) // recv from sendbuf when the number of bits k' > k is even
					sendindex[b] = (MPI_Aint)((char*)temp_send_buffer+index*unit_size);
				else // from intermediate buffer
					sendindex[b] = (MPI_Aint)((char*)inter_buffer+index*unit_size);
			}
			else // send to intermediate buffer
			{
				recvindex[b] = (MPI_Aint)((char*)inter_buffer+index*unit_size);

				if ((j & mask) == j) // recv from sendbuf when the number of bits k' > k is even
					sendindex[b] = (MPI_Aint)((char*)temp_send_buffer+index*unit_size);
				else
					sendindex[b] = (MPI_Aint)((char*)temp_recv_buffer+index*unit_size);
			}
			b++;
			j++; if ((j & k) != k) j += k; // data blocks whose kth bit is 1
		}

		MPI_Datatype sendblocktype;
		MPI_Type_create_struct(b, commblocks, sendindex, commtypes, &sendblocktype);
		MPI_Type_commit(&sendblocktype);
		MPI_Datatype recvblocktype;
		MPI_Type_create_struct(b,commblocks, recvindex, commtypes, &recvblocktype);
		MPI_Type_commit(&recvblocktype);
		double create_datatype_end = MPI_Wtime();
		total_create_dt_time += (create_datatype_end - create_datatype_start);

		// 2) exchange data
		double comm_start = MPI_Wtime();
		int sendrank = (rank - k + nprocs) % nprocs;
		int recvrank = (rank + k) % nprocs;
		MPI_Sendrecv(MPI_BOTTOM, 1, sendblocktype, sendrank, 0, MPI_BOTTOM, 1, recvblocktype, recvrank, 0, comm, MPI_STATUS_IGNORE);

		MPI_Type_free(&recvblocktype);
		MPI_Type_free(&sendblocktype);
		double comm_end = MPI_Wtime();
		total_comm_time += (comm_end - comm_start);

		mask <<= 1;
	}
	free(inter_buffer);
	free(temp_send_buffer);
	double exchange_end =  MPI_Wtime();
	double exchange_time = exchange_end - exchange_start;

	// 4. remove padding
	double filter_start = MPI_Wtime();
	for (int i=0; i < nprocs; i++)
		memcpy(&recvbuf[rdispls[i]*typesize], &temp_recv_buffer[i*max_send_count*typesize], recvcounts[i]*typesize);
 	free(temp_recv_buffer);
	double filter_end = MPI_Wtime();
	double filter_time = filter_end - filter_start;

	double u_end = MPI_Wtime();
	double max_u_time = 0;
	double total_u_time = u_end - u_start;
	MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, comm);
	if (total_u_time == max_u_time)
	{
		 std::cout << "[ZeroCopyBruckNoN] ["  << nprocs << " " << range << " " << max_send_count << "] " << total_u_time << " " << find_count_time << " " << rotation_time << " "
				 << exchange_time << " [" << total_create_dt_time << " " << total_comm_time << "] "<< filter_time << std::endl;
	}
}


static void onephase_non_uniform_benchmark(int dist, int range, char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm)
{
	int rank, nprocs;
	MPI_Comm_rank(comm, &rank);
	MPI_Comm_size(comm, &nprocs);

	int typesize;
	MPI_Type_size(sendtype, &typesize);

	// find maximum count of data-blocks
	double u_start = MPI_Wtime();
	double find_count_start = MPI_Wtime();
	int local_max_count = 0;
	for (int i = 0; i < nprocs; i++)
	{
		if (sendcounts[i] > local_max_count)
			local_max_count = sendcounts[i];
	}
	int max_send_count = 0;
	MPI_Allreduce(&local_max_count, &max_send_count, 1, MPI_INT, MPI_MAX, comm);
	double find_count_end = MPI_Wtime();
	double find_count_time = find_count_end - find_count_start;

	// gather meta-data
	double metadata_start = MPI_Wtime();
	int* metadata = (int*) malloc(nprocs*nprocs*sizeof(int));
	MPI_Allgather(sendcounts, nprocs, MPI_INT, metadata, nprocs, MPI_INT, comm);
	double metadata_end = MPI_Wtime();
	double metadata_gather_time = metadata_end - metadata_start;

	// create rotation array for all the processes
	double rotation_start = MPI_Wtime();
	int* rotate_index_array = (int*)malloc(nprocs*nprocs*sizeof(int));
	for (int j = 0; j < nprocs; j++)
	{
		for (int i = 0; i < nprocs; i++)
			rotate_index_array[j*nprocs+i] = (2*j-i+nprocs)%nprocs;
	}
	double rotation_end = MPI_Wtime();
	double rotation_time = rotation_end - rotation_start;

	// communication steps
	double exchange_start = MPI_Wtime();
	int max_send_elements = (nprocs+1)/2;
	int send_indice[max_send_elements];
	int metadata_recv[max_send_elements];
	char* extra_buffer = (char*) malloc(max_send_count*typesize*nprocs);
	char* temp_send_buffer = (char*) malloc(max_send_count*typesize*max_send_elements);
	char* temp_recv_buffer = (char*) malloc(max_send_count*typesize*max_send_elements);
	int pos_status[nprocs];
	memset(pos_status, 0, nprocs*sizeof(int));
	double total_find_blocks_time=0, total_pre_time=0, total_comm_time=0, total_replace_time=0, total_update_time=0;
	int its = log2(nprocs)+1;
	double find_blocks_time[its];
	double pre_time[its];
	double comm_time[its];
	double replace_time[its];
	double update_time[its];

	int it = 0;
	for (int k = 1; k < nprocs; k <<= 1)
	{
		// find the indices of sending data-blocks
		double find_blocks_start = MPI_Wtime();
		int n = 0, i = k;
		while(i < nprocs)
		{
			send_indice[n++] = i;
			i++; if ((i & k) != k) i += k;
		}
		double find_blocks_end = MPI_Wtime();
		find_blocks_time[it] = (find_blocks_end - find_blocks_start);
		total_find_blocks_time += (find_blocks_end - find_blocks_start);

		// prepare sending data-blocks
		double pre_start = MPI_Wtime();
		int offset = 0;
		for (int i = 0; i < n; i++)
		{
			int d = (rank+send_indice[i])%nprocs;
			int send_index = rotate_index_array[rank*nprocs+d];
			int meta = metadata[rank*nprocs+send_index];

			if (pos_status[send_index] == 0)
				memcpy(&temp_send_buffer[offset], &sendbuf[sdispls[send_index]*typesize], meta*typesize);
			else
				memcpy(&temp_send_buffer[offset], &extra_buffer[d*max_send_count*typesize], meta*typesize);

			offset += meta*typesize;
		}

		int sendrank = (rank - k + nprocs) % nprocs;  // the process which we send data-blocks to
		int recvrank = (rank + k) % nprocs; // the process which we receive data-blocks from

		// find the correct meta-data in order
		int total_count = 0;
		for(int i = 0; i < n; i++)
		{
			int d = (recvrank + send_indice[i])%nprocs;
			int recv_index = rotate_index_array[recvrank*nprocs+d];
			metadata_recv[i] = metadata[recvrank*nprocs+recv_index];
			total_count += metadata_recv[i];
		}
		double pre_end = MPI_Wtime();
		pre_time[it] = (pre_end - pre_start);
		total_pre_time += (pre_end - pre_start);

		// exchange data-blocks
		double comm_start = MPI_Wtime();
		MPI_Sendrecv(temp_send_buffer, offset, MPI_CHAR, sendrank, 1, temp_recv_buffer, total_count*typesize, MPI_CHAR, recvrank, 1, comm, MPI_STATUS_IGNORE);
		double comm_end = MPI_Wtime();
		total_comm_time += (comm_end - comm_start);

		// organize received data-blocks
		double replace_start = MPI_Wtime();
		offset = 0;
		for (int i = 0; i < n; i++)
		{
			int d = (rank+send_indice[i])%nprocs;
			int send_index = rotate_index_array[rank*nprocs+d];

			if (send_indice[i] < (k << 1))
				memcpy(&recvbuf[rdispls[d]*typesize], &temp_recv_buffer[offset], metadata_recv[i]*typesize);
			else
				memcpy(&extra_buffer[d*max_send_count*typesize], &temp_recv_buffer[offset], metadata_recv[i]*typesize);

			offset += metadata_recv[i]*typesize;
			pos_status[send_index] = 1;
		}
		double replace_end = MPI_Wtime();
		replace_time[it] = (replace_end - replace_start);
		total_replace_time += (replace_end - replace_start);

		// update meta-data for all the processes
		double update_start = MPI_Wtime();
		for (int i = 0; i < n; i++)
		{
			int update_metadata[nprocs];
			for (int p = 0; p < nprocs; p++)
			{
				int d = (p + send_indice[i])%nprocs;
				int index = rotate_index_array[p*nprocs+d];
				update_metadata[p] = metadata[p*nprocs+index];
			}

			for (int p = 0; p < nprocs; p++)
			{
				int d = (p + send_indice[i])%nprocs;
				int index = rotate_index_array[p*nprocs+d];
				metadata[p*nprocs+index] = update_metadata[(p+k)%nprocs];
			}
		}
		double update_end = MPI_Wtime();
		update_time[it] = (update_end - update_start);
		total_update_time += (update_end - update_start);

		it++;
	}
	free(temp_send_buffer);
	free(temp_recv_buffer);
	free(extra_buffer);
	free(rotate_index_array);
	free(metadata);

	memcpy(&recvbuf[rdispls[rank]*typesize], &sendbuf[sdispls[rank]*typesize], recvcounts[rank]*typesize);

	double exchange_end = MPI_Wtime();
	double exchange_time = (exchange_end - exchange_start);

    double u_end = MPI_Wtime();
	double max_u_time = 0;
	double total_u_time = u_end - u_start;
	MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, comm);
	if (total_u_time == max_u_time)
	{
		 std::cout << "[OnePhase] [" << dist << " " << nprocs << " " << range << " " << max_send_count << "] " << total_u_time << " " << find_count_time << " " << metadata_gather_time << " " << rotation_time
				 << " " << exchange_time << " [" << total_find_blocks_time << " " << total_pre_time << " " << total_comm_time << " " << total_replace_time << " " << total_update_time << "] " << std::endl;
		 for (int i = 0; i < it; i++)
		 {
			 std::cout << i << ": [" << find_blocks_time[i] << " " << pre_time[i] << " " << comm_time[i] <<
					 " " << replace_time[i] << " " << update_time[i] << "] \n";;
		 }
	}
}

static void new_twophase_non_uniform_benchmark(int dist, int range, char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm)
{
	int rank, nprocs;
	MPI_Comm_rank(comm, &rank);
	MPI_Comm_size(comm, &nprocs);

	int typesize;
	MPI_Type_size(sendtype, &typesize);

	// 1. Find max send count
	double u_start = MPI_Wtime();
	double find_count_start = MPI_Wtime();
	int local_max_count = 0;
	for (int i = 0; i < nprocs; i++)
	{
		if (sendcounts[i] > local_max_count)
			local_max_count = sendcounts[i];
	}
	int max_send_count = 0;
	MPI_Allreduce(&local_max_count, &max_send_count, 1, MPI_INT, MPI_MAX, comm);
	double find_count_end = MPI_Wtime();
	double find_count_time = find_count_end - find_count_start;

	// 2. create local index array after rotation
	double create_rindex_start = MPI_Wtime();
	int rotate_index_array[nprocs];
	for (int i = 0; i < nprocs; i++)
		rotate_index_array[i] = (2*rank-i+nprocs)%nprocs;
	double create_rindex_end = MPI_Wtime();
	double create_rindex_time = create_rindex_end - create_rindex_start;

	// 3. exchange data with log(P) steps
	double exchange_start = MPI_Wtime();
	int max_send_elements = (nprocs+1)/2;
	char* extra_buffer = (char*) malloc(max_send_count*typesize*nprocs);
	char* temp_send_buffer = (char*) malloc(max_send_count*typesize*max_send_elements);
	char* temp_recv_buffer = (char*) malloc(max_send_count*typesize*max_send_elements);
	int pos_status[nprocs];
	memset(pos_status, 0, nprocs*sizeof(int));
	double total_find_blocks_time = 0, total_pre_time = 0, total_send_meda_time = 0, total_comm_time = 0, total_replace_time = 0;

	memcpy(&recvbuf[rdispls[rank]*typesize], &sendbuf[sdispls[rank]*typesize], recvcounts[rank]*typesize);

	for (int k = 1; k < nprocs; k <<= 1)
	{
		// 1) find which data blocks to send
		double find_blocks_start = MPI_Wtime();
		int send_indexes[max_send_elements];
		int sendb_num = 0;
		for (int i = k; i < nprocs; i++)
		{
			if (i & k)
				send_indexes[sendb_num++] = (rank+i)%nprocs;
		}

		double find_blocks_end = MPI_Wtime();
		total_find_blocks_time += (find_blocks_end - find_blocks_start);

		// 2) prepare metadata and send buffer
		double pre_send_start = MPI_Wtime();
		int metadata_send[sendb_num];
		int sendCount = 0;
		int offset = 0;
		for (int i = 0; i < sendb_num; i++)
		{
			int send_index = rotate_index_array[send_indexes[i]];
			metadata_send[i] = sendcounts[send_index];
			if (pos_status[send_index] == 0)
				memcpy(&temp_send_buffer[offset], &sendbuf[sdispls[send_index]*typesize], sendcounts[send_index]*typesize);
			else
				memcpy(&temp_send_buffer[offset], &extra_buffer[send_indexes[i]*max_send_count*typesize], sendcounts[send_index]*typesize);
			offset += sendcounts[send_index]*typesize;
		}
		double pre_send_end = MPI_Wtime();
		total_pre_time += pre_send_end - pre_send_start;


		// 3) exchange metadata
		double send_meda_start = MPI_Wtime();
		int sendrank = (rank - k + nprocs) % nprocs;
		int recvrank = (rank + k) % nprocs;
		int metadata_recv[sendb_num];
		MPI_Sendrecv(metadata_send, sendb_num, MPI_INT, sendrank, 0, metadata_recv, sendb_num, MPI_INT, recvrank, 0, comm, MPI_STATUS_IGNORE);
		double send_meda_end = MPI_Wtime();
		total_send_meda_time += (send_meda_end - send_meda_start);

		for(int i = 0; i < sendb_num; i++)
			sendCount += metadata_recv[i];

		// 4) exchange data
		double comm_start = MPI_Wtime();
		MPI_Sendrecv(temp_send_buffer, offset, MPI_CHAR, sendrank, 1, temp_recv_buffer, sendCount*typesize, MPI_CHAR, recvrank, 1, comm, MPI_STATUS_IGNORE);
		double comm_end = MPI_Wtime();
		total_comm_time = (comm_end - comm_start);

		// 5) replace
		double replace_start = MPI_Wtime();
		offset = 0;
		for (int i = 0; i < sendb_num; i++)
		{
			int send_index = rotate_index_array[send_indexes[i]];

			if ((send_indexes[i] - rank + nprocs) % nprocs < (k << 1))
				memcpy(&recvbuf[rdispls[send_indexes[i]]*typesize], &temp_recv_buffer[offset], metadata_recv[i]*typesize);
			else
				memcpy(&extra_buffer[send_indexes[i]*max_send_count*typesize], &temp_recv_buffer[offset], metadata_recv[i]*typesize);

			offset += metadata_recv[i]*typesize;
			pos_status[send_index] = 1;
			sendcounts[send_index] = metadata_recv[i];
		}
		double replace_end = MPI_Wtime();
		total_replace_time += (replace_end - replace_start);
	}
	free(temp_send_buffer);
	free(temp_recv_buffer);
	free(extra_buffer);


	double exchange_end = MPI_Wtime();
	double exchange_time = (exchange_end - exchange_start);

    double u_end = MPI_Wtime();
	double max_u_time = 0;
	double total_u_time = u_end - u_start;
	MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, comm);
	if (total_u_time == max_u_time)
	{
		 std::cout << "[NewTwoPhase] [" << dist << " " << nprocs << " " << range << " " << max_send_count << "] " << total_u_time << " " << find_count_time << " " << create_rindex_time << " " << exchange_time << " ["
				 << total_find_blocks_time << " " << total_pre_time << " " << total_send_meda_time << " " << total_comm_time << " " << total_replace_time << "] " << std::endl;
	}
}


static void twophase_non_uniform_benchmark(int dist, int range, char *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype sendtype, char *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype recvtype, MPI_Comm comm)
{
	int rank, nprocs;
	MPI_Comm_rank(comm, &rank);
	MPI_Comm_size(comm, &nprocs);

	int typesize;
	MPI_Type_size(sendtype, &typesize);

	// 1. Find max send count
	double u_start = MPI_Wtime();
	double find_count_start = MPI_Wtime();
	int local_max_count = 0;
	for (int i = 0; i < nprocs; i++)
	{
		if (sendcounts[i] > local_max_count)
			local_max_count = sendcounts[i];
	}
	int max_send_count = 0;
	MPI_Allreduce(&local_max_count, &max_send_count, 1, MPI_INT, MPI_MAX, comm);
	double find_count_end = MPI_Wtime();
	double find_count_time = find_count_end - find_count_start;

	// 2. create local index array after rotation
	double create_rindex_start = MPI_Wtime();
	int rotate_index_array[nprocs];
    for (int i = 0; i < nprocs; i++)
    	rotate_index_array[i] = (2*rank-i+nprocs)%nprocs;
	double create_rindex_end = MPI_Wtime();
	double create_rindex_time = create_rindex_end - create_rindex_start;

	// 3. exchange data with log(P) steps
	double exchange_start = MPI_Wtime();
	int max_send_elements = (nprocs+1)/2;
	char* extra_buffer = (char*) malloc(max_send_count*typesize*nprocs);
	char* temp_send_buffer = (char*) malloc(max_send_count*typesize*max_send_elements);
	char* temp_recv_buffer = (char*) malloc(max_send_count*typesize*max_send_elements);
	int pos_status[nprocs];
	memset(pos_status, 0, nprocs*sizeof(int));
	double total_find_blocks_time = 0, total_pre_time = 0, total_send_meda_time = 0, total_comm_time = 0, total_replace_time = 0;
	for (int k = 1; k < nprocs; k <<= 1)
	{
    	// 1) find which data blocks to send
    	double find_blocks_start = MPI_Wtime();
    	int send_indexes[max_send_elements];
    	int sendb_num = 0;
		for (int i = 1; i < nprocs; i++)
		{
			if (i & k)
				send_indexes[sendb_num++] = (rank+i)%nprocs;
		}
		double find_blocks_end = MPI_Wtime();
		total_find_blocks_time += (find_blocks_end - find_blocks_start);

		// 2) prepare metadata and send buffer
		double pre_send_start = MPI_Wtime();
		int metadata_send[sendb_num+1];

		int offset = 0;
		for (int i = 0; i < sendb_num; i++)
		{
			int send_index = rotate_index_array[send_indexes[i]];
			metadata_send[i] = sendcounts[send_index];
			if (pos_status[send_index] == 0)
				memcpy(&temp_send_buffer[offset], &sendbuf[sdispls[send_index]*typesize], sendcounts[send_index]*typesize);
			else
				memcpy(&temp_send_buffer[offset], &extra_buffer[send_indexes[i]*max_send_count*typesize], sendcounts[send_index]*typesize);
			offset += sendcounts[send_index]*typesize;
		}
		double pre_send_end = MPI_Wtime();
		total_pre_time += pre_send_end - pre_send_start;

		// 3) exchange metadata
		double send_meda_start = MPI_Wtime();
		int sendrank = (rank - k + nprocs) % nprocs;
		int recvrank = (rank + k) % nprocs;
		int metadata_recv[sendb_num];
		MPI_Sendrecv(metadata_send, sendb_num, MPI_INT, sendrank, 0, metadata_recv, sendb_num, MPI_INT, recvrank, 0, comm, MPI_STATUS_IGNORE);
		double send_meda_end = MPI_Wtime();
		total_send_meda_time += (send_meda_end - send_meda_start);

		int sendCount = 0;
		for (int i = 0; i < sendb_num; i++)
			sendCount += metadata_recv[i];

		// 4) exchange data
		double comm_start = MPI_Wtime();
		MPI_Sendrecv(temp_send_buffer, offset, MPI_CHAR, sendrank, 1, temp_recv_buffer, sendCount*typesize, MPI_CHAR, recvrank, 1, comm, MPI_STATUS_IGNORE);
		double comm_end = MPI_Wtime();
		total_comm_time = (comm_end - comm_start);

		// 5) replace
		double replace_start = MPI_Wtime();
		offset = 0;
		for (int i = 0; i < sendb_num; i++)
		{
			int send_index = rotate_index_array[send_indexes[i]];
			memcpy(&extra_buffer[send_indexes[i]*max_send_count*typesize], &temp_recv_buffer[offset], metadata_recv[i]*typesize);
			offset += metadata_recv[i]*typesize;
			pos_status[send_index] = 1;
			sendcounts[send_index] = metadata_recv[i];
		}
		double replace_end = MPI_Wtime();
		total_replace_time += (replace_end - replace_start);
	}
	free(temp_send_buffer);
	free(temp_recv_buffer);
	double exchange_end = MPI_Wtime();
	double exchange_time = (exchange_end - exchange_start);

	double filter_start = MPI_Wtime();
	for (int i = 0; i < nprocs; i++)
	{
		if (rank == i)
			memcpy(&recvbuf[rdispls[i]*typesize], &sendbuf[sdispls[i]*typesize], recvcounts[i]*typesize);
		else
			memcpy(&recvbuf[rdispls[i]*typesize], &extra_buffer[i*max_send_count*typesize], recvcounts[i]*typesize);
	}
	free(extra_buffer);
	double filter_end = MPI_Wtime();
	double filter_time = filter_end - filter_start;

    double u_end = MPI_Wtime();
	double max_u_time = 0;
	double total_u_time = u_end - u_start;
	MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, comm);
	if (total_u_time == max_u_time)
	{
		 std::cout << "[TwoPhase] [" << dist << " " << nprocs << " " << range << " " << max_send_count << "] " << total_u_time << " " << find_count_time << " " << create_rindex_time << " " << exchange_time << " ["
				 << total_find_blocks_time << " " << total_pre_time << " " << total_send_meda_time << " " << total_comm_time << " " << total_replace_time << "] " << filter_time << std::endl;
	}
}

// naive Bruck (without any datatype)
static void basic_bruck_uniform_benchmark(char *sendbuf, int sendcount, MPI_Datatype sendtype, char *recvbuf, int recvcount, MPI_Datatype recvtype,  MPI_Comm comm)
{
	double u_start = MPI_Wtime();

    int rank, nprocs;
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &nprocs);

    int typesize;
    MPI_Type_size(sendtype, &typesize);

    u64 unit_size = sendcount * typesize;
    u64 local_size = sendcount * nprocs * typesize;

	// 1. local rotation
	double rotation_start = MPI_Wtime();
	memcpy(recvbuf, sendbuf, local_size);
	for (int i = 0; i < nprocs; i++)
	{
		int index = (i - rank + nprocs) % nprocs;
		memcpy(&sendbuf[index*unit_size], &recvbuf[i*unit_size], unit_size);
	}
    double rotation_end = MPI_Wtime();
    double rotation_time = rotation_end - rotation_start;

    // 2. exchange data with log(P) steps
    double exchange_start =  MPI_Wtime();
    double total_find_blocks_time = 0, total_copy_time = 0, total_comm_time = 0, total_replace_time = 0;
    char* temp_buffer = (char*)malloc(local_size);
    for (int k = 1; k < nprocs; k <<= 1)
    {
    	// 1) find which data blocks to send
    	double find_blocks_start = MPI_Wtime();
    	int send_indexes[(nprocs+1)/2];
    	int sendb_num = 0;
		for (int i = 1; i < nprocs; i++)
		{
			if (i & k)
				send_indexes[sendb_num++] = i;
		}
		double find_blocks_end = MPI_Wtime();
		total_find_blocks_time += (find_blocks_end - find_blocks_start);

		// 2) copy blocks which need to be sent at this step
		double copy_start = MPI_Wtime();
		for (int i = 0; i < sendb_num; i++)
		{
			u64 offset = send_indexes[i] * unit_size;
			memcpy(temp_buffer+(i*unit_size), sendbuf+offset, unit_size);
		}
		double copy_end = MPI_Wtime();
		total_copy_time += (copy_end - copy_start);

		// 3) send and receive
		double comm_start = MPI_Wtime();
		int recv_proc = (rank - k + nprocs) % nprocs; // receive data from rank - 2^step process
		int send_proc = (rank + k) % nprocs; // send data from rank + 2^k process
		u64 comm_size = sendb_num * unit_size;
		MPI_Sendrecv(temp_buffer, comm_size, MPI_CHAR, send_proc, 0, recvbuf, comm_size, MPI_CHAR, recv_proc, 0, comm, MPI_STATUS_IGNORE);
		double comm_end = MPI_Wtime();
		total_comm_time += (comm_end - comm_start);

		// 4) replace with received data
		double replace_start = MPI_Wtime();
		for (int i = 0; i < sendb_num; i++)
		{
			u64 offset = send_indexes[i] * unit_size;
			memcpy(sendbuf+offset, recvbuf+(i*unit_size), unit_size);
		}
		double replace_end = MPI_Wtime();
		total_replace_time += (replace_end - replace_start);
    }
    free(temp_buffer);
    double exchange_end = MPI_Wtime();
    double exchange_time = exchange_end - exchange_start;

    // 3. second rotation
	double revs_rotation_start = MPI_Wtime();
	for (int i = 0; i < nprocs; i++)
	{
		int index = (rank - i + nprocs) % nprocs;
		memcpy(&recvbuf[index*unit_size], &sendbuf[i*unit_size], unit_size);
	}
    double revs_rotation_end = MPI_Wtime();
    double revs_rotation_time = revs_rotation_end - revs_rotation_start;

    double u_end = MPI_Wtime();
	double max_u_time = 0;
	double total_u_time = u_end - u_start;
	MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, comm);
	if (total_u_time == max_u_time)
	{
		 std::cout << "[BasicBruck] ["  << nprocs << " " << sendcount << "] " << total_u_time << " " << rotation_time << " " << exchange_time << " ["
				 << total_find_blocks_time << " " << total_copy_time << " " << total_comm_time << " " << total_replace_time
				 << "] " << revs_rotation_time << std::endl;
	}
}


// create datatype
static void datatype_bruck_uniform_benchmark(char *sendbuf, int sendcount, MPI_Datatype sendtype, char *recvbuf, int recvcount, MPI_Datatype recvtype,  MPI_Comm comm)
{
	double u_start = MPI_Wtime();

    int rank, nprocs;
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &nprocs);

    int typesize;
    MPI_Type_size(sendtype, &typesize);

    u64 unit_size = sendcount * typesize;
    u64 local_size = sendcount * nprocs * typesize;

	// 1. local rotation
	double rotation_start = MPI_Wtime();
	memcpy(recvbuf, sendbuf, local_size);
	for (int i = 0; i < nprocs; i++)
	{
		int index = (i - rank + nprocs) % nprocs;
		memcpy(&sendbuf[index*unit_size], &recvbuf[i*unit_size], unit_size);
	}
    double rotation_end = MPI_Wtime();
    double rotation_time = rotation_end - rotation_start;

    // 2. exchange data with log(P) steps
    double exchange_start =  MPI_Wtime();
    double total_create_dt_time=0, total_replace_time=0, total_comm_time=0;
    for (int k = 1; k < nprocs; k <<= 1)
    {
		// 1) create data type
    	double create_datatype_start = MPI_Wtime();
		int displs[(nprocs+1)/2];
		int sendb_num = 0;
		for (int i = 1; i < nprocs; i++)
		{
			if (i & k)
				displs[sendb_num++] = i*unit_size;
		}
    	MPI_Datatype send_type;
//    	MPI_Type_vector(block_count, k*unit_size, (k<<1)*unit_size, MPI_CHAR, &vector_type);
    	MPI_Type_create_indexed_block(sendb_num, unit_size, displs, MPI_CHAR, &send_type);
    	MPI_Type_commit(&send_type);
    	int packsize;
    	MPI_Pack_size(1, send_type, MPI_COMM_WORLD, &packsize);
    	double create_datatype_end = MPI_Wtime();
    	total_create_dt_time += create_datatype_end - create_datatype_start;

    	// 2) exchange data
    	double comm_start = MPI_Wtime();
    	int recv_proc = (rank - k + nprocs) % nprocs; // receive data from rank - 2^step process
    	int send_proc = (rank + k) % nprocs; // send data from rank + 2^k process
    	MPI_Sendrecv(sendbuf, 1, send_type, send_proc, 0, recvbuf, packsize, MPI_PACKED, recv_proc, 0, comm, MPI_STATUS_IGNORE);
    	double comm_end = MPI_Wtime();
		total_comm_time += (comm_end - comm_start);

		// 3) replace time
		double replace_start = MPI_Wtime();
		int pos = 0;
		MPI_Unpack(recvbuf, packsize, &pos, sendbuf, 1, send_type, comm);
    	MPI_Type_free(&send_type);
//		for (int i = 0; i < sendb_num; i++)
//			memcpy(sendbuf+displs[i], recvbuf+displs[i], unit_size);
		double replace_end = MPI_Wtime();
		total_replace_time += (replace_end - replace_start);
    }
	double exchange_end = MPI_Wtime();
	double exchange_time = exchange_end - exchange_start;

	// 3. second rotation
	double revs_rotation_start = MPI_Wtime();
	for (int i = 0; i < nprocs; i++)
	{
		int index = (rank - i + nprocs) % nprocs;
		memcpy(&recvbuf[index*unit_size], &sendbuf[i*unit_size], unit_size);
	}
	double revs_rotation_end = MPI_Wtime();
	double revs_rotation_time = revs_rotation_end - revs_rotation_start;

    double u_end = MPI_Wtime();
	double max_u_time = 0;
	double total_u_time = u_end - u_start;
	MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, comm);
	if (total_u_time == max_u_time)
	{
		 std::cout << "[DTBruck] [" << nprocs << " " << sendcount << "] " << total_u_time << " " << rotation_time << " " << exchange_time << " ["
				 << total_create_dt_time << " " << total_comm_time << " " << total_replace_time << "] " << revs_rotation_time << std::endl;
	}
}


static void modified_basic_bruck_uniform_benchmark(char *sendbuf, int sendcount, MPI_Datatype sendtype, char *recvbuf, int recvcount, MPI_Datatype recvtype,  MPI_Comm comm)
{
	double u_start = MPI_Wtime();

	int rank, nprocs;
	MPI_Comm_rank(comm, &rank);
	MPI_Comm_size(comm, &nprocs);

	int typesize;
	MPI_Type_size(sendtype, &typesize);

	u64 unit_size = sendcount * typesize;
	u64 local_size = sendcount * nprocs * typesize;

    // 1. local rotation
    double rotation_start = MPI_Wtime();
    for (int i = 0; i < nprocs; i++)
    {
    	int index = (2*rank - i + nprocs) % nprocs;
    	memcpy(&recvbuf[index*unit_size], &sendbuf[i*unit_size], unit_size);
    }
    double rotation_end = MPI_Wtime();
    double rotation_time = rotation_end - rotation_start;

    // 2. exchange data with log(P) steps
    double exchange_start =  MPI_Wtime();
	double total_find_blocks_time = 0, total_copy_time = 0, total_comm_time = 0, total_replace_time = 0;
	char* temp_buffer = (char*)malloc(local_size);
	for (int k = 1; k < nprocs; k <<= 1)
	{
    	// 1) find which data blocks to send
    	double find_blocks_start = MPI_Wtime();
    	int send_indexes[(nprocs+1)/2];
    	int sendb_num = 0;
		for (int i = 1; i < nprocs; i++)
		{
			if (i & k)
				send_indexes[sendb_num++] = (rank+i) % nprocs;
		}
		double find_blocks_end = MPI_Wtime();
		total_find_blocks_time += (find_blocks_end - find_blocks_start);

		// 2) copy blocks which need to be sent at this step
		double copy_start = MPI_Wtime();
		for (int i = 0; i < sendb_num; i++)
		{
			u64 offset = send_indexes[i] * unit_size;
			memcpy(temp_buffer+(i*unit_size), recvbuf+offset, unit_size);
		}
		double copy_end = MPI_Wtime();
		total_copy_time += (copy_end - copy_start);

		// 3) send and receive
		double comm_start = MPI_Wtime();
		int recv_proc = (rank + k) % nprocs; // receive data from rank + 2^k process
		int send_proc = (rank - k + nprocs) % nprocs; // send data from rank - 2^k process
		u64 comm_size = sendb_num * unit_size;
		MPI_Sendrecv(temp_buffer, comm_size, MPI_CHAR, send_proc, 0, sendbuf, comm_size, MPI_CHAR, recv_proc, 0, comm, MPI_STATUS_IGNORE);
		double comm_end = MPI_Wtime();
		total_comm_time += (comm_end - comm_start);

		// 4) replace with received data
		double replace_start = MPI_Wtime();
		for (int i = 0; i < sendb_num; i++)
		{
			u64 offset = send_indexes[i] * unit_size;
			memcpy(recvbuf+offset, sendbuf+(i*unit_size), unit_size);
		}
		double replace_end = MPI_Wtime();
		total_replace_time += (replace_end - replace_start);
	}
	free(temp_buffer);
	double exchange_end = MPI_Wtime();
	double exchange_time = exchange_end - exchange_start;

    double u_end = MPI_Wtime();
	double max_u_time = 0;
	double total_u_time = u_end - u_start;
	MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, comm);
	if (total_u_time == max_u_time)
	{
		 std::cout << "[ModNaiveBruck] ["  << nprocs << " " << sendcount << "] " << total_u_time << " " << rotation_time << " " << exchange_time << " ["
				 << total_find_blocks_time << " " << total_copy_time << " " << total_comm_time << " " << total_replace_time << "] " << std::endl;
	}
}


static void rot_mod_naive_bruck_uniform_benchmark(char *sendbuf, int sendcount, MPI_Datatype sendtype, char *recvbuf, int recvcount, MPI_Datatype recvtype,  MPI_Comm comm)
{
	double u_start = MPI_Wtime();

	int rank, nprocs;
	MPI_Comm_rank(comm, &rank);
	MPI_Comm_size(comm, &nprocs);

	int typesize;
	MPI_Type_size(sendtype, &typesize);

	u64 unit_size = sendcount * typesize;
	u64 local_size = sendcount * nprocs * typesize;

    // 1. local rotation
    double rotation_start = MPI_Wtime();
    int rotate_index_array[nprocs];
    for (int i = 0; i < nprocs; i++)
    	rotate_index_array[i] = (2*rank - i + nprocs) % nprocs;
    double rotation_end = MPI_Wtime();
    double rotation_time = rotation_end - rotation_start;

    // 2. exchange data with log(P) steps
    double exchange_start =  MPI_Wtime();
	double total_find_blocks_time = 0, total_copy_time = 0, total_comm_time = 0, total_replace_time = 0;
	char* temp_buffer = (char*)malloc(local_size);
	for (int k = 1; k < nprocs; k <<= 1)
	{
    	// 1) find which data blocks to send
    	double find_blocks_start = MPI_Wtime();
    	int send_indexes[(nprocs+1)/2];
    	int sendb_num = 0;
		for (int i = 1; i < nprocs; i++)
		{
			if (i & k)
				send_indexes[sendb_num++] = (rank+i) % nprocs;
		}
		double find_blocks_end = MPI_Wtime();
		total_find_blocks_time += (find_blocks_end - find_blocks_start);

		// 2) copy blocks which need to be sent at this step
		double copy_start = MPI_Wtime();
		for (int i = 0; i < sendb_num; i++)
		{
			u64 offset = rotate_index_array[send_indexes[i]] * unit_size;
			memcpy(temp_buffer+(i*unit_size), recvbuf+offset, unit_size);
		}
		double copy_end = MPI_Wtime();
		total_copy_time += (copy_end - copy_start);

		// 3) send and receive
		double comm_start = MPI_Wtime();
		int recv_proc = (rank + k) % nprocs; // receive data from rank + 2^k process
		int send_proc = (rank - k + nprocs) % nprocs; // send data from rank - 2^k process
		u64 comm_size = sendb_num * unit_size;
		MPI_Sendrecv(temp_buffer, comm_size, MPI_CHAR, send_proc, 0, sendbuf, comm_size, MPI_CHAR, recv_proc, 0, comm, MPI_STATUS_IGNORE);
		double comm_end = MPI_Wtime();
		total_comm_time += (comm_end - comm_start);

		// 4) replace with received data
		double replace_start = MPI_Wtime();
		for (int i = 0; i < sendb_num; i++)
		{
			u64 offset = rotate_index_array[send_indexes[i]] * unit_size;
			memcpy(recvbuf+offset, sendbuf+(i*unit_size), unit_size);
		}
		double replace_end = MPI_Wtime();
		total_replace_time += (replace_end - replace_start);
	}
	free(temp_buffer);
	double exchange_end = MPI_Wtime();
	double exchange_time = exchange_end - exchange_start;

    double u_end = MPI_Wtime();
	double max_u_time = 0;
	double total_u_time = u_end - u_start;
	MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, comm);
	if (total_u_time == max_u_time)
	{
		 std::cout << "[NoRotBruck] ["  << nprocs << " " << sendcount << "] " << total_u_time << " " << rotation_time << " " << exchange_time << " ["
				 << total_find_blocks_time << " " << total_copy_time << " " << total_comm_time << " " << total_replace_time << "] " << std::endl;
	}
}


static void modified_dt_bruck_uniform_benchmark(char *sendbuf, int sendcount, MPI_Datatype sendtype, char *recvbuf, int recvcount, MPI_Datatype recvtype,  MPI_Comm comm)
{
	double u_start = MPI_Wtime();

    int rank, nprocs;
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &nprocs);

    int typesize;
    MPI_Type_size(sendtype, &typesize);

    u64 unit_size = sendcount * typesize;
    u64 local_size = sendcount * nprocs * typesize;

    // 1. local rotation
    double rotation_start = MPI_Wtime();
    for (int i = 0; i < nprocs; i++)
    {
    	int index = (2*rank-i+nprocs)%nprocs;
    	memcpy(&recvbuf[index*unit_size], &sendbuf[i*unit_size], unit_size);
    }
    double rotation_end = MPI_Wtime();
    double rotation_time = rotation_end - rotation_start;

    // 2. exchange data with log(P) steps
    double exchange_start =  MPI_Wtime();
 	double total_create_dt_time=0, total_comm_time=0, total_copy_time=0;
 	for (int k = 1; k < nprocs; k <<= 1)
 	{
 		// 1) create data type
		double create_datatype_start = MPI_Wtime();
		int displs[(nprocs+1)/2];
		int sendb_num = 0;
		for (int i = 1; i < nprocs; i++)
		{
			if (i & k)
				displs[sendb_num++] = ((rank+i)%nprocs)*unit_size;
		}
		MPI_Datatype send_type;
		MPI_Type_create_indexed_block(sendb_num, unit_size, displs, MPI_CHAR, &send_type);
		MPI_Type_commit(&send_type);
		double create_datatype_end = MPI_Wtime();
		total_create_dt_time += create_datatype_end - create_datatype_start;

		// 2) exchange data
		double comm_start = MPI_Wtime();
		int recv_proc = (rank + k) % nprocs; // receive data from rank + 2^k process
		int send_proc = (rank - k + nprocs) % nprocs; // send data from rank - 2^k process
		MPI_Sendrecv(recvbuf, 1, send_type, send_proc, 0, sendbuf, 1, send_type, recv_proc, 0, comm, MPI_STATUS_IGNORE);
		MPI_Type_free(&send_type);
		double comm_end = MPI_Wtime();
		total_comm_time += (comm_end - comm_start);

		// 3) copy time
		double copy_start = MPI_Wtime();
		for (int i = 0; i < sendb_num; i++)
			memcpy(recvbuf+displs[i], sendbuf+displs[i], unit_size);
		double copy_end = MPI_Wtime();
		total_copy_time += (copy_end - copy_start);
 	}
 	double exchange_end = MPI_Wtime();
 	double exchange_time = exchange_end - exchange_start;

    double u_end = MPI_Wtime();
 	double max_u_time = 0;
 	double total_u_time = u_end - u_start;
 	MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, comm);
	if (total_u_time == max_u_time)
	{
		 std::cout << "[ModDtBruck] [" << nprocs << " " << sendcount << "] " << total_u_time << " " << rotation_time << " " << exchange_time << " ["
				 << total_create_dt_time << " " << total_comm_time << " " << total_copy_time << "] " << std::endl;
	}
}


static void zerocopy_bruck_uniform_benchmark(char *sendbuf, int sendcount, MPI_Datatype sendtype, char *recvbuf, int recvcount, MPI_Datatype recvtype,  MPI_Comm comm)
{
	double u_start = MPI_Wtime();

    int rank, nprocs;
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &nprocs);

    int typesize;
    MPI_Type_size(sendtype, &typesize);

    u64 unit_size = sendcount * typesize;
    u64 local_size = sendcount * nprocs * typesize;

    // 1. local rotation
    double rotation_start = MPI_Wtime();
    for (int i = 0; i < nprocs; i++)
    {
    	int index = (2*rank-i+nprocs)%nprocs;
    	memcpy(&recvbuf[index*unit_size], &sendbuf[i*unit_size], unit_size);
    }
    double rotation_end = MPI_Wtime();
    double rotation_time = rotation_end - rotation_start;

    // 2. initial data to recv_buffer and intermediate buffer
    double initial_start = MPI_Wtime();
    char* temp_buffer = (char*)malloc(local_size);
	memcpy(temp_buffer, recvbuf, local_size);
	memcpy(sendbuf, recvbuf, local_size);

	int bits[nprocs];
	bits[0] = 0; // the number of bits k' > k
	for (int j = 1; j < nprocs; j++) bits[j] = bits[j>>1]+(j&0x1);

	double initial_end = MPI_Wtime();
	double initial_time = initial_end - initial_start;

	// 3. exchange data with log(P) steps
	double exchange_start =  MPI_Wtime();
	int commblocks[nprocs];
	MPI_Datatype commtypes[nprocs];
	MPI_Aint recvindex[nprocs];
	MPI_Aint sendindex[nprocs];
	double total_create_dt_time = 0, total_comm_time = 0;
	unsigned int mask = 0xFFFFFFFF;
	int b, j;
	for (int k = 1; k < nprocs; k <<= 1)
	{
		// 1) create struct data-type that send and receive data from two buffers
		double create_datatype_start = MPI_Wtime();
		b = 0; j = k;
		while (j < nprocs)
		{
			int index = (rank + j) % nprocs;
			commblocks[b] = unit_size;
			commtypes[b] = MPI_CHAR;

			if ((bits[j&mask]&0x1)==0x1) // send to recvbuf when the number of bits k' > k is odd
			{
				recvindex[b] = (MPI_Aint)((char*)recvbuf+index*unit_size);

				if ((j & mask) == j) // recv from sendbuf when the number of bits k' > k is even
					sendindex[b] = (MPI_Aint)((char*)sendbuf+index*unit_size);
				else // from intermediate buffer
					sendindex[b] = (MPI_Aint)((char*)temp_buffer+index*unit_size);
			}
			else // send to intermediate buffer
			{
				recvindex[b] = (MPI_Aint)((char*)temp_buffer+index*unit_size);

				if ((j & mask) == j) // recv from sendbuf when the number of bits k' > k is even
					sendindex[b] = (MPI_Aint)((char*)sendbuf+index*unit_size);
				else
					sendindex[b] = (MPI_Aint)((char*)recvbuf+index*unit_size);
			}
			b++;
			j++; if ((j & k) != k) j += k; // data blocks whose kth bit is 1
		}

		MPI_Datatype sendblocktype;
		MPI_Type_create_struct(b, commblocks, sendindex, commtypes, &sendblocktype);
		MPI_Type_commit(&sendblocktype);
		MPI_Datatype recvblocktype;
		MPI_Type_create_struct(b,commblocks, recvindex, commtypes, &recvblocktype);
		MPI_Type_commit(&recvblocktype);
		double create_datatype_end = MPI_Wtime();
		total_create_dt_time += (create_datatype_end - create_datatype_start);

		// 2) exchange data
		double comm_start = MPI_Wtime();
		int sendrank = (rank - k + nprocs) % nprocs;
		int recvrank = (rank + k) % nprocs;
		MPI_Sendrecv(MPI_BOTTOM, 1, sendblocktype, sendrank, 0, MPI_BOTTOM, 1, recvblocktype, recvrank, 0, comm, MPI_STATUS_IGNORE);

		MPI_Type_free(&recvblocktype);
		MPI_Type_free(&sendblocktype);
		double comm_end = MPI_Wtime();
		total_comm_time += (comm_end - comm_start);

		mask <<= 1;
	}
	free(temp_buffer);
	double exchange_end =  MPI_Wtime();
	double exchange_time = exchange_end - exchange_start;

	double u_end = MPI_Wtime();
	double max_u_time = 0;
	double total_u_time = u_end - u_start;
	MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, comm);
	if (total_u_time == max_u_time)
	{
		 std::cout << "[ZerocopyBruck] [" << nprocs << " " << sendcount << "] " << total_u_time << " " << rotation_time << " " << exchange_time << " ["
				 << total_create_dt_time << " " << total_comm_time << "] " << std::endl;
	}
}


static void zeroCopyRot_bruck_uniform_benchmark(char *sendbuf, int sendcount, MPI_Datatype sendtype, char *recvbuf, int recvcount, MPI_Datatype recvtype,  MPI_Comm comm)
{
	double u_start = MPI_Wtime();

	int rank, nprocs;
	MPI_Comm_rank(comm, &rank);
	MPI_Comm_size(comm, &nprocs);

	int typesize;
	MPI_Type_size(sendtype, &typesize);

	u64 unit_size = sendcount * typesize;
	u64 local_size = sendcount * nprocs * typesize;

	// 1. create local index array after rotation
	double create_rindex_start = MPI_Wtime();
	int rotate_index_array[nprocs];
    for (int i = 0; i < nprocs; i++)
    	rotate_index_array[i] = (2*rank-i+nprocs)%nprocs;
	double create_rindex_end = MPI_Wtime();
	double create_rindex_time = create_rindex_end - create_rindex_start;

	// 2. initial data to recv_buffer and intermediate buffer
	double initial_start = MPI_Wtime();
	char* temp_buffer = (char*)malloc(local_size);
	memcpy(temp_buffer, sendbuf, local_size);
	memcpy(recvbuf, sendbuf, local_size);

	int bits[nprocs];
	bits[0] = 0; // the number of bits k' > k
	for (int j = 1; j < nprocs; j++) bits[j] = bits[j>>1]+(j&0x1);

	double initial_end = MPI_Wtime();
	double initial_time = initial_end - initial_start;

	// 3. exchange data with log(P) steps
	double exchange_start =  MPI_Wtime();
	int commblocks[nprocs];
	MPI_Datatype commtypes[nprocs];
	MPI_Aint recvindex[nprocs];
	MPI_Aint sendindex[nprocs];
	double total_create_dt_time = 0, total_comm_time = 0;
	unsigned int mask = 0xFFFFFFFF;
	int b, j;
	for (int k = 1; k < nprocs; k <<= 1)
	{
		// 1) create struct data-type that send and receive data from two buffers
		double create_datatype_start = MPI_Wtime();
		b = 0; j = k;
		while (j < nprocs)
		{
			int send_index = rotate_index_array[(rank+j)%nprocs];
			int recv_index = (rank+j)%nprocs;

			commblocks[b] = unit_size;
			commtypes[b] = MPI_CHAR;

			if ((bits[j&mask]&0x1)==0x1) // send to recvbuf when the number of bits k' > k is odd
			{
				recvindex[b] = (MPI_Aint)((char*)recvbuf+recv_index*unit_size);

				if ((j & mask) == j) // recv from sendbuf when the number of bits k' > k is even
					sendindex[b] = (MPI_Aint)((char*)sendbuf+send_index*unit_size);
				else // from intermediate buffer
					sendindex[b] = (MPI_Aint)((char*)temp_buffer+send_index*unit_size);
			}
			else // send to intermediate buffer
			{
				recvindex[b] = (MPI_Aint)((char*)temp_buffer+send_index*unit_size);

				if ((j & mask) == j) // recv from sendbuf when the number of bits k' > k is even
					sendindex[b] = (MPI_Aint)((char*)sendbuf+send_index*unit_size);
				else
					sendindex[b] = (MPI_Aint)((char*)recvbuf+recv_index*unit_size);
			}
			b++;
			j++; if ((j & k) != k) j += k; // data blocks whose kth bit is 1
		}

		MPI_Datatype sendblocktype;
		MPI_Type_create_struct(b, commblocks, sendindex, commtypes, &sendblocktype);
		MPI_Type_commit(&sendblocktype);
		MPI_Datatype recvblocktype;
		MPI_Type_create_struct(b,commblocks, recvindex, commtypes, &recvblocktype);
		MPI_Type_commit(&recvblocktype);
		double create_datatype_end = MPI_Wtime();
		total_create_dt_time += (create_datatype_end - create_datatype_start);

		// 2) exchange data
		double comm_start = MPI_Wtime();
		int sendrank = (rank - k + nprocs) % nprocs;
		int recvrank = (rank + k) % nprocs;
		MPI_Sendrecv(MPI_BOTTOM, 1, sendblocktype, sendrank, 0, MPI_BOTTOM, 1, recvblocktype, recvrank, 0, comm, MPI_STATUS_IGNORE);

		MPI_Type_free(&recvblocktype);
		MPI_Type_free(&sendblocktype);
		double comm_end = MPI_Wtime();
		total_comm_time += (comm_end - comm_start);

		mask <<= 1;
	}
	free(temp_buffer);
	double exchange_end =  MPI_Wtime();
	double exchange_time = exchange_end - exchange_start;

	double u_end = MPI_Wtime();
	double max_u_time = 0;
	double total_u_time = u_end - u_start;
	MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, comm);
	if (total_u_time == max_u_time)
	{
		 std::cout << "[ZeroRotCopyBruck] [" << nprocs << " " << sendcount << "] " << total_u_time << " " << create_rindex_time << " " << exchange_time << " ["
				 << total_create_dt_time << " " << total_comm_time << "] " << std::endl;
	}
}



static void noRotation_bruck_uniform_benchmark(char *sendbuf, int sendcount, MPI_Datatype sendtype, char *recvbuf, int recvcount, MPI_Datatype recvtype,  MPI_Comm comm)
{
	double u_start = MPI_Wtime();

    int rank, nprocs;
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &nprocs);

    int typesize;
    MPI_Type_size(sendtype, &typesize);

    u64 unit_size = sendcount * typesize;
    u64 local_size = sendcount * nprocs * typesize;

	// 1. create local index array after rotation
	double create_rindex_start = MPI_Wtime();
	int rotate_index_array[nprocs];
    for (int i = 0; i < nprocs; i++)
    	rotate_index_array[i] = (2*rank-i+nprocs)%nprocs;
	double create_rindex_end = MPI_Wtime();
	double create_rindex_time = create_rindex_end - create_rindex_start;

	// 2. Initial receive buffer
	memcpy(recvbuf+rank*unit_size, sendbuf+rank*unit_size, unit_size);
    double exchange_start =  MPI_Wtime();
 	double total_create_dt_time = 0, total_comm_time = 0, total_copy_time = 0;
 	char* temp_buffer = (char*)malloc(local_size);
 	for (int k = 1; k < nprocs; k <<= 1)
 	{
 		// 1) create data type
		double create_datatype_start = MPI_Wtime();
		int send_displs[(nprocs+1)/2];
		int send_index[(nprocs+1)/2];
		int sendb_num = 0;
		for (int i = 1; i < nprocs; i++)
		{
			if (i & k)
			{
				send_index[sendb_num] = (rank+i)%nprocs;
				send_displs[sendb_num] = rotate_index_array[(rank+i)%nprocs]*unit_size;
				sendb_num++;
			}
		}
		MPI_Datatype send_type;
		MPI_Type_create_indexed_block(sendb_num, unit_size, send_displs, MPI_CHAR, &send_type);
		MPI_Type_commit(&send_type);
		int packsize;
		MPI_Pack_size(1, send_type, comm, &packsize);
		double create_datatype_end = MPI_Wtime();
		total_create_dt_time += create_datatype_end - create_datatype_start;

		// 2) exchange data
		double comm_start = MPI_Wtime();
		int recv_proc = (rank + k) % nprocs; // receive data from rank + 2^k process
		int send_proc = (rank - k + nprocs) % nprocs; // send data from rank - 2^k process
		MPI_Sendrecv(sendbuf, 1, send_type, send_proc, 0, temp_buffer, packsize, MPI_PACKED, recv_proc, 0, comm, MPI_STATUS_IGNORE);
		int pos = 0;
		MPI_Unpack(temp_buffer, packsize, &pos, sendbuf, 1, send_type, comm);
		MPI_Type_free(&send_type);
		double comm_end = MPI_Wtime();
		total_comm_time += (comm_end - comm_start);

		// 3) copy data to recvbuf
		double copy_start = MPI_Wtime();
		for (int i = 0; i < sendb_num; i++)
			memcpy(recvbuf+send_index[i]*unit_size, temp_buffer+i*unit_size, unit_size);
		double copy_end = MPI_Wtime();
		total_copy_time += (copy_end - copy_start);
 	}
 	free(temp_buffer);
 	double exchange_end =  MPI_Wtime();
 	double exchange_time = exchange_end - exchange_start;

    double u_end = MPI_Wtime();
 	double max_u_time = 0;
 	double total_u_time = u_end - u_start;
 	MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, comm);
	if (total_u_time == max_u_time)
	{
		 std::cout << "[NoRotBruck] [" << nprocs << " " << sendcount << "] " << total_u_time << " " << create_rindex_time << " " << exchange_time << " ["
				 << total_create_dt_time << " " << total_comm_time << " " << total_copy_time << "] " << std::endl;
	}
}



static void non_uniform_benchmark(int ra_count, int nprocs, u64 entry_count, int random_offset, int range)
{
    all_to_allv_buffer non_uniform_buffer;
    non_uniform_buffer.nprocs = nprocs;
    non_uniform_buffer.ra_count = ra_count;
    non_uniform_buffer.local_compute_output_size_total = 0;

    non_uniform_buffer.local_compute_output_size_flat = new int[non_uniform_buffer.nprocs * non_uniform_buffer.ra_count];
    non_uniform_buffer.cumulative_tuple_process_map = new int[non_uniform_buffer.nprocs];
    memset(non_uniform_buffer.cumulative_tuple_process_map, 0, non_uniform_buffer.nprocs * sizeof(int));

    for (int r=0; r < non_uniform_buffer.ra_count; r++)
    {
        for (int i=0; i < non_uniform_buffer.nprocs; i++)
        {
        	int random = random_offset + rand() % range;
        	non_uniform_buffer.local_compute_output_size_flat[i * non_uniform_buffer.ra_count + r] = (entry_count * random) / 100;
        	non_uniform_buffer.cumulative_tuple_process_map[i] = non_uniform_buffer.cumulative_tuple_process_map[i] + non_uniform_buffer.local_compute_output_size_flat[i * non_uniform_buffer.ra_count + r];
        }
    }

    for (int it=0; it < ITERATION_COUNT; it++)
    {
        non_uniform_buffer.local_compute_output = new vector_buffer*[non_uniform_buffer.ra_count];
        for (int r=0; r < non_uniform_buffer.ra_count; r++)
            non_uniform_buffer.local_compute_output[r] = new vector_buffer[non_uniform_buffer.nprocs];

        double buffer_creation_time=0, all_to_allv_time=0, buffer_deletion_time=0;

        double nu_start = MPI_Wtime();

        double buffer_creation_start=MPI_Wtime();
        non_uniform_buffer.local_compute_output_size_total=0;
//        memset(non_uniform_buffer.cumulative_tuple_process_map, 0, non_uniform_buffer.nprocs * sizeof(int));
//        memset(non_uniform_buffer.local_compute_output_size_flat, 0, non_uniform_buffer.nprocs * non_uniform_buffer.ra_count * sizeof(int));

        for (int r=0; r < non_uniform_buffer.ra_count; r++)
        {
            for (int i=0; i < non_uniform_buffer.nprocs; i++)
            {
                u64 val = i;
                for (int t=0; t < non_uniform_buffer.local_compute_output_size_flat[i * non_uniform_buffer.ra_count + r]; t++)
                {
                    non_uniform_buffer.local_compute_output[r][i].vector_buffer_append((const unsigned char*)&val, sizeof(u64));
                    non_uniform_buffer.local_compute_output_size_total++;
                }
            }
        }

        double buffer_creation_end=MPI_Wtime();
        buffer_creation_time = buffer_creation_end - buffer_creation_start;

        double all_to_allv_start=MPI_Wtime();
        double a2a_all_to_all_time, a2a_buffer_creation_time, a2a_buffer_deletion_time, a2a_all_to_allv_time;
        all_to_allv_test(non_uniform_buffer, MPI_COMM_WORLD, &a2a_all_to_all_time, &a2a_buffer_creation_time, &a2a_buffer_deletion_time, &a2a_all_to_allv_time, it, nprocs, entry_count, random_offset, range);
        double all_to_allv_end=MPI_Wtime();
        all_to_allv_time = all_to_allv_end - all_to_allv_start;


        double buffer_deletion_start=MPI_Wtime();
        for (int r=0; r < non_uniform_buffer.ra_count; r++)
            for (int i=0; i < non_uniform_buffer.nprocs; i++)
                non_uniform_buffer.local_compute_output[r][i].vector_buffer_free();
        double buffer_deletion_end=MPI_Wtime();
        buffer_deletion_time = buffer_deletion_end-buffer_deletion_start;

        double nu_end = MPI_Wtime();

        double max_nu_time = 0;
        double total_nu_time = nu_end - nu_start;
        MPI_Allreduce(&total_nu_time, &max_nu_time, 1, MPI_DOUBLE, MPI_MAX, MPI_COMM_WORLD);


        if (max_nu_time == total_nu_time){
            if (it == 0)
                std::cout << "SKIP [NU] " << it << ", " << nprocs << " [ " << random_offset << " " << range << " ] " << entry_count << " NU time " << (nu_end - nu_start) << " [" << buffer_creation_time + all_to_allv_time + buffer_deletion_time << "] Buff pop " << buffer_creation_time << " NA2A " << all_to_allv_time << " [ a2a " << a2a_all_to_all_time << " bct " << a2a_buffer_creation_time << " a2av " << a2a_all_to_allv_time << " bdt " << a2a_buffer_deletion_time << " ] Buff del " << buffer_deletion_time << " " << std::endl;
            else
                std::cout << "[NU] " << it << ", " << nprocs << " [ " << random_offset << " " << range << " ] " << entry_count << " NU time " << (nu_end - nu_start) << " [" << buffer_creation_time + all_to_allv_time + buffer_deletion_time << "] Buff pop " << buffer_creation_time << " NA2A " << all_to_allv_time << " [ a2a " << a2a_all_to_all_time << " bct " << a2a_buffer_creation_time << " a2av " << a2a_all_to_allv_time << " bdt " << a2a_buffer_deletion_time << " ] Buff del " << buffer_deletion_time << " " << std::endl;
        }

        for (int i=0; i < non_uniform_buffer.ra_count; i++)
            delete[] non_uniform_buffer.local_compute_output[i];
        delete[] non_uniform_buffer.local_compute_output;
    }


    delete[] non_uniform_buffer.local_compute_output_size_flat;
    delete[] non_uniform_buffer.cumulative_tuple_process_map;
}


#if 1
static void all_to_allv_test(all_to_allv_buffer non_uniform_buffer, MPI_Comm comm, double* all_to_all_time, double* buffer_create_time, double* buffer_delete_time, double *all_to_allv_time, int it, int nprocs, u64 entry_count, int random_offset, int range)
{
    double all_to_all_start = MPI_Wtime();
    int outer_hash_buffer_size = 0;
    u32 RA_count = non_uniform_buffer.ra_count;
    int *recv_buffer_offset_size = new int[RA_count * nprocs];
    MPI_Alltoall(non_uniform_buffer.local_compute_output_size_flat, RA_count, MPI_INT, recv_buffer_offset_size, RA_count, MPI_INT, comm);
    double all_to_all_end = MPI_Wtime();
    *all_to_all_time = all_to_all_end - all_to_all_start;

    double bt_start = MPI_Wtime();
    int *send_disp = new int[nprocs];
    int *recv_counts = new int[nprocs];
    int *recv_displacements = new int[nprocs];

    recv_counts[0] = 0;
    send_disp[0] = 0;
    recv_displacements[0] = 0;

    u64* send_buffer = new u64[non_uniform_buffer.local_compute_output_size_total];

    u32 boffset = 0;
    for(int i = 0; i < nprocs; i++)
    {
        recv_counts[i] = 0;

        if (i >= 1)
            send_disp[i] = send_disp[i - 1] + non_uniform_buffer.cumulative_tuple_process_map[i - 1];

        for (u32 r = 0; r < RA_count; r++)
        {
            memcpy(send_buffer + boffset, non_uniform_buffer.local_compute_output[r][i].buffer, non_uniform_buffer.local_compute_output[r][i].size);
            boffset = boffset + (non_uniform_buffer.local_compute_output[r][i].size)/sizeof(u64);
            recv_counts[i] = recv_counts[i] + recv_buffer_offset_size[i*RA_count + r];
        }

        if (i >= 1)
            recv_displacements[i] = recv_displacements[i - 1] + recv_counts[i - 1];
        outer_hash_buffer_size = outer_hash_buffer_size + recv_counts[i];
    }

    u64 *recv_buffer = new u64[outer_hash_buffer_size];
    double bt_end = MPI_Wtime();
    *buffer_create_time = bt_end - bt_start;

    double atv_start = MPI_Wtime();
    MPI_Alltoallv(send_buffer, non_uniform_buffer.cumulative_tuple_process_map, send_disp, MPI_UNSIGNED_LONG_LONG, recv_buffer, recv_counts, recv_displacements, MPI_UNSIGNED_LONG_LONG, comm);
    double atv_end = MPI_Wtime();
    *all_to_allv_time = atv_end - atv_start;

    if (it == 0)
    {
        int rank;
        MPI_Comm_rank(comm, &rank);
        u64 total_send_count=0;
        u64 total_recv_count=0;
        u64 global_total_send_count=0;
        u64 global_total_recv_count=0;
        for (int i=0; i < nprocs; i++)
        {
            total_send_count = total_send_count + non_uniform_buffer.cumulative_tuple_process_map[i];
            total_recv_count = total_recv_count + recv_counts[i];
        }
        MPI_Allreduce(&total_send_count, &global_total_send_count, 1, MPI_UNSIGNED_LONG_LONG, MPI_SUM, MPI_COMM_WORLD);
        MPI_Allreduce(&total_recv_count, &global_total_recv_count, 1, MPI_UNSIGNED_LONG_LONG, MPI_SUM, MPI_COMM_WORLD);

        if (rank == 0)
        {
            std::cout << "[Non Uniform] " << nprocs << " , " << entry_count << " , " << random_offset << " , "  << range << " ,  Total recv count " << global_total_recv_count  << " Total send count " << global_total_send_count << std::endl;

//            std::cout << "Send count list [ ";
//            for (int i=0; i < nprocs; i++)
//            	std::cout << non_uniform_buffer.local_compute_output_size_flat[i] << ", ";
//            std::cout << "]" << std::endl;
        }

        if (global_total_recv_count != global_total_send_count)
            MPI_Abort(MPI_COMM_WORLD, -1);
        if (global_total_recv_count != global_total_send_count)
            MPI_Abort(MPI_COMM_WORLD, -1);
    }

    double bt_d_start = MPI_Wtime();
    delete[] recv_buffer;
    delete[] send_buffer;
    delete[] send_disp;
    delete[] recv_displacements;
    delete[] recv_counts;
    delete[] recv_buffer_offset_size;
    double bt_d_end = MPI_Wtime();
    *buffer_delete_time = bt_d_end - bt_d_start;
}
#endif


static void complete_uniform_benchmark(int ra_count, int nprocs, u64 entry_count, int random_offset, int range)
{
    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    all_to_allv_buffer non_uniform_buffer;
    non_uniform_buffer.local_compute_output_size_flat = new int[nprocs * ra_count];

    int max_random = 0;
    for (int r=0; r < ra_count; r++)
    {
        for (int i=0; i < nprocs; i++)
        {
        	int random = random_offset + rand() % range;
        	non_uniform_buffer.local_compute_output_size_flat[i * ra_count + r] = (entry_count * random) / 100;
        	if (((entry_count * random) / 100) > max_random)
        		max_random = (entry_count * random) / 100;
        }
    }

    int max_send_count = 0;
    MPI_Allreduce(&max_random, &max_send_count, 1, MPI_INT, MPI_MAX, MPI_COMM_WORLD);

	all_to_all_buffer uniform_buffer;
	uniform_buffer.local_compute_output = new u64[(ra_count * nprocs * max_send_count)];

	u64 *cumulative_all_to_allv_buffer = new u64[(ra_count * nprocs * max_send_count)];

    for (int it=0; it < ITERATION_COUNT; it++)
    {
    	double u_start = MPI_Wtime();
		double buff_pop_start = MPI_Wtime();
		for (u64 i=0; i < (ra_count * nprocs * max_send_count); i++)
			uniform_buffer.local_compute_output[i] = i / (ra_count * max_send_count);
		double buff_pop_end = MPI_Wtime();

		double a2a_start = MPI_Wtime();
		MPI_Alltoall(uniform_buffer.local_compute_output, (ra_count * max_send_count), MPI_UNSIGNED_LONG_LONG, cumulative_all_to_allv_buffer, (ra_count * max_send_count), MPI_UNSIGNED_LONG_LONG, MPI_COMM_WORLD);
		double a2a_end = MPI_Wtime();

		double u_end = MPI_Wtime();

        double u_iter_buffer_time = buff_pop_end - buff_pop_start;
        double u_iter_a2a_time = a2a_end - a2a_start;

	    double max_u_time = 0;
		double total_u_time = u_end - u_start;
		MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, MPI_COMM_WORLD);
		if (total_u_time == max_u_time)
		{
			if (it == 0)
			{
				std::cout << "SKIP [CU] " << it << ", " << nprocs << " [ " << entry_count << " " << random_offset << " " << range <<  " ] CU time " << (u_end - u_start)
						<< " [" << u_iter_buffer_time + u_iter_a2a_time << "] Buff pop " << u_iter_buffer_time << " a2a " << u_iter_a2a_time << std::endl;
			}
			else
				std::cout << "[CU] " << it << ", " << nprocs << " [ " << entry_count << " " << random_offset << " " << range << " ] CU time " << (u_end - u_start)
						<< " [" << u_iter_buffer_time + u_iter_a2a_time << "] Buff pop " << u_iter_buffer_time  << " a2a " << u_iter_a2a_time << std::endl;
		}
    }

    u64 global_total_send_count = max_send_count * nprocs * nprocs;

    if(rank == 0)
    	std::cout << "[Complete Uniform] Max send count " << max_send_count  << ", total send count " << global_total_send_count << std::endl;

    delete[] non_uniform_buffer.local_compute_output_size_flat;
    delete[] cumulative_all_to_allv_buffer;
    delete[] uniform_buffer.local_compute_output;
}



static void uniform_ptp_benchmark(int ra_count, int nprocs, u64 entry_count)
{
    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    all_to_all_buffer uniform_buffer;

    u64 unit_count = ra_count * entry_count;
    uniform_buffer.local_compute_output = new u64[(unit_count * nprocs)];
    u64 *cumulative_all_to_allv_buffer = new u64[(unit_count * nprocs)];

    for (int it=0; it < ITERATION_COUNT; it++)
    {
        double u_iter_buffer_time;
        double u_iter_a2a_time;
        double u_start = MPI_Wtime();

        u64 global_cumulativ_send_count = 0;

		double buff_pop_start = MPI_Wtime();
		for (u64 i=0; i < (ra_count * nprocs * entry_count); i++)
			uniform_buffer.local_compute_output[i] = i / (ra_count * entry_count);
		double buff_pop_end = MPI_Wtime();

		double a2a_start = MPI_Wtime();
		MPI_Request* req = (MPI_Request*)malloc(2 * nprocs * sizeof(MPI_Request));
		MPI_Status* stat = (MPI_Status*)malloc(2 * nprocs * sizeof(MPI_Status));
		for (int i = 0; i < nprocs; i++)
		{
			int comm_p = (rank + i) % nprocs; // avoid always to reach first master node
			MPI_Irecv(&cumulative_all_to_allv_buffer[comm_p*unit_count], unit_count, MPI_UNSIGNED_LONG_LONG, comm_p, 0, MPI_COMM_WORLD, &req[i]);
		}

		for (int i = 0; i < nprocs; i++)
		{
			int comm_p = (rank + i) % nprocs;
			MPI_Isend(&uniform_buffer.local_compute_output[comm_p*unit_count], unit_count, MPI_UNSIGNED_LONG_LONG, comm_p, 0, MPI_COMM_WORLD, &req[i+nprocs]);
		}
		MPI_Waitall(2 * nprocs, req, stat);
		free(req);
		free(stat);
		double a2a_end = MPI_Wtime();

		u_iter_buffer_time = buff_pop_end-buff_pop_start;
		u_iter_a2a_time = a2a_end-a2a_start;

		double u_end = MPI_Wtime();

		double max_u_time = 0;
		double total_u_time = u_end - u_start;
		MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, MPI_COMM_WORLD);
		if (total_u_time == max_u_time)
		{
			if (it == 0)
			{
				std::cout << "SKIP [PU] " << rank << ", " << nprocs << " [ " << entry_count << " ] PU time " << (u_end - u_start) << " ";
				std::cout << u_iter_buffer_time << " " << u_iter_a2a_time << " " << std::endl;
			}
			else
			{
				std::cout << "[PU] " << rank << ", " << nprocs << " [ " << entry_count << " ] PU time " << (u_end - u_start) << " ";
				std::cout << u_iter_buffer_time << " " << u_iter_a2a_time << std::endl;
			}
		}
    }

    delete[] cumulative_all_to_allv_buffer;
    delete[] uniform_buffer.local_compute_output;
}


static void three_phases_uniform_benchmark(int ra_count, int nprocs, u64 entry_count, int node_procs)
{
    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    int node_id = rank / node_procs;
    int master_rank = node_id * node_procs; // 0 * 64, 1 * 64

    u64 unit_count = ra_count * entry_count;
    u64 local_count = unit_count * nprocs;
    std::vector<u64> local_compute_output(local_count);

    std::vector<u64> node_all_to_all_buffer;
    std::vector<u64> cumulative_all_to_allv_buffer;

    for (int it=0; it < ITERATION_COUNT; it++)
    {
    	double u_start = MPI_Wtime();

        double buff_pop_start = MPI_Wtime();
        for (u64 i=0; i < local_count; i++)
            local_compute_output[i] = i / (unit_count);
        double buff_pop_end = MPI_Wtime();

        // 1. gather data from other process on the same node to one master rank on that node
        double gather_node_procs_start = MPI_Wtime();
        int comm_count = (rank == master_rank)? (node_procs + 1) : 1;
        MPI_Request req[comm_count];
        MPI_Status stat[comm_count];
        int req_count = 0;
        if (rank == master_rank)
        {
        	node_all_to_all_buffer.reserve(local_count * node_procs);
        	cumulative_all_to_allv_buffer.reserve(local_count * node_procs);

        	for (int i = 0; i < node_procs; i++)
        	{
        		int recv_rank = node_id * node_procs + i;  // 3 * 64 + (0 ~ 64)
        		MPI_Irecv(&node_all_to_all_buffer.data()[i*local_count], local_count, MPI_UNSIGNED_LONG_LONG, recv_rank, recv_rank, MPI_COMM_WORLD, &req[req_count]);
        		req_count++;
        	}
        }

        MPI_Isend(local_compute_output.data(), local_count, MPI_UNSIGNED_LONG_LONG, master_rank, rank, MPI_COMM_WORLD, &req[req_count]);
		req_count++;
		MPI_Waitall(req_count, req, stat);
		double gather_node_procs_end = MPI_Wtime();


		double reorder_time = 0, ata_time = 0, reorder_2_time = 0;
		u64 blocklength = unit_count * node_procs;
		int node_num = nprocs / node_procs;
		if (rank == master_rank)
		{
	        // 2. exchange order of buffer
			//    e.g., 0 2 [0 1 2 3 0 1 2 3] to [0 1 0 1 2 3 2 3] (each process [0 1 2 3])
			double reorder_start = MPI_Wtime();
			for (int i = 0; i < node_num; i++)
			{
				for (int j = 0; j < node_procs; j++)
				{
					int offset = j * local_count + i * blocklength;
					int index = i * node_procs + j;
					memcpy(&cumulative_all_to_allv_buffer.data()[index * blocklength], &node_all_to_all_buffer.data()[offset], blocklength * sizeof(long long));
				}
			}
			double reorder_end = MPI_Wtime();
			reorder_time = reorder_end - reorder_start;

			// 3. all to all communication across all the master ranks
			double ata_start = MPI_Wtime();
			req_count = 0;
			MPI_Request req_1[2*node_num];
			MPI_Status stat_1[2*node_num];
			u64 exchange_count = blocklength * node_procs;
			for (int i = 0; i < node_num; i++)
			{
				int comm_node_id = (node_id + i) % node_num; // avoid always to reach first master node
				int comm_p = comm_node_id * node_procs;
				MPI_Irecv(&node_all_to_all_buffer.data()[comm_node_id*exchange_count], exchange_count, MPI_UNSIGNED_LONG_LONG, comm_p, comm_p, MPI_COMM_WORLD, &req_1[req_count]);
				req_count++;

				MPI_Isend(&cumulative_all_to_allv_buffer.data()[comm_node_id*exchange_count], exchange_count, MPI_UNSIGNED_LONG_LONG, comm_p, rank, MPI_COMM_WORLD, &req_1[req_count]);
				req_count++;
			}
			MPI_Waitall(req_count, req_1, stat_1);
			double ata_end = MPI_Wtime();
			ata_time = ata_end - ata_start;


			// 4. reorder [0 1 0 1 0 1 0 1] --> [0 0 0 0 1 1 1 1]
			double reorder_2_start = MPI_Wtime();
			for (int i = 0; i < node_procs; i++)
			{
				for (int j = 0; j < nprocs; j++)
				{
					int offset = i * unit_count + j * blocklength;
					int index = i * nprocs + j;
					memcpy(&cumulative_all_to_allv_buffer.data()[index * unit_count], &node_all_to_all_buffer.data()[offset], unit_count * sizeof(long long));
				}
			}
			double reorder_2_end = MPI_Wtime();
			reorder_2_time = reorder_2_end - reorder_2_start;
		}

		// 5. master rank assign data to each process on the same node
		double scatter_start = MPI_Wtime();
		req_count = 0;
		MPI_Irecv(local_compute_output.data(), local_count, MPI_UNSIGNED_LONG_LONG, master_rank, rank, MPI_COMM_WORLD, &req[req_count]);
		req_count++;

        if (rank == master_rank)
        {
        	for (int i = 0; i < node_procs; i++)
        	{
        		int send_rank = node_id * node_procs + i;
        		MPI_Isend(&cumulative_all_to_allv_buffer.data()[i*local_count], local_count, MPI_UNSIGNED_LONG_LONG, send_rank, send_rank, MPI_COMM_WORLD, &req[req_count]);
        		req_count++;
        	}
        }
        MPI_Waitall(req_count, req, stat);
        double scatter_end = MPI_Wtime();

        double u_end = MPI_Wtime();

        double max_u_time = 0;
        double total_u_time = u_end - u_start;
        MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, MPI_COMM_WORLD);

        double buff_pop_time = buff_pop_end - buff_pop_start;
        double gather_time = gather_node_procs_end - gather_node_procs_start;
        double scatter_time = scatter_end - scatter_start;


        if (total_u_time == max_u_time)
		{
			if (it == 0)
			{
				std::cout << "SKIP [TPU] " << it << ", " << nprocs << " [ " << entry_count << " ] TPU time " << (u_end - u_start) << " [ "
						<< buff_pop_time << " " << gather_time << " " << reorder_time << " " << ata_time << " " << reorder_2_time << " " << scatter_time << " ] " << std::endl;
			}
			else
			{
				std::cout << "[TPU] " << it << ", " << nprocs << " [ " << entry_count << " ] TPU time " << (u_end - u_start) << " [ "
						<< buff_pop_time << " " << gather_time << " " << reorder_time << " " << ata_time << " " << reorder_2_time << " " << scatter_time << " ] " << std::endl;
			}
		}
    }

    std::vector<u64>().swap(local_compute_output);
    std::vector<u64>().swap(node_all_to_all_buffer);
    std::vector<u64>().swap(cumulative_all_to_allv_buffer);
    local_compute_output.clear();
    node_all_to_all_buffer.clear();
    cumulative_all_to_allv_buffer.clear();
}


#if 1
static void uniform_benchmark(int ra_count, int nprocs, int epoch_count, u64 entry_count)
{
    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    all_to_all_buffer uniform_buffer;
    uniform_buffer.nprocs = nprocs;//mcomm.get_nprocs();
    uniform_buffer.ra_count = ra_count;//atoi(argv[1]);

    uniform_buffer.local_compute_output = new u64[(uniform_buffer.ra_count * uniform_buffer.nprocs * entry_count)/epoch_count];


    u64 *cumulative_all_to_allv_buffer = new u64[(uniform_buffer.ra_count * uniform_buffer.nprocs * entry_count)/epoch_count];

    for (int it=0; it < ITERATION_COUNT; it++)
    {
        double u_iter_buffer_total=0;
        double u_iter_a2a_total=0;
        double u_iter_buffer_time[epoch_count];
        double u_iter_a2a_time[epoch_count];
        double u_start = MPI_Wtime();

        u64 global_cumulativ_send_count = 0;
        for (int e=0; e<epoch_count; e++)
        {
            double buff_pop_start = MPI_Wtime();
            for (u64 i=0; i < (uniform_buffer.ra_count * uniform_buffer.nprocs * entry_count); i=i+epoch_count)
                uniform_buffer.local_compute_output[i/epoch_count] = i / (uniform_buffer.ra_count * entry_count);
            double buff_pop_end = MPI_Wtime();

            double a2a_start = MPI_Wtime();
            MPI_Alltoall(uniform_buffer.local_compute_output, (uniform_buffer.ra_count * entry_count)/epoch_count, MPI_UNSIGNED_LONG_LONG, cumulative_all_to_allv_buffer, (uniform_buffer.ra_count * entry_count)/epoch_count, MPI_UNSIGNED_LONG_LONG, MPI_COMM_WORLD);
            double a2a_end = MPI_Wtime();

            if (it == 0)
            {
                u64 global_send_count = 0;
                u64 send_count = ((uniform_buffer.ra_count * entry_count)/epoch_count) * nprocs;
                MPI_Allreduce(&send_count, &global_send_count, 1, MPI_UNSIGNED_LONG_LONG, MPI_SUM, MPI_COMM_WORLD);
                global_cumulativ_send_count = global_cumulativ_send_count + global_send_count;

                if (e == epoch_count - 1)
                {
                    if (rank == 0)
                        std::cout << "[Uniform] Send and Recieve count " << nprocs << " " << epoch_count << " " << entry_count << " " << global_cumulativ_send_count << std::endl;
                    if (global_cumulativ_send_count != entry_count * nprocs * ra_count * nprocs)
                        MPI_Abort(MPI_COMM_WORLD, -1);
                }
            }

            u_iter_buffer_time[e] = buff_pop_end-buff_pop_start;
            u_iter_a2a_time[e] = a2a_end-a2a_start;

            u_iter_buffer_total = u_iter_buffer_total + u_iter_buffer_time[e];
            u_iter_a2a_total = u_iter_a2a_total + u_iter_a2a_time[e];
        }
        double u_end = MPI_Wtime();

        double max_u_time = 0;
        double total_u_time = u_end - u_start;
        MPI_Allreduce(&total_u_time, &max_u_time, 1, MPI_DOUBLE, MPI_MAX, MPI_COMM_WORLD);
        if (total_u_time == max_u_time)
        {
            if (it == 0)
            {
                std::cout << "SKIP [U] " << it << ", " << nprocs << " [ " << entry_count << " " << epoch_count << " ] U time " << (u_end - u_start) << " [" << u_iter_buffer_total + u_iter_a2a_total << "] " << u_iter_buffer_total << " ( ";
            for (int i=0; i<epoch_count; i++)
                std::cout << u_iter_buffer_time[i] << " ";
            std::cout << ") " << u_iter_a2a_total << " ( ";
            for (int i=0; i<epoch_count; i++)
                std::cout << u_iter_a2a_time[i] << " ";
            std::cout << ")" << std::endl;
            }
            else
            {
                std::cout << "[U] " << it << ", " << nprocs << " [ " << entry_count << " " << epoch_count << " ] U time " << (u_end - u_start) << " [" << u_iter_buffer_total + u_iter_a2a_total << "] " << u_iter_buffer_total << " ( ";
            for (int i=0; i<epoch_count; i++)
                std::cout << u_iter_buffer_time[i] << " ";
            std::cout << ") " << u_iter_a2a_total << " ( ";
            for (int i=0; i<epoch_count; i++)
                std::cout << u_iter_a2a_time[i] << " ";
            std::cout << ")" << std::endl;

            }
        }
    }

    delete[] cumulative_all_to_allv_buffer;
    delete[] uniform_buffer.local_compute_output;
}
#endif
