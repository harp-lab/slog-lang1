/*
 * Function to handle MPI Communicator
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */


#include "../parallel_RA_inc.h"



/// Function is called when new communicator is created
void mpi_comm::set_local_comm(MPI_Comm comm)
{
    local_comm = comm;
    MPI_Comm_size(local_comm, &local_nprocs);
    MPI_Comm_rank(local_comm, &local_rank);
}



/// Initialize MPI
void mpi_comm::create(int argc, char **argv)
{
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    world_comm = MPI_COMM_WORLD;
}



/// Finalize MPI
void mpi_comm::destroy()
{
    MPI_Finalize();
}
