/*
 * Function to handle MPI Communicator
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */

#pragma once

class mpi_comm
{

private:

    MPI_Comm world_comm;    /// Stores the global comm (MPI_COMM_WORLD)
    int rank;               /// Rank of a processe in the global communicator (MPI_COMM_WORLD)
    int nprocs;             /// Total number of processes in the global communicator (MPI_COMM_WORLD)

    MPI_Comm local_comm;    /// Stores the local comm (per-task communicator)
    int local_rank;         /// Rank of a processe in the local communicator (per-task communicator)
    int local_nprocs;       /// Total number of processes in the local communicator (per-task communicator)

public:

    mpi_comm()  {}

    mpi_comm (const mpi_comm &copy)
    {
        rank = copy.rank;
        nprocs = copy.nprocs;
        local_nprocs = copy.local_nprocs;
        local_rank = copy.local_rank;
        world_comm = copy.world_comm;
        local_comm = copy.local_comm;
    }



    /// Returns the global communicator (MPI_COMM_WORLD)
    MPI_Comm get_comm() {return world_comm;}



    /// Returns total number of processes in the global communicator (MPI_COMM_WORLD)
    int get_nprocs()    {return nprocs;}



    /// Returns the rank of a process in the global communicator (MPI_COMM_WORLD)
    int get_rank()  {return rank;}



    /// Returns total number of processes in the local communicator (per-task communicator)
    int get_local_nprocs()  {return local_nprocs;}



    /// Returns the rank of a process in the local communicator (per-task communicator)
    int get_local_rank()    {return local_rank;}



    /// Returns the local communicator (per-task communicator)
    MPI_Comm get_local_comm()    {return local_comm;}


    /// Sets local communicators, it is called when new tasks (and hence new communicators) are created
    void set_local_comm(MPI_Comm comm);


    /// MPI_Init
    void create(int argc, char **argv);


    /// MPI_Finalize
    void destroy();
};
