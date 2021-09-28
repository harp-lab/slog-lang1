// Compilation template for slog daemon
#include "parallel_RA_inc.h"

// builtins.cpp goes here!
~a

// global definitions
~a

int main(int argc, char **argv)
{
  mpi_comm mcomm;
  mcomm.create(argc, argv);

~a

  // Enable IO
 lie->enable_share_io();
 lie->set_output_dir("~a"); // Write to this directory
 lie->set_comm(mcomm);
 lie->set_batch_size(1);
 lie->execute();
 lie->print_all_relation_size(); // Continuously print relation sizes

 delete lie;
 mcomm.destroy();
 return 0;
}
