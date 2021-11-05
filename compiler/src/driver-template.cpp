// location of `parallel_RA_inc.h` here
#include "~a"

// builtins.cpp goes here!
~a

// global definitions:
~a

int main(int argc, char **argv)
{
  // input dir from compiler
  std::string slog_input_dir = "~a";
  // output dir from compiler
  std::string slog_output_dir = "~a";
  if (argc == 3) {
    slog_input_dir = argv[1];
    slog_output_dir = argv[2];
    std::cout << "using input " << slog_input_dir << std::endl;
    std::cout << "using output " << slog_output_dir << std::endl;
  }
  mpi_comm mcomm;
  mcomm.create(argc, argv);

~a

  
  // Enable IO
  lie->enable_share_io();
  lie->enable_data_IO();
  lie->set_output_dir(slog_output_dir); // Write to this directory
  lie->set_comm(mcomm);
  lie->set_batch_size(1);
  lie->execute();
  lie->print_all_relation_size(); // Continuously print relation sizes

  delete lie;
  mcomm.destroy();
  return 0;
}
