#include "parallel_RA_inc.h"

// builtins.cpp goes here!
~a

// global definitions:
~a

std::vector<int> used_tags;

int get_tag_for_rel(std::string relation_name, std::string db_dir) {
  used_tags.push_back(255);
  for (const auto & entry : std::filesystem::directory_iterator(db_dir))
  {
    // check if ends with table
    std::string filename_ss = entry.path().filename().string();
    std::string suffix = ".table";
    int ft = filename_ss.size()-suffix.size();
    if (ft < 0)
      ft = 0;
    if (filename_ss.rfind(suffix) != ft)
    {
      continue;
    }
    std::string filename_s = entry.path().stem().string();
    std::cout << "filename >>>>>>>>>>> " << filename_s << std::endl;
    std::string rel_name = filename_s.substr(filename_s.find(".")+1, filename_s.rfind(".")-filename_s.find(".")-1);
    std::string tag_s = filename_s.substr(0, filename_s.find("."));
    std::cout << "relation1 >>>>>>>>>> " << rel_name << std::endl;
    std::cout << "relation2 >>>>>>>>>> " << relation_name << std::endl;
    if (rel_name == relation_name)
    {
      std::cout << "relation " << std::stoi(tag_s) << std::endl;
      return std::stoi(tag_s);
    }
    used_tags.push_back(std::stoi(tag_s));
  }
  std::sort(used_tags.begin(), used_tags.end(), std::greater<int>());
  return used_tags[0]+1;
}

int main(int argc, char **argv)
{
  // input dir from compiler
  std::string slog_input_dir = "~a";
  // output dir from compiler
  std::string slog_output_dir = "~a";
  if (argc == 3) {
    slog_input_dir = argv[1];
    slog_output_dir = argv[2];
  }
  mpi_comm mcomm;
  mcomm.create(argc, argv);

~a

  
  // Enable IO
  lie->enable_all_to_all_dump();
  lie->enable_data_IO();
  lie->enable_IO();
  lie->set_output_dir(slog_output_dir); // Write to this directory
  lie->set_comm(mcomm);
  lie->set_batch_size(1);
  lie->execute();
  lie->print_all_relation_size(); // Continuously print relation sizes

  delete lie;
  mcomm.destroy();
  return 0;
}
