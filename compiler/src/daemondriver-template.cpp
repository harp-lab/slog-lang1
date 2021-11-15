#include "parallel_RA_inc.h"

// builtins.cpp goes here!
~a

// global definitions:
~a

int max_rel = 255;
std::map<std::string, int> rel_tag_map;

// load all relation inside input database
void load_input_relation(std::string db_dir)
{
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
    int tag = std::stoi(filename_s.substr(0, filename_s.find(".")));
    std::string name_arity = filename_s.substr(filename_s.find(".")+1, filename_s.size()-filename_s.find(".")-1);
    if (tag > max_rel)
      max_rel = tag;
    rel_tag_map[name_arity] = tag;
  }
}

int get_tag_for_rel(std::string relation_name, int arity) {
  std::string name_arity = relation_name + "." + std::to_string(arity);
  if (rel_tag_map.find(name_arity) != rel_tag_map.end())
  {
    // std::cout << "rel: " << name_arity << rel_tag_map[name_arity] << std::endl;
    return rel_tag_map[name_arity];
  }
  max_rel++;
  rel_tag_map[name_arity] = max_rel;
  // std::cout << "rel: " << name_arity << " " << max_rel << std::endl;
  return max_rel;
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
  load_input_relation(slog_input_dir);
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
