#include "../src/parallel_RA_inc.h"

// builtins.cpp goes here!
~a

// global definitions:
~a

std::string get_stem(const std::filesystem::path &p) { return (p.stem().string()); }

int get_tag_for_rel(std::string& relation_name, std::string& db_dir) {

  std::vector<int> used_tags;
  used_tag.push_back(255);
  for (const auto & entry : std::filesystem::directory_iterator(path))
  {
    // check if ends with table
    std::string filename_s = get_stem(entry);
    std::string suffix = ".table";
    if (filename_s.rfind(suffix) != std::abs(filename_s.size()-suffix.size()))
    {
      continue;
    }
    std::string filename_s = filename_s.substr(0, std::abs(filename_s.size()-suffix.size()));
    std::string rel_name = filename_s.substr(filename_s.lfind(".")+1, filename_s.rfind("."));
    std::string tag_s = filename_s.substr(0, filename_s.lfind("."));
    if (rel_name == relation_name)
    {
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
