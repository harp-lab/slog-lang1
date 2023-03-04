// location of `parallel_RA_inc.h` here
#include "../src/parallel_RA_inc.h"

#include <iostream>
#include <iterator>
#include <sstream>
#include <string>
#include <unordered_set>
#include <map>

// builtins.cpp goes here!
// builtins.cpp
#include <cstddef>
#include <vector>
#include <string>
#include <cassert>
#include <iostream>
#include <array>
#include <functional>
#include <tuple>
#include <functional>

using namespace std;
#define u64  uint64_t
#define u32  uint32_t
using i64 = int64_t;



vector<array<u64,2>> builtin_div_rem(const u64* const data){
  if (is_number(data[0]) && is_number(data[1])){
    auto div = number_to_datum(d2n(data[0]) / d2n(data[1]));
    auto rem = number_to_datum(d2n(data[0]) % d2n(data[1]));
    return {{div, rem}};
  } else {
    return {};
  }
}

#define BUILTIN_BINARY_NUMBER_PRED(name, op) \
template<typename TState> inline TState name(const u64* data, TState init_state, TState (*callback) (TState state)){ \
  if (is_number(data[0]) && is_number(data[1]) &&\
      datum_to_number(data[0]) op datum_to_number(data[1])){\
    return callback(init_state);\
  } else \
    return init_state;\
}

BUILTIN_BINARY_NUMBER_PRED(builtin_less, <)
BUILTIN_BINARY_NUMBER_PRED(builtin_greater, >)
BUILTIN_BINARY_NUMBER_PRED(builtin_le, <=)
BUILTIN_BINARY_NUMBER_PRED(builtin_ge, >=)

#define BUILTIN_BINARY_NUMBER_FUNC(name, op) \
template<typename TState> inline TState name(const u64* data, TState init_state, TState (*callback) (u64 res, TState state)){ \
  if (is_number(data[0]) && is_number(data[1])){\
    auto res = number_to_datum(datum_to_number(data[0]) op datum_to_number(data[1]));\
    return callback(res, init_state);\
} else \
  return init_state;\
}

BUILTIN_BINARY_NUMBER_FUNC(builtin_add, +)
BUILTIN_BINARY_NUMBER_FUNC(builtin_subtract, -)
BUILTIN_BINARY_NUMBER_FUNC(builtin_multiply, *)
BUILTIN_BINARY_NUMBER_FUNC(builtin_divide, /)

#define BUILTIN_BINARY_NUMBER_FUNC2(name, impl) \
template<typename TState> inline TState name(const u64* data, TState init_state, TState (*callback) (u64 res, TState state)){ \
  if (is_number(data[0]) && is_number(data[1])){\
    auto res = number_to_datum(impl(datum_to_number(data[0]), datum_to_number(data[1])));\
    return callback(res, init_state);\
} else \
  return init_state;\
}

inline u64 impl_arg2_minus_arg1(u64 arg1, u64 arg2) {return arg2 - arg1;}
BUILTIN_BINARY_NUMBER_FUNC2(builtin_arg2_minus_arg1, impl_arg2_minus_arg1)


#define BUILTIN_UNARY_NUMBER_FUNC(name, impl) \
template<typename TState> inline TState name(const u64* data, TState init_state, TState (*callback) (u64 res, TState state)){ \
  if (is_number(data[0])){\
    auto res = number_to_datum(impl(datum_to_number(data[0])));\
    return callback(res, init_state);\
} else \
  return init_state;\
}

inline u64 add1(u64 x) {return x + 1;}
inline u64 sub1(u64 x) {return x - 1;}

BUILTIN_UNARY_NUMBER_FUNC(builtin_add1, add1)
BUILTIN_UNARY_NUMBER_FUNC(builtin_add1_2, sub1)
BUILTIN_UNARY_NUMBER_FUNC(builtin_sub1, sub1)
BUILTIN_UNARY_NUMBER_FUNC(builtin_sub1_2, add1)


vector<array<u64,1>> builtin_range(const u64* const data){
  vector<array<u64,1>> res;
  if (is_number(data[0]) && is_number(data[1])){
    auto lb = datum_to_number(data[0]);
    auto ub = datum_to_number(data[1]);
    res.reserve(ub - lb);
    for (u64 x = lb; x < ub; x++)
      res.push_back({number_to_datum(x)});
  } 
  return res;
}

template<typename TState>
TState callback_builtin_range(const u64* data, TState init_state, TState (*callback) (u64 res, TState state)){
  auto state = init_state;
  if (is_number(data[0]) && is_number(data[1])){
    auto lb = datum_to_number(data[0]);
    auto ub = datum_to_number(data[1]);
    for (u64 x = lb; x < ub; x++)
      state = callback(number_to_datum(x), state);
  }
  return state;
}


#define BUILTIN_BINARY_PRED(name, op) \
template<typename TState> TState name(const u64* data, TState init_state, TState (*callback) (TState state)){ \
  if (data[0] op data[1])\
    return callback(init_state);\
  else\
    return init_state;\
}
BUILTIN_BINARY_PRED(builtin_eq, ==)
BUILTIN_BINARY_PRED(builtin_neq, !=)

template<typename TState>
TState builtin_eq_1(const u64* data, TState init_state, TState (*callback) (u64 res, TState state)){
  return callback(data[0], init_state);
}

#define BUILTIN_UNARY_PRED(name, pred) \
template<typename TState> TState name(const u64* data, TState init_state, TState (*callback) (TState state)){ \
  if (pred(data[0]))\
    return callback(init_state);\
  else\
    return init_state;\
}

bool is_not_number(u64 datum) {return !is_number(datum);}
BUILTIN_UNARY_PRED(builtin_number_huh, is_number)
BUILTIN_UNARY_PRED(builtin_not_number_huh, is_not_number)


//an experiment:
template<bool f (u64, u64)>
bool builtin_binary_number_pred(const u64* data){
  if (is_number(data[0]) && is_number(data[1])){
    return f(datum_to_number(data[0]), datum_to_number(data[1]));
  } else {
    return false;
  }
}
bool _less(u64 x, u64 y) { return x < y;}
auto builtin_less2 = builtin_binary_number_pred<_less>;


template<typename TState> inline TState builtin_nop(const u64* data, TState init_state, TState (*callback) (TState state)){ 
  return callback(init_state);
}


//////////////////// AGGREGATORS  ////////////////////
using local_agg_res_t = u64;

local_agg_res_t agg_not_reduce(local_agg_res_t x, local_agg_res_t y) {
  return x | y;
}

template<typename TState> TState agg_not_global(u64* data, local_agg_res_t agg_data, u64 agg_data_count, TState init_state, TState (*callback) (TState state)){
  cout << "agg_not_global called with count: " << agg_data_count << ", and res: " << agg_data << "\n";
  if (agg_data_count > 0 && agg_data)  {
    return init_state;
  } else {
    cout << "agg_not_global: calling callback!" << "\n";
    return callback(init_state);
  }
}

local_agg_res_t agg_sum_reduce(local_agg_res_t x, local_agg_res_t y) {
  return x + y;
}

template<typename TState> TState agg_sum_global(u64* data, local_agg_res_t agg_data, u64 agg_data_count, TState init_state, TState (*callback) (u64 res, TState state)){
  return callback(n2d(agg_data), init_state);
}

///////////////// OLD DESIGN ////////////////////////

// struct _BTree_Iterator {
//   virtual u64* operator*() = 0;
//   virtual bool operator!=(const _BTree_Iterator& rhs) = 0;
//   virtual _BTree_Iterator& operator++() = 0;
// };
// struct _BTree {
//   virtual bool has_key(const u64* key) = 0;
//   virtual _BTree_Iterator& begin() = 0;
//   virtual _BTree_Iterator& end() = 0;
// };

// typedef local_agg_res_t *local_agg_func_t (_BTree* agg_rel, const u64* data);

// typedef local_agg_res_t *reduce_agg_func_t (local_agg_res_t x, local_agg_res_t y);

// typedef int *global_agg_func_t (u64* data, local_agg_res_t agg_data, int agg_data_count, u64* output); 

// parallel_copy_aggregate(relation rel, relation agg_rel, relation target_rel, 
//                              local_agg_func_t local_agg_func, reduce_agg_func_t reduce_agg_func, global_agg_func_t global_agg_fun);


// local_agg_res_t agg_not_1_local_OLD(_BTree* rel, const u64* data){
//   return rel->has_key(data)? (u64) true : (u64) false;
// }

// local_agg_res_t agg_not_2_local_OLD(_BTree* rel, const u64* data){
//   return rel->has_key(data)? (u64) true : (u64) false;
// }


// local_agg_res_t agg_sum_local_OLD(_BTree* rel, const u64* data) {
//   u64 sum = 0;
//   for (auto iter = rel->begin(); iter != rel->end(); iter++){
//     auto current = *iter;
//     if (is_number(*current)) {
//       sum += d2n(*current);
//     }
//   }
//   return sum;
// }

// //////////////////// AGGREGATORS Alternative design ////////////////////

// struct RelDataIterator {
//   virtual u64* operator*() = 0;
//   virtual bool operator!=(const RelDataIterator& rhs) = 0;
//   virtual RelDataIterator& operator++() = 0;
// };

// struct ReLData {
//   virtual RelDataIterator& begin() = 0;
//   virtual RelDataIterator& end() = 0;
// };

// enum class SpecialAggregator {
//   none,
//   sum,
// };
// // typedef local_agg_res_t *local_agg_func_t (ReLData& agg_rel, const u64* data);

// // typedef local_agg_res_t *reduce_agg_func_t (local_agg_res_t x, local_agg_res_t y);

// // typedef int *global_agg_func_t (u64* data, local_agg_res_t agg_data, int agg_data_count, u64* output); 

// // void parallel_copy_aggregate(relation rel, relation agg_rel, relation target_rel, 
// //                              local_agg_func_t local_agg_func, 
// //                              SpecialAggregator special_agg, reduce_agg_func_t reduce_agg_func, 
// //                              global_agg_func_t global_agg_fun);

// local_agg_res_t agg_not_1_local(ReLData& rel, const u64* data){
//   auto has_any = rel.begin() != rel.end();
//   return has_any;
// }

// local_agg_res_t agg_not_2_local(ReLData& rel, const u64* data){
//   return agg_not_1_local(rel, data);
// }

// local_agg_res_t agg_sum_local(ReLData& rel, const u64* data) {
//   u64 sum = 0;
//   for (auto iter = rel.begin(); iter != rel.end(); iter++){
//     auto current = *iter;
//     if (is_number(*current)) {
//       sum += d2n(*current);
//     }
//   }
//   return sum;
// }
// // end of builtins.cpp


// global definitions:


int max_rel = 255;
std::map<std::string, int> rel_tag_map;
std::map<std::string, std::unordered_set<std::string>> rel_index_map;

// load all relation inside input database
void load_input_relation(std::string db_dir)
{
  for (const auto & entry : std::filesystem::directory_iterator(db_dir))
  {
    // check if ends with table
    std::string filename_ss = entry.path().filename().string();
    std::cout << "input database has file " << filename_ss << std::endl;
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
    std::string name = name_arity.substr(0, name_arity.rfind("."));
    std::string arity_s = name_arity.substr(name_arity.rfind(".")+1, name_arity.size());
    int arity = std::stoi(arity_s);
    std::stringstream index_stream;
    index_stream << name;
    for (int i = 1; i <= arity; i++)
    {
      index_stream << "__" <<  i;
    }
    if (tag > max_rel)
      max_rel = tag;
    std::cout << "load " << tag << "." << index_stream.str() << "has arity " << arity << std::endl;
    rel_tag_map[index_stream.str()] = tag;
  }
}

int get_tag_for_rel(std::string relation_name, std::string index_str) {
  std::string name_arity = relation_name + "__" + index_str;
  if (rel_index_map.find(relation_name) != rel_index_map.end()) {
    rel_index_map[relation_name].insert(index_str);
  } else {
    rel_index_map[relation_name] = {index_str};
  }

  if (rel_tag_map.find(name_arity) != rel_tag_map.end())
  {
    // std::cout << "rel: " << name_arity << " " << rel_tag_map[name_arity] << std::endl;
    return rel_tag_map[name_arity];
  }
  max_rel++;
  rel_tag_map[name_arity] = max_rel;
  std::cout << "generate rel tag: " << name_arity << " " << max_rel << std::endl;
  return max_rel;
}

int main(int argc, char **argv)
{
  // input dir from compiler
  std::string slog_input_dir = "../data/tc-input";
  // output dir from compiler
  std::string slog_output_dir = "../data/tc";
  if (argc == 3) {
    slog_input_dir = argv[1];
    slog_output_dir = argv[2];
  }
  load_input_relation(slog_input_dir);
  mpi_comm mcomm;
  mcomm.create(argc, argv);

relation* rel__path__2__1__2 = new relation(2, true, 2, get_tag_for_rel("path","1__2"), std::to_string(get_tag_for_rel("path","1__2")) + ".path.2.table", slog_input_dir + "/" + std::to_string(get_tag_for_rel("path","1__2")) + ".path.2.table", FULL);
relation* rel__edge__2__1__2 = new relation(2, true, 2, get_tag_for_rel("edge","1__2"), std::to_string(get_tag_for_rel("edge","1__2")) + ".edge.2.table", slog_input_dir + "/" + std::to_string(get_tag_for_rel("edge","1__2")) + ".edge.2.table", FULL);
relation* rel__edge__2__1 = new relation(1, false, 2, get_tag_for_rel("edge","1"), std::to_string(get_tag_for_rel("edge","1")) + ".edge.2.table",   FULL);
relation* rel__path__2__2 = new relation(1, false, 2, get_tag_for_rel("path","2"), std::to_string(get_tag_for_rel("path","2")) + ".path.2.table",   FULL);

RAM* scc0 = new RAM(false, 0);
scc0->add_relation(rel__path__2__1__2, true, false);
scc0->add_relation(rel__edge__2__1__2, false, false);
scc0->add_rule(new parallel_copy(rel__path__2__1__2, rel__edge__2__1__2, FULL, {0, 1}));

RAM* scc1 = new RAM(false, 1);
scc1->add_relation(rel__edge__2__1__2, true, false);
scc1->add_relation(rel__edge__2__1, true, false);
scc1->add_rule(new parallel_acopy(rel__edge__2__1, rel__edge__2__1__2, DELTA, {0, 2, 1}));

RAM* scc2 = new RAM(true, 2);
scc2->add_relation(rel__path__2__1__2, true, false);
scc2->add_relation(rel__edge__2__1, false, false);
scc2->add_relation(rel__path__2__2, true, false);
scc2->add_rule(new parallel_acopy(rel__path__2__2, rel__path__2__1__2, DELTA, {1, 2, 0}));
scc2->add_rule(new parallel_join(rel__path__2__1__2, rel__edge__2__1, FULL, rel__path__2__2, DELTA, {4, 2}));

LIE* lie = new LIE();
lie->add_relation(rel__path__2__1__2);
lie->add_relation(rel__edge__2__1__2);
lie->add_relation(rel__edge__2__1);
lie->add_relation(rel__path__2__2);
lie->add_scc(scc0);
lie->add_scc(scc1);
lie->add_scc(scc2);
lie->add_scc_dependance(scc0, scc2);
lie->add_scc_dependance(scc1, scc2);



  
  // Enable IO
  //lie->enable_all_to_all_dump();
  lie->enable_data_IO();
  lie->enable_IO();
  // lie->enable_share_io();
  lie->set_output_dir(slog_output_dir); // Write to this directory
  lie->set_comm(mcomm);
  lie->set_batch_size(1);
  lie->execute();
  lie->print_all_relation_size(); // Continuously print relation sizes 
  lie->stat_intermediate();

  // print all variants(non-canonical index of each relation)
  if (mcomm.get_rank() == 0)
  {
    std::cout << "rel_name" << ",\t" << "indices\n"; 
    for (auto const& rel_p : rel_index_map) {
      std::cout << rel_p.first << ",\t" << rel_p.second.size() << "\n";
    }
    std::cout << std::endl;
  }
  // lie->print_all_relation_size(); // Continuously print relation sizes

  delete lie;
  mcomm.destroy();
  return 0;
}