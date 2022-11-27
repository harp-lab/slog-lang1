// location of `parallel_RA_inc.h` here
#include "/home/ubuntu/workspace/slog/compiler/../backend/src/parallel_RA_inc.h"

#include <bit>
#include <iostream>
#include <iterator>
#include <map>
#include <optional>
#include <sstream>
#include <string>
#include <unordered_set>

// builtins.cpp goes here!
// builtins.cpp
#include <array>
#include <cassert>
#include <cstddef>
#include <functional>
#include <iostream>
#include <limits>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

using namespace std;
#define u64 uint64_t
#define u32 uint32_t
using i64 = int64_t;

const u64 tag_mask = 0xffffc00000000000;
const u64 tag_position = 46;
const u64 int_tag = 0;
const u64 str_tag = 2;
const u64 sign_flip_const = 0x0000200000000000;
const u64 signed_num_mask = 0xFFFFE00000000000;

inline bool is_number(u64 datum) {
  // cout << "is_number(" << datum << "): " << (datum >> tag_position ==
  // int_tag) << "\n";
  return datum >> tag_position == int_tag;
}

inline i64 datum_to_number(u64 datum) {
  i64 signed_val =
      (datum & ~tag_mask) << (64 - tag_position) >> (64 - tag_position);
  if (signed_val >= sign_flip_const) {
    signed_val = sign_flip_const - signed_val;
  }
  return signed_val;
  // return (i64) (datum & ~tag_mask) << (64 - tag_position) >> (64 -
  // tag_position);
}
const auto d2n = datum_to_number;

inline u64 number_to_datum(i64 number) {
  i64 unsigned_value = number;
  if (number < 0) {
    unsigned_value = (-number) + sign_flip_const;
  }
  return (unsigned_value & ~tag_mask) | (int_tag << tag_position);
  // return (number & ~tag_mask) | (int_tag << tag_position);
}

const auto n2d = number_to_datum;

inline u64 string_to_datum(std::string str) {
  u32 str_hash = string_hash(str);
  return (str_hash & ~tag_mask) | (str_tag << tag_position);
}
const auto s2d = string_to_datum;

vector<array<u64, 2>> builtin_div_rem(const u64 *const data) {
  if (is_number(data[0]) && is_number(data[1])) {
    auto div = number_to_datum(d2n(data[0]) / d2n(data[1]));
    auto rem = number_to_datum(d2n(data[0]) % d2n(data[1]));
    return {{div, rem}};
  } else {
    return {};
  }
}

#define BUILTIN_BINARY_NUMBER_PRED(name, op)                                   \
  template <typename TState>                                                   \
  inline TState name(const u64 *data, TState init_state,                       \
                     TState (*callback)(TState state)) {                       \
    if (is_number(data[0]) && is_number(data[1]) &&                            \
        datum_to_number(data[0]) op datum_to_number(data[1])) {                \
      return callback(init_state);                                             \
    } else                                                                     \
      return init_state;                                                       \
  }

BUILTIN_BINARY_NUMBER_PRED(builtin_less, <)
BUILTIN_BINARY_NUMBER_PRED(builtin_greater, >)
BUILTIN_BINARY_NUMBER_PRED(builtin_le, <=)
BUILTIN_BINARY_NUMBER_PRED(builtin_ge, >=)

#define BUILTIN_BINARY_NUMBER_FUNC(name, op)                                   \
  template <typename TState>                                                   \
  inline TState name(const u64 *data, TState init_state,                       \
                     TState (*callback)(u64 res, TState state)) {              \
    if (is_number(data[0]) && is_number(data[1])) {                            \
      auto res = number_to_datum(datum_to_number(data[0])                      \
                                     op datum_to_number(data[1]));             \
      return callback(res, init_state);                                        \
    } else                                                                     \
      return init_state;                                                       \
  }

BUILTIN_BINARY_NUMBER_FUNC(builtin_add, +)
BUILTIN_BINARY_NUMBER_FUNC(builtin_subtract, -)
BUILTIN_BINARY_NUMBER_FUNC(builtin_multiply, *)
BUILTIN_BINARY_NUMBER_FUNC(builtin_divide, /)

#define BUILTIN_BINARY_NUMBER_FUNC2(name, impl)                                \
  template <typename TState>                                                   \
  inline TState name(const u64 *data, TState init_state,                       \
                     TState (*callback)(u64 res, TState state)) {              \
    if (is_number(data[0]) && is_number(data[1])) {                            \
      auto res = number_to_datum(                                              \
          impl(datum_to_number(data[0]), datum_to_number(data[1])));           \
      return callback(res, init_state);                                        \
    } else                                                                     \
      return init_state;                                                       \
  }

inline u64 impl_arg2_minus_arg1(u64 arg1, u64 arg2) { return arg2 - arg1; }
BUILTIN_BINARY_NUMBER_FUNC2(builtin_arg2_minus_arg1, impl_arg2_minus_arg1)

#define BUILTIN_UNARY_NUMBER_FUNC(name, impl)                                  \
  template <typename TState>                                                   \
  inline TState name(const u64 *data, TState init_state,                       \
                     TState (*callback)(u64 res, TState state)) {              \
    if (is_number(data[0])) {                                                  \
      auto res = number_to_datum(impl(datum_to_number(data[0])));              \
      return callback(res, init_state);                                        \
    } else                                                                     \
      return init_state;                                                       \
  }

inline u64 add1(u64 x) { return x + 1; }
inline u64 sub1(u64 x) { return x - 1; }

BUILTIN_UNARY_NUMBER_FUNC(builtin_add1, add1)
BUILTIN_UNARY_NUMBER_FUNC(builtin_add1_2, sub1)
BUILTIN_UNARY_NUMBER_FUNC(builtin_sub1, sub1)
BUILTIN_UNARY_NUMBER_FUNC(builtin_sub1_2, add1)

vector<array<u64, 1>> builtin_range(const u64 *const data) {
  vector<array<u64, 1>> res;
  if (is_number(data[0]) && is_number(data[1])) {
    auto lb = datum_to_number(data[0]);
    auto ub = datum_to_number(data[1]);
    res.reserve(ub - lb);
    for (u64 x = lb; x < ub; x++)
      res.push_back({number_to_datum(x)});
  }
  return res;
}

template <typename TState>
TState callback_builtin_range(const u64 *data, TState init_state,
                              TState (*callback)(u64 res, TState state)) {
  auto state = init_state;
  if (is_number(data[0]) && is_number(data[1])) {
    auto lb = datum_to_number(data[0]);
    auto ub = datum_to_number(data[1]);
    for (u64 x = lb; x < ub; x++)
      state = callback(number_to_datum(x), state);
  }
  return state;
}

#define BUILTIN_BINARY_PRED(name, op)                                          \
  template <typename TState>                                                   \
  TState name(const u64 *data, TState init_state,                              \
              TState (*callback)(TState state)) {                              \
    if (data[0] op data[1])                                                    \
      return callback(init_state);                                             \
    else                                                                       \
      return init_state;                                                       \
  }
BUILTIN_BINARY_PRED(builtin_eq, ==)
BUILTIN_BINARY_PRED(builtin_neq, !=)

template <typename TState>
TState builtin_eq_1(const u64 *data, TState init_state,
                    TState (*callback)(u64 res, TState state)) {
  return callback(data[0], init_state);
}

#define BUILTIN_UNARY_PRED(name, pred)                                         \
  template <typename TState>                                                   \
  TState name(const u64 *data, TState init_state,                              \
              TState (*callback)(TState state)) {                              \
    if (pred(data[0]))                                                         \
      return callback(init_state);                                             \
    else                                                                       \
      return init_state;                                                       \
  }

bool is_not_number(u64 datum) { return !is_number(datum); }
BUILTIN_UNARY_PRED(builtin_number_huh, is_number)
BUILTIN_UNARY_PRED(builtin_not_number_huh, is_not_number)

// for generate-cpp-lambda-for-computational-join
struct CL2CB_State {
  void *original_callback; // There be dragons?
  void *original_state;
  const u64 *original_data;
  u64 *cl1_output_args;
};

// for generate-cpp-lambda-for-computational-copy
struct BCLCB_State {
  void *original_callback;
  void *original_state;
  const u64 *original_data;
};

// an experiment:
template <bool f(u64, u64)> bool builtin_binary_number_pred(const u64 *data) {
  if (is_number(data[0]) && is_number(data[1])) {
    return f(datum_to_number(data[0]), datum_to_number(data[1]));
  } else {
    return false;
  }
}
bool _less(u64 x, u64 y) { return x < y; }
auto builtin_less2 = builtin_binary_number_pred<_less>;

template <typename TState>
inline TState builtin_nop(const u64 *data, TState init_state,
                          TState (*callback)(TState state)) {
  return callback(init_state);
}

// //////////////////// AGGREGATORS Alternative design ////////////////////

// TODO: add number type check
//////////////////////////////  count /////////////////////////////////////

local_agg_res_t
agg_count_local(std::pair<shmap_relation::iterator, shmap_relation::iterator>
                    joined_range) {
  local_agg_res_t cnt = 0;
  for (auto it = joined_range.first; it != joined_range.second; ++it) {
    cnt++;
  }
  return cnt;
}

local_agg_res_t agg_count_reduce(local_agg_res_t x, local_agg_res_t y) {
  return x + y;
}

//////////////////////////////  sum /////////////////////////////////////

local_agg_res_t
agg_sum_local(std::pair<shmap_relation::iterator, shmap_relation::iterator>
                  joined_range) {
  local_agg_res_t sum_res = 0;
  for (shmap_relation::iterator it = joined_range.first;
       it != joined_range.second; ++it) {
    auto tuple = (*it);
    sum_res += tuple[tuple.size() - 2];
  }
  return sum_res;
}

local_agg_res_t agg_sum_reduce(local_agg_res_t x, local_agg_res_t y) {
  return x + y;
}

local_agg_res_t agg_sum_float_local(
    std::pair<shmap_relation::iterator, shmap_relation::iterator>
        joined_range) {
  float sum_res = 0.0;
  for (shmap_relation::iterator it = joined_range.first;
       it != joined_range.second; ++it) {
    auto tuple = (*it);
    u32 agg_column_raw = tuple[tuple.size() - 2];

    sum_res += *reinterpret_cast<float *>(&agg_column_raw);
  }
  // std::cout << ">>>>>>>  " << sum_res << " " <<
  // *reinterpret_cast<u32*>(&sum_res) << std::endl;
  u32 sum_res_encoded = *reinterpret_cast<u32 *>(&sum_res);
  return sum_res_encoded;
}

local_agg_res_t agg_sum_float_reduce(local_agg_res_t x_raw,
                                     local_agg_res_t y_raw) {
  float x = *reinterpret_cast<float *>(&x_raw);
  float y = *reinterpret_cast<float *>(&y_raw);
  float res = x + y;
  // std::cout << res << std::endl;
  u32 res_encoded = *reinterpret_cast<u32 *>(&res);
  return res_encoded;
}

//////////////////////////////  maximum  /////////////////////////////////////

local_agg_res_t
agg_maximum_local(std::pair<shmap_relation::iterator, shmap_relation::iterator>
                      joined_range) {
  local_agg_res_t max_res = 0;
  for (auto it = joined_range.first; it != joined_range.second; ++it) {
    auto tuple = (*it);
    auto current_v = tuple[tuple.size() - 1];
    if (current_v > max_res) {
      max_res = current_v;
    }
  }
  return max_res;
}

local_agg_res_t agg_maximum_reduce(local_agg_res_t x, local_agg_res_t y) {
  if (x > y) {
    return x;
  } else {
    return y;
  }
}

//////////////////////////////  minimum  /////////////////////////////////////

local_agg_res_t
agg_minimum_local(std::pair<shmap_relation::iterator, shmap_relation::iterator>
                      joined_range) {
  local_agg_res_t min_res = std::numeric_limits<u32>::max();
  for (auto it = joined_range.first; it != joined_range.second; ++it) {
    auto tuple = (*it);
    auto current_v = tuple[tuple.size() - 1];
    if (current_v < min_res) {
      min_res = current_v;
    }
  }
  return min_res;
}

local_agg_res_t agg_minimum_reduce(local_agg_res_t x, local_agg_res_t y) {
  if (x < y) {
    return x;
  } else {
    return y;
  }
}

// // end of builtins.cpp

// global definitions:

int max_rel = 255;
std::map<std::string, int> rel_tag_map;
std::map<std::string, std::unordered_set<std::string>> rel_index_map;

// load all relation inside input database
void load_input_relation(std::string db_dir) {
  for (const auto &entry : std::filesystem::directory_iterator(db_dir)) {
    // check if ends with table
    std::string filename_ss = entry.path().filename().string();
    std::cout << "input database has file " << filename_ss << std::endl;
    std::string suffix = ".table";
    int ft = filename_ss.size() - suffix.size();
    if (ft < 0)
      ft = 0;
    if (filename_ss.rfind(suffix) != ft) {
      continue;
    }
    std::string filename_s = entry.path().stem().string();
    int tag = std::stoi(filename_s.substr(0, filename_s.find(".")));
    std::string name_arity = filename_s.substr(
        filename_s.find(".") + 1, filename_s.size() - filename_s.find(".") - 1);
    std::string name = name_arity.substr(0, name_arity.rfind("."));
    std::string arity_s =
        name_arity.substr(name_arity.rfind(".") + 1, name_arity.size());
    int arity = std::stoi(arity_s);
    std::stringstream index_stream;
    index_stream << name;
    for (int i = 1; i <= arity; i++) {
      index_stream << "__" << i;
    }
    if (tag > max_rel)
      max_rel = tag;
    std::cout << "load " << tag << "." << index_stream.str() << "has arity "
              << arity << std::endl;
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

  if (rel_tag_map.find(name_arity) != rel_tag_map.end()) {
    // std::cout << "rel: " << name_arity << " " << rel_tag_map[name_arity] <<
    // std::endl;
    return rel_tag_map[name_arity];
  }
  max_rel++;
  rel_tag_map[name_arity] = max_rel;
  std::cout << "generate rel tag: " << name_arity << " " << max_rel
            << std::endl;
  return max_rel;
}

float ALPHA = 0.85;
u64 total_node_size = 0;

int main(int argc, char **argv) {
  // input dir from compiler
  std::string slog_input_dir = "/home/ubuntu/workspace/slog/out/input-data";
  // output dir from compiler
  std::string slog_output_dir = "/home/ubuntu/workspace/slog/out/checkpoints";
  if (argc == 3) {
    slog_input_dir = argv[1];
    slog_output_dir = argv[2];
  }
  load_input_relation(slog_input_dir);
  mpi_comm mcomm;
  mcomm.create(argc, argv);

  // (edge from to)
  relation *rel__edge__2__1 = new relation(
      1, true, 2, get_tag_for_rel("edge", "1__2"),
      std::to_string(get_tag_for_rel("edge", "1__2")) + ".edge.2.table",
      slog_input_dir + "/" + std::to_string(get_tag_for_rel("edge", "1__2")) +
          ".edge.2.table",
      FULL);

  // >>>>>>>>>>>>>>> compute node size
  // (node x)
  relation *rel__node__1__1 = new relation(
      1, true, 1, get_tag_for_rel("node", "1"),
      std::to_string(get_tag_for_rel("node", "1")) + ".node.1.table",
      slog_input_dir + "/" + std::to_string(get_tag_for_rel("node", "1")) +
          ".node.1.table",
      FULL);

  // (total_node_cnt n)
  relation *rel__total_node_cnt__1__1 =
      new relation(1, true, 2, get_tag_for_rel("total_node_cnt", "1"),
                   std::to_string(get_tag_for_rel("total_node_cnt", "1")) +
                       ".total_node_cnt.1.table",
                   slog_input_dir + "/" +
                       std::to_string(get_tag_for_rel("total_node_cnt", "1")) +
                       ".total_node_cnt.1.table",
                   FULL);

  // helper relation for non-join aggregation
  relation *rel___dollorunit__1__1 = new relation(
      0, true, 1, get_tag_for_rel("$unit", "1"),
      std::to_string(get_tag_for_rel("$unit", "1")) + ".$unit.1.table",
      slog_input_dir + "/" + std::to_string(get_tag_for_rel("$unit", "1")) +
          ".$unit.1.table",
      FULL);

  RAM *scc_helper_fact = new RAM(false, 0);
  scc_helper_fact->add_relation(rel___dollorunit__1__1, true, false);
  scc_helper_fact->add_rule(new fact(rel___dollorunit__1__1, {n2d(0)}));

  // [(node a) (node b) <-- (edge a b)]
  RAM *scc_compute_node = new RAM(false, 1);
  scc_compute_node->add_relation(rel__edge__2__1, false, false);
  scc_compute_node->add_relation(rel__node__1__1, true, false);
  scc_compute_node->add_rule(new parallel_copy_generate(
      rel__node__1__1, rel__edge__2__1, FULL,
      [](const u64 *const data, u64 *const output) -> int {
        output[0] = data[0];
        return 1;
      }));
  scc_compute_node->add_rule(new parallel_copy_generate(
      rel__node__1__1, rel__edge__2__1, FULL,
      [](const u64 *const data, u64 *const output) -> int {
        output[0] = data[1];
        return 1;
      }));

  // (total_node_cnt {count node _})
  RAM *scc_count_nodes = new RAM(false, 2);
  scc_count_nodes->add_relation(rel__node__1__1, false, false);
  scc_count_nodes->add_relation(rel__total_node_cnt__1__1, true, false);
  scc_count_nodes->add_relation(rel___dollorunit__1__1, false, false);
  scc_count_nodes->add_rule(new parallel_join_aggregate(
      rel__total_node_cnt__1__1, rel__node__1__1, rel___dollorunit__1__1, FULL,
      agg_count_local, SpecialAggregator::count, agg_count_reduce, nullptr,
      {2}));

  LIE *cnt_lie = new LIE();
  cnt_lie->add_relation(rel__edge__2__1);
  cnt_lie->add_relation(rel__node__1__1);
  cnt_lie->add_relation(rel___dollorunit__1__1);
  cnt_lie->add_relation(rel__total_node_cnt__1__1);
  cnt_lie->add_scc(scc_helper_fact);
  cnt_lie->add_scc(scc_compute_node);
  cnt_lie->add_scc(scc_count_nodes);
  cnt_lie->add_scc_dependance(scc_helper_fact, scc_count_nodes);
  cnt_lie->add_scc_dependance(scc_compute_node, scc_count_nodes);

  cnt_lie->enable_all_to_all_dump();
  cnt_lie->set_output_dir(slog_output_dir); // Write to this directory
  cnt_lie->set_comm(mcomm);
  cnt_lie->set_batch_size(1);
  cnt_lie->execute();
  cnt_lie->print_all_relation_size(); // Continuously print relation sizes

  // only 1 data in this rel so its safe
  rel__total_node_cnt__1__1->print();

  for (auto &t : rel__total_node_cnt__1__1->get_full()[mcomm.get_rank()]) {
    total_node_size = t[0];
    std::cout << ">>>>>>>>> Number of nodes: " << total_node_size << std::endl;
  }

  // >>>>>>>>>>>>>>> compute page rank
  std::cout << ">>>>>>>>>> Computing pagerank ... " << std::endl;

  rel__edge__2__1->disable_initialization();
  rel__node__1__1->disable_initialization();

  relation *rel__edge__2__2 = new relation(
      1, false, 2, get_tag_for_rel("edge", "2"),
      std::to_string(get_tag_for_rel("edge", "2")) + ".edge.2.table",
      std::to_string(get_tag_for_rel("edge", "2")) + ".edge.2.table", FULL);

  //   matrix edge + successor count
  relation *rel__matrix__3__1 = new relation(
      1, true, 3, get_tag_for_rel("matrix", "1"),
      std::to_string(get_tag_for_rel("matrix", "1")) + ".matrix.3.table",
      std::to_string(get_tag_for_rel("matrix", "1")) + ".matrix.3.table", FULL);

  relation *rel__rank__3__1 = new relation(
      1, true, 3, get_tag_for_rel("rank", "1"),
      std::to_string(get_tag_for_rel("rank", "1")) + ".rank.3.table",
      std::to_string(get_tag_for_rel("rank", "1")) + ".rank.3.table", FULL);

  rel__rank__3__1->set_dependent_column_update(
      {1, 2, 3},
      [](const std::vector<u64> &old_v, const std::vector<u64> &new_v,
         const vector<u64> &nt) -> std::optional<bool> {
        // if (nt[0] == 59 && nt[1] == 58) {
        //   std::cout << "dependent column size " << new_v.size() << std::endl;
        //   std::cout << new_v[0] << " " << new_v[1] << " " << new_v[2] << "
        //   comparing with " << old_v[0] << " " << old_v[1] << " " << old_v[2]
        //   << std::endl;
        // }
        if (new_v[0] != old_v[0]) {
          // std::cout << " www >>>>>>>>>" << std::endl;
          return std::nullopt;
        } else {
          // monotonic
          // assert(new_v[1] > old_v[1]);
          // u32 new_sum_raw = new_v[1];
          // u32 old_sum_raw = old_v[1];
          // float new_sum = *reinterpret_cast<float*>(&new_sum_raw);
          // float old_sum = *reinterpret_cast<float*>(&old_sum_raw);
          // if (new_sum > old_sum) {
          //   std::cout << "new >> " << new_sum << " old >> " << old_sum <<
          //   std::endl;
          // }
          // return new_sum > old_sum;
          return new_v[1] > old_v[1];
          // return true;
        }
      });

  relation *rel__result__2__1__2 = new relation(
      2, true, 2, get_tag_for_rel("result", "1__2"),
      std::to_string(get_tag_for_rel("result", "1__2")) + ".result.2.table",
      std::to_string(get_tag_for_rel("result", "1__2")) + ".result.2.table",
      FULL);

  //

  RAM *scc_copy_edge = new RAM(false, 0);
  scc_copy_edge->add_relation(rel__edge__2__1, false, false);
  scc_copy_edge->add_relation(rel__edge__2__2, true, false);
  scc_copy_edge->add_rule(
      new parallel_acopy(rel__edge__2__2, rel__edge__2__1, FULL, {1, 0, 2}));

  RAM *scc_compute_matrix = new RAM(false, 1);
  scc_compute_matrix->add_relation(rel__edge__2__1, false, false);
  scc_compute_matrix->add_relation(rel__edge__2__2, false, false);
  scc_compute_matrix->add_relation(rel__matrix__3__1, true, false);
  scc_compute_matrix->add_rule(new parallel_join_aggregate(
      rel__matrix__3__1, rel__edge__2__2, rel__edge__2__1, FULL,
      agg_count_local, SpecialAggregator::count, agg_count_reduce, nullptr,
      {0, 1, 3}));

  RAM *scc_init = new RAM(false, 2);
  scc_init->add_relation(rel__matrix__3__1, false, false);
  scc_init->add_relation(rel__rank__3__1, true, false);
  scc_init->add_rule(new parallel_copy_generate(
      rel__rank__3__1, rel__matrix__3__1, FULL,
      [](const u64 *const data, u64 *const output) -> int {
        output[0] = data[0];
        output[1] = data[0];
        // float init_pg_v = (1 - ALPHA) / total_node_size;
        u64 init_pg_v = (u64)(((1 - ALPHA) / total_node_size) * 100000);
        // std::cout << init_pg_v << std::endl;
        // output[2] = *reinterpret_cast<u32*>(&init_pg_v);
        output[2] = init_pg_v;
        return 1;
      }));

  RAM *scc_page_rank = new RAM(true, 3);
  scc_page_rank->add_relation(rel__matrix__3__1, false, false);
  scc_page_rank->add_relation(rel__rank__3__1, true, false);
  parallel_join *rank_join =
      new parallel_join(rel__rank__3__1, rel__matrix__3__1, FULL,
                        rel__rank__3__1, DELTA, {3, 1, 2} // useless
      );
  rank_join->set_generator_func([](const depend_val_t &target_vs,
                                   const std::vector<u64> &input_v,
                                   depend_val_t &res_set) -> bool {
    // float pg_sum = 0.0;
    u64 pg_sum = 0;

    int count = 0;
    for (auto &tv : target_vs) {
      // std::cout << "tagret v >>>>> ";
      // for (auto c: tv) {
      //   std::cout << c << " ";
      // }
      // std::cout << std::endl;
      u32 raw_succ_pg_v = tv[2]; // all columns are u64, cast to u32 first
      // std::cout << ">>>>>>>>>>>>>>> " <<
      // *reinterpret_cast<float*>(&raw_succ_pg_v) << std::endl;
      // auto succ_pg_v = *reinterpret_cast<float*>(&raw_succ_pg_v);
      // if(succ_pg_v == 0) {
      //   // std::cout << ">>>>>>>>>> " << succ_pg_v << std::endl;
      // std::cout << "tagret v >>>>> ";
      // for (auto c: tv) {
      //   std::cout << c << " ";
      // }
      // std::cout << std::endl;
      // }
      if (input_v[2] != 0) {
        // pg_sum += ((ALPHA * succ_pg_v) / input_v[2]);
        pg_sum += (u64)(((u64)(ALPHA * raw_succ_pg_v)) / input_v[2]);
        // if (input_v[1] == 51) {
        //   std::cout << "Sum 51 " << input_v[0] << " with ";
        //   for (auto c: tv) {
        //     std::cout << c << " ";
        //   }
        //   std::cout << " result " << pg_sum << std::endl;
        // }
      }
      count++;
    }
    if (pg_sum == 0) {
      return false;
    }
    if (count == 0) {
      return false;
    }
    std::vector<u64> res_tuple(3, 0);
    res_tuple[0] = input_v[1];
    res_tuple[1] = input_v[0];
    // res_tuple[2] = *reinterpret_cast<u32*>(&pg_sum);
    res_tuple[2] = pg_sum;
    // std::cout << "New tuple >>>>>>> " << pg_sum << std::endl;
    // for (auto c: res_tuple) {
    //   std::cout << c << " ";
    // }
    // std::cout << std::endl;
    res_set.push_back(res_tuple);
    return true;
  });
  scc_page_rank->add_rule(rank_join);

  RAM *scc_result = new RAM(false, 4);
  scc_result->add_relation(rel__rank__3__1, false, false);
  scc_result->add_relation(rel__result__2__1__2, true, false);
  scc_result->add_relation(rel__node__1__1, false, false);
  // scc_result->add_rule(new parallel_join_aggregate(
  //     rel__result__2__1__2, rel__rank__3__1, rel__node__1__1, FULL,
  //     agg_sum_float_local, SpecialAggregator::sum, agg_sum_float_reduce,
  //     nullptr, {0, 2}));
  scc_result->add_rule(new parallel_join_aggregate(
      rel__result__2__1__2, rel__rank__3__1, rel__node__1__1, FULL,
      agg_sum_local, SpecialAggregator::sum, agg_sum_reduce, nullptr, {0, 2}));

  LIE *pg_lie = new LIE();
  pg_lie->add_relation(rel__edge__2__1);
  pg_lie->add_relation(rel__matrix__3__1);
  pg_lie->add_relation(rel__node__1__1);
  pg_lie->add_relation(rel__edge__2__2);
  pg_lie->add_relation(rel__rank__3__1);
  pg_lie->add_relation(rel__result__2__1__2);
  pg_lie->add_scc(scc_copy_edge);
  pg_lie->add_scc(scc_compute_matrix);
  pg_lie->add_scc(scc_init);
  pg_lie->add_scc(scc_page_rank);
  pg_lie->add_scc(scc_result);
  pg_lie->add_scc_dependance(scc_copy_edge, scc_compute_matrix);
  pg_lie->add_scc_dependance(scc_compute_matrix, scc_init);
  pg_lie->add_scc_dependance(scc_init, scc_page_rank);
  pg_lie->add_scc_dependance(scc_page_rank, scc_result);

  // Enable IO
  pg_lie->enable_all_to_all_dump();
  pg_lie->enable_data_IO();
  pg_lie->enable_IO();
  // lie->enable_share_io();
  pg_lie->set_output_dir(slog_output_dir); // Write to this directory
  pg_lie->set_comm(mcomm);
  pg_lie->set_batch_size(1);
  pg_lie->execute();
  pg_lie->print_all_relation_size(); // Continuously print relation sizes
  // lie->stat_intermediate();
  // rel__matrix__3__1->print();
  // rel__rank__3__1->print(
  //   [](const std::vector<u64>& tp){
  //     u32 pg_v = tp[2];
  //     // std::cout << tp[0] << " " << tp[1] << " " <<
  //     *reinterpret_cast<float*>(&pg_v) << std::cout << tp[0] << " " << tp[1]
  //     << " " << pg_v << std::endl;
  //   }
  // );
  rel__result__2__1__2->print([](const std::vector<u64> &tp) {
    u32 pg_v = tp[1];
    // std::cout << tp[0] << " " << *reinterpret_cast<float*>(&pg_v) <<
    std::cout << tp[0] << " " << pg_v * 1.0 / 100000 << std::endl;
  });

  // print all variants(non-canonical index of each relation)
  //   if (mcomm.get_rank() == 0) {
  //     std::cout << "rel_name"
  //               << ",\t"
  //               << "indices\n";
  //     for (auto const &rel_p : rel_index_map) {
  //       std::cout << rel_p.first << ",\t" << rel_p.second.size() << "\n";
  //     }
  //     std::cout << std::endl;
  //   }

  delete pg_lie;

  // >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  mcomm.destroy();

  return 0;
}
