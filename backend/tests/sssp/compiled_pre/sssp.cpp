// location of `parallel_RA_inc.h` here
#include "/home/stargazermiao/workspace/PL/slog/compiler/../backend/src/parallel_RA_inc.h"

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
int start_node = 1;

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
  for (auto it = joined_range.first; it != joined_range.second; ++it) {
    auto tuple = (*it);
    sum_res += tuple[tuple.size() - 1];
  }
  return sum_res;
}

local_agg_res_t agg_sum_reduce(local_agg_res_t x, local_agg_res_t y) {
  return x + y;
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

void compute_sssp_from(mpi_comm& mcomm, int sp, std::string input_dir, std::string output_dir, int argc, char **argv) {
  start_node = sp;
  load_input_relation(input_dir);


  // relation *rel__edge__3__1__2__3 = new relation(
  //     3, true, 3, get_tag_for_rel("edge", "1__2__3"),
  //     std::to_string(get_tag_for_rel("edge", "1__2__3")) + ".edge.3.table",
  //     slog_input_dir + "/" +
  //         std::to_string(get_tag_for_rel("edge", "1__2__3")) +
  //         ".edge.3.table",
  //     FULL);
  // relation* rel__edge__3__1 = new relation(
  //   1, false, 3, get_tag_for_rel("edge","1"),
  //   std::to_string(get_tag_for_rel("edge","1")) + ".edge.3.table",
  //   FULL);

  // // the dependent column must be exclude from hash computation, so join
  // column count is 3 - 1 = 2 relation *rel__spath__3__1__2__3 = new relation(
  //     2, true, 3, get_tag_for_rel("spath", "1__2__3"),
  //     std::to_string(get_tag_for_rel("spath", "1__2__3")) + ".spath.3.table",
  //     slog_input_dir + "/" +
  //         std::to_string(get_tag_for_rel("spath", "1__2__3")) +
  //         ".spath.3.table",
  //     FULL);
  // // set functional dependency for spath
  // rel__spath__3__1__2__3->set_dependent_column_update(
  //   {2, 3},   // len and id column
  //   [](std::vector<u64> old_v, std::vector<u64> new_v) -> std::optional<bool>
  //   {
  //     return new_v[0] < old_v[0];
  //   }
  // );
  // relation* rel__spath__3__2 = new relation(
  //   1, false, 3, get_tag_for_rel("spath","2"),
  //   std::to_string(get_tag_for_rel("spath","2")) + ".spath.3.table",
  //   FULL);
  // rel__spath__3__2->set_dependent_column_update(
  //   {2, 3},
  //   [](std::vector<u64> old_v, std::vector<u64> new_v) -> std::optional<bool>
  //   {
  //     return new_v[0] < old_v[0];
  //   }
  // );

  // RAM* scc0 = new RAM(false, 0);
  // scc0->add_relation(rel__edge__3__1, true, false);
  // scc0->add_relation(rel__edge__3__1__2__3, true, false);
  // scc0->add_rule(new parallel_acopy(rel__edge__3__1, rel__edge__3__1__2__3,
  // DELTA, {0, 3, 1, 2}));

  // RAM *scc1 = new RAM(false, 0);
  // scc1->add_relation(rel__edge__3__1__2__3, false, false);
  // scc1->add_relation(rel__spath__3__1__2__3, true, false);
  // scc1->add_rule(new parallel_copy(rel__spath__3__1__2__3,
  //                                  rel__edge__3__1__2__3, FULL, {0, 1, 2}));

  // RAM *scc2 = new RAM(true, 1);
  // scc2->add_relation(rel__edge__3__1__2__3, false, false);
  // scc2->add_relation(rel__spath__3__2, true, false);
  // scc2->add_relation(rel__spath__3__1__2__3, true, false);
  // //  the order of non join column also need to be carefully arranged
  // because, dependent column
  // //  should always at last
  // scc2->add_rule(new parallel_acopy(
  //   rel__spath__3__2,
  //   rel__spath__3__1__2__3, DELTA,
  //   {1, 0, 2, 3})); // 2, 1, 3, id
  // parallel_join* update_spath_j = new parallel_join(
  //   rel__spath__3__1__2__3,
  //   rel__edge__3__1, FULL,
  //   rel__spath__3__2, DELTA,
  //   {5, 2, 3}// useless
  // );
  // update_spath_j->set_generator_func([](std::vector<u64>& target_v,
  // std::vector<u64>& input_v, u64* res) {
  //   res[0] = target_v[1];
  //   res[1] = input_v[2];
  //   if (res[0] == res[1]) {
  //     res[2] = 0;
  //   } else {
  //     res[2] = target_v[2] + input_v[3];
  //   }
  // });
  // scc2->add_rule(update_spath_j);

  // LIE *lie = new LIE();
  // lie->add_relation(rel__edge__3__1);
  // lie->add_relation(rel__edge__3__1__2__3);
  // lie->add_relation(rel__spath__3__2);
  // lie->add_relation(rel__spath__3__1__2__3);
  // lie->add_scc(scc0);
  // lie->add_scc(scc1);
  // lie->add_scc(scc2);
  // lie->add_scc_dependance(scc0, scc2);
  // lie->add_scc_dependance(scc1, scc2);

  relation *rel__spath__2__1__2 = new relation(
      2, true, 2, get_tag_for_rel("spath", "1__2"),
      std::to_string(get_tag_for_rel("spath", "1__2")) + ".spath.2.table",
      input_dir + "/" + std::to_string(get_tag_for_rel("spath", "1__2")) +
          ".spath.2.table",
      FULL);
  rel__spath__2__1__2->set_dependent_column_update(
    {1, 2},   // len and id column
    [](std::vector<u64> old_v, std::vector<u64> new_v) -> std::optional<bool>
    {
      // std::cout << "Comparing  ";
      // for (auto c : old_v) {
      //   std::cout << c << " ";
      // }
      // std::cout << " <<<<<<  ";
      // for (auto c : new_v) {
      //   std::cout << c << " ";
      // }
      return new_v[0] < old_v[0];
    }
  );
  relation *rel__edge__3__1 = new relation(
      1, false, 3, get_tag_for_rel("edge", "1"),
      std::to_string(get_tag_for_rel("edge", "1")) + ".edge.3.table", FULL);
  relation *rel__edge__3__1__2__3 = new relation(
      3, true, 3, get_tag_for_rel("edge", "1__2__3"),
      std::to_string(get_tag_for_rel("edge", "1__2__3")) + ".edge.3.table",
      input_dir + "/" +
          std::to_string(get_tag_for_rel("edge", "1__2__3")) + ".edge.3.table",
      FULL);
  relation *rel__spath__2__1 = new relation(
      1, false, 2, get_tag_for_rel("spath", "1"),
      std::to_string(get_tag_for_rel("spath", "1")) + ".spath.2.table", FULL);
  rel__spath__2__1->set_dependent_column_update(
    {1, 2},
    [](std::vector<u64> old_v, std::vector<u64> new_v) -> std::optional<bool>
    {
      // std::cout << "Comparing  ";
      // for (auto c : old_v) {
      //   std::cout << c << " ";
      // }
      // std::cout << " <<<<<<  ";
      // for (auto c : new_v) {
      //   std::cout << c << " ";
      // }
      return new_v[0] < old_v[0];
    }
  );

  RAM *scc0 = new RAM(false, 0);
  scc0->add_relation(rel__edge__3__1, true, false);
  scc0->add_relation(rel__edge__3__1__2__3, true, false);
  scc0->add_rule(new parallel_acopy(rel__edge__3__1, rel__edge__3__1__2__3,
                                    DELTA, {0, 3, 1, 2}));

  RAM *scc1 = new RAM(false, 1);
  scc1->add_relation(rel__spath__2__1__2, true, false);
  scc1->add_relation(rel__edge__3__1, false, false);
  scc1->add_rule(new parallel_copy_generate(
      rel__spath__2__1__2, rel__edge__3__1, FULL,
      [](const u64 *const data, u64 *const output) -> int {
        auto args_for_old_bi = std::array<u64, 1>{data[0]};
        using TState = std::tuple<const u64 *, u64 *>;
        TState state = std::make_tuple(data, output);
        auto callback = [](u64 res_0, TState state) -> TState {
          auto [data, output] = state;
          auto head_tuple = output;

          bool compatible = true && res_0 == n2d(start_node);
          if (!compatible)
            return state;

          head_tuple[0] = data[2];
          head_tuple[1] = data[3];
          return std::make_tuple(data, output + 2);
        };
        auto [_, new_ptr] =
            builtin_eq_1<TState>(args_for_old_bi.data(), state, callback);
        auto tuples_count = (new_ptr - output) / 2;
        return tuples_count;
      }));

  RAM *scc2 = new RAM(true, 2);
  scc2->add_relation(rel__spath__2__1__2, true, false);
  scc2->add_relation(rel__edge__3__1, false, false);
  scc2->add_relation(rel__spath__2__1, true, false);
  // scc2->add_rule(new parallel_join(rel__spath__2__1__2, rel__spath__2__1, DELTA,
  //                                  rel__edge__3__1, FULL, {4, 5}));
  parallel_join* update_spath_j = new parallel_join(
    rel__spath__2__1__2,
    rel__edge__3__1, FULL,
    rel__spath__2__1, DELTA,
    {5,4}// useless
  );
  update_spath_j->set_generator_func([](std::vector<u64>& target_v,
  std::vector<u64>& input_v, u64* res) {
    // res[0] = target_v[0];
    res[0] = input_v[2];
    if (res[0] == start_node) {
      res[1] = 0;
    } else {
      res[1] = target_v[1] + input_v[3];
    }
  });
  scc2->add_rule(update_spath_j);
  scc2->add_rule(new parallel_acopy(rel__spath__2__1, rel__spath__2__1__2,
                                    DELTA, {0, 1, 2}));


  LIE *lie = new LIE();
  lie->add_relation(rel__spath__2__1__2);
  lie->add_relation(rel__edge__3__1);
  lie->add_relation(rel__edge__3__1__2__3);
  lie->add_relation(rel__spath__2__1);
  lie->add_scc(scc0);
  lie->add_scc(scc1);
  lie->add_scc(scc2);
  lie->add_scc_dependance(scc0, scc2);
  lie->add_scc_dependance(scc0, scc1);
  lie->add_scc_dependance(scc1, scc2);

  // Enable IO
  lie->enable_all_to_all_dump();
  lie->enable_data_IO();
  lie->enable_IO();
  lie->set_output_dir(output_dir); // Write to this directory
  lie->set_comm(mcomm);
  lie->set_batch_size(1);
  lie->execute();
  lie->print_all_relation_size(); // Continuously print relation sizes
  lie->stat_intermediate();

  // rel__spath__2__1__2->print();
  // rel__spath__2__1->print();
  // rel__edge__3__1->print();
  // rel__edge__3__1__2__3->print();

  // print all variants(non-canonical index of each relation)
  if (mcomm.get_rank() == 0) {
    std::cout << "rel_name"
              << ",\t"
              << "indices\n";
    for (auto const &rel_p : rel_index_map) {
      std::cout << rel_p.first << ",\t" << rel_p.second.size() << "\n";
    }
    std::cout << std::endl;
  }

  // lie->print_all_relation_size(); // Continuously print relation sizes

  delete lie;

}

int main(int argc, char **argv) {
  // input dir from compiler
  std::string slog_input_dir =
      "/home/stargazermiao/workspace/PL/slog/out/input-data";
  // output dir from compiler
  std::string slog_output_dir =
      "/home/stargazermiao/workspace/PL/slog/out/checkpoints";
  if (argc > 2) {
    slog_input_dir = argv[1];
    slog_output_dir = argv[2];
  }
  mpi_comm mcomm;
  mcomm.create(argc, argv);

  compute_sssp_from(mcomm, atoi(argv[3]), slog_input_dir, slog_output_dir, argc, argv);

  mcomm.destroy();
  return 0;
}
