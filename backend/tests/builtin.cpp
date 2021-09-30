#include "../src/parallel_RA_inc.h"

// builtins.cpp goes here!
// builtins.cpp
#include <vector>
#include <string>
#include <cassert>
#include <iostream>
#include <array>
#include <functional>
#include <tuple>

using namespace std;
#define u64  unsigned long long
using i64 = long long;

const u64 tag_mask = 0xffffc00000000000;
const u64 tag_position = 46;
const u64 int_tag = 0;

inline bool is_number(u64 datum) {
  // cout << "is_number(" << datum << "): " << (datum >> tag_position == int_tag) << "\n";
  return datum >> tag_position == int_tag;
}

inline i64 datum_to_number(u64 datum) {
  return (i64) (datum & ~tag_mask) << (64 - tag_position) >> (64 - tag_position);
}
const auto d2n = datum_to_number;

inline u64 number_to_datum(i64 number) {
  return (number & ~tag_mask) | (int_tag << tag_position);
}
const auto n2d = number_to_datum;




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


// for generate-cpp-lambda-for-computational-join
struct CL2CB_State{
  void* original_callback; // There be dragons?
  void* original_state;
  const u64* original_data;
  u64* cl1_output_args;
};

// for generate-cpp-lambda-for-computational-copy
struct BCLCB_State{
  void* original_callback;
  void* original_state;
  const u64* original_data;
};

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

// end of builtins.cpp

int main(int argc, char **argv)
{
  mpi_comm mcomm;
  mcomm.create(argc, argv);

relation* rel__Q__1__1 = new relation(1, true, 1, 257, "rel__Q__1__1", "../data/builtin//Q_1_1.dat", FULL);
relation* rel__R__1__1 = new relation(1, true, 1, 256, "rel__R__1__1", "../data/builtin//R_1_0.dat", FULL);

RAM* scc6281 = new RAM(false, 0);
scc6281->add_relation(rel__R__1__1, false);
scc6281->add_relation(rel__Q__1__1, true);
scc6281->add_rule(new parallel_copy_generate(rel__Q__1__1, rel__R__1__1, FULL, [](const u64* data, u64* const output) -> int{
      auto args_for_old_bi = std::array<u64, 2> {data[0], data[1]};
      //std::cout << "INPUT " << data[0] << " " << data[1] << std::endl;
      using TState = std::tuple<const u64*,u64*>;
      TState state = {data, output};
      auto callback = [](u64 res_0,  TState state) -> TState{
        auto [data, output] = state;
        bool compatible = true;
        if (! compatible) return state;

        auto head_tuple = output;
        head_tuple[0] = res_0;
        return {data, output + 1};
      };
      auto [_,new_ptr] = builtin_add<TState>(args_for_old_bi.data(), state, callback);
      auto tuples_count = (new_ptr - output) / 1;
      //std::cout << "OUTPUT " << output[0] << " " << output[1] << std::endl;
      //std::cout << "tuples_count " << tuples_count << std::endl;
      return tuples_count;
    }));

LIE* lie = new LIE();
lie->add_relation(rel__Q__1__1);
lie->add_relation(rel__R__1__1);
lie->add_scc(scc6281);



  // Enable IO
 lie->enable_share_io();
 lie->set_output_dir("../data/builtin-output"); // Write to this directory
 lie->set_comm(mcomm);
 lie->set_batch_size(1);
 lie->execute();
 lie->print_all_relation_size(); // Continuously print relation sizes


 delete lie;
 mcomm.destroy();
 return 0;
}
