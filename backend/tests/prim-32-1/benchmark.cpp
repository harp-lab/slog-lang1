#include "../../src/parallel_RA_inc.h"

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


// global definitions:


int main(int argc, char **argv)
{
  mpi_comm mcomm;
  mcomm.create(argc, argv);

relation* rel___dollorinter__body52__2__1__2 = new relation(2, true, 2, 282, "rel___dollorinter__body52__2__1__2", "./benchmark-input//$inter-body52_2", FULL);
relation* rel___dollorinter__body55__5__1__2__3__4__5 = new relation(5, true, 5, 399, "rel___dollorinter__body55__5__1__2__3__4__5", "./benchmark-input//$inter-body55_5", FULL);
relation* rel___dollorhead__stratified44__4__3__2__1 = new relation(3, false, 4, 455, "rel___dollorhead__stratified44__4__3__2__1", "./benchmark-input//$head-stratified44_4", FULL);
relation* rel___dollorinter__body38__6__2 = new relation(1, false, 6, 316, "rel___dollorinter__body38__6__2", "./benchmark-input//$inter-body38_6", FULL);
relation* rel___dollorhead__stratified42__4__1__2__3__4 = new relation(4, true, 4, 349, "rel___dollorhead__stratified42__4__1__2__3__4", "./benchmark-input//$head-stratified42_4", FULL);
relation* rel___dollorhead__stratified16__4__1__2__3__4 = new relation(4, true, 4, 406, "rel___dollorhead__stratified16__4__1__2__3__4", "./benchmark-input//$head-stratified16_4", FULL);
relation* rel___dollorinter__head21__3__ = new relation(0, false, 3, 385, "rel___dollorinter__head21__3__", "./benchmark-input//$inter-head21_3", FULL);
relation* rel___dollorinter__body17__2__2 = new relation(1, false, 2, 258, "rel___dollorinter__body17__2__2", "./benchmark-input//$inter-body17_2", FULL);
relation* rel___dollorinter__body44__2__2 = new relation(1, false, 2, 413, "rel___dollorinter__body44__2__2", "./benchmark-input//$inter-body44_2", FULL);
relation* rel___dollorinter__body45__3__3 = new relation(1, false, 3, 435, "rel___dollorinter__body45__3__3", "./benchmark-input//$inter-body45_3", FULL);
relation* rel___dollorhead__stratified12__4__ = new relation(0, false, 4, 283, "rel___dollorhead__stratified12__4__", "./benchmark-input//$head-stratified12_4", FULL);
relation* rel___dollorinter__body31__2__1__2 = new relation(2, true, 2, 306, "rel___dollorinter__body31__2__1__2", "./benchmark-input//$inter-body31_2", FULL);
relation* rel__lambda__arg__list__3__1 = new relation(1, false, 3, 420, "rel__lambda__arg__list__3__1", "./benchmark-input//lambda_arg_list_3", FULL);
relation* rel___dollorinter__body7__4__1__2__3__4 = new relation(4, true, 4, 326, "rel___dollorinter__body7__4__1__2__3__4", "./benchmark-input//$inter-body7_4", FULL);
relation* rel___dollorinter__head22__4__4__3__1 = new relation(3, false, 4, 424, "rel___dollorinter__head22__4__4__3__1", "./benchmark-input//$inter-head22_4", FULL);
relation* rel__if__4__1 = new relation(1, false, 4, 405, "rel__if__4__1", "./benchmark-input//if_4", FULL);
relation* rel___dollorhead__stratified20__4__1__2__3__4 = new relation(4, true, 4, 268, "rel___dollorhead__stratified20__4__1__2__3__4", "./benchmark-input//$head-stratified20_4", FULL);
relation* rel__E__3__1__2__3 = new relation(3, true, 3, 335, "rel__E__3__1__2__3", "./benchmark-input//E_3", FULL);
relation* rel___dollorinter__body6__6__1__2__3__4__5__6 = new relation(6, true, 6, 279, "rel___dollorinter__body6__6__1__2__3__4__5__6", "./benchmark-input//$inter-body6_6", FULL);
relation* rel__callcck__2__2__1 = new relation(2, true, 2, 265, "rel__callcck__2__2__1", "./benchmark-input//callcck_2", FULL);
relation* rel__call__3__1 = new relation(1, false, 3, 287, "rel__call__3__1", "./benchmark-input//call_3", FULL);
relation* rel___dollorhead__stratified36__4__1__2__3__4 = new relation(4, true, 4, 375, "rel___dollorhead__stratified36__4__1__2__3__4", "./benchmark-input//$head-stratified36_4", FULL);
relation* rel___dollorinter__body49__5__1 = new relation(1, false, 5, 383, "rel___dollorinter__body49__5__1", "./benchmark-input//$inter-body49_5", FULL);
relation* rel___dollorinter__head21__3__1__2__3 = new relation(3, true, 3, 385, "rel___dollorinter__head21__3__1__2__3", "./benchmark-input//$inter-head21_3", FULL);
relation* rel__prim1__4__0 = new relation(1, false, 4, 417, "rel__prim1__4__0", "./benchmark-input//prim1_4", FULL);
relation* rel___dollorinter__body49__5__1__2__3__4__5 = new relation(5, true, 5, 383, "rel___dollorinter__body49__5__1__2__3__4__5", "./benchmark-input//$inter-body49_5", FULL);
relation* rel___dollorhead__stratified11__2__1__2 = new relation(2, true, 2, 445, "rel___dollorhead__stratified11__2__1__2", "./benchmark-input//$head-stratified11_2", FULL);
relation* rel___dollornil__0__0 = new relation(1, false, 0, 281, "rel___dollornil__0__0", "./benchmark-input//$nil_0", FULL);
relation* rel___dollorhead__stratified37__4__4__2 = new relation(2, false, 4, 256, "rel___dollorhead__stratified37__4__4__2", "./benchmark-input//$head-stratified37_4", FULL);
relation* rel__vaddr__2__2 = new relation(1, false, 2, 363, "rel__vaddr__2__2", "./benchmark-input//vaddr_2", FULL);
relation* rel___dollorinter__body11__2__1__2 = new relation(2, true, 2, 323, "rel___dollorinter__body11__2__1__2", "./benchmark-input//$inter-body11_2", FULL);
relation* rel___dollorinter__body47__4__3__1 = new relation(2, false, 4, 348, "rel___dollorinter__body47__4__3__1", "./benchmark-input//$inter-body47_4", FULL);
relation* rel___dollorinter__body38__6__1__2__3__4__5__6 = new relation(6, true, 6, 316, "rel___dollorinter__body38__6__1__2__3__4__5__6", "./benchmark-input//$inter-body38_6", FULL);
relation* rel___dollorinter__body40__4__1__2__3__4 = new relation(4, true, 4, 446, "rel___dollorinter__body40__4__1__2__3__4", "./benchmark-input//$inter-body40_4", FULL);
relation* rel__kont__1__0 = new relation(1, false, 1, 382, "rel__kont__1__0", "./benchmark-input//kont_1", FULL);
relation* rel___dollorhead__stratified18__4__4__2 = new relation(2, false, 4, 319, "rel___dollorhead__stratified18__4__4__2", "./benchmark-input//$head-stratified18_4", FULL);
relation* rel___dollorhead__stratified26__3__3__1 = new relation(2, false, 3, 450, "rel___dollorhead__stratified26__3__3__1", "./benchmark-input//$head-stratified26_3", FULL);
relation* rel__ifk__4__1__2__3__4 = new relation(4, true, 4, 366, "rel__ifk__4__1__2__3__4", "./benchmark-input//ifk_4", FULL);
relation* rel___dollorinter__body53__2__2 = new relation(1, false, 2, 320, "rel___dollorinter__body53__2__2", "./benchmark-input//$inter-body53_2", FULL);
relation* rel__if__4__1__2__3__4 = new relation(4, true, 4, 405, "rel__if__4__1__2__3__4", "./benchmark-input//if_4", FULL);
relation* rel___dollornil__0__ = new relation(0, true, 0, 281, "rel___dollornil__0__", "./benchmark-input//$nil_0", FULL);
relation* rel__store__2__1 = new relation(1, false, 2, 437, "rel__store__2__1", "./benchmark-input//store_2", FULL);
relation* rel___dollorhead__stratified25__4__1__2__3__4 = new relation(4, true, 4, 280, "rel___dollorhead__stratified25__4__1__2__3__4", "./benchmark-input//$head-stratified25_4", FULL);
relation* rel__if__4__2 = new relation(1, false, 4, 405, "rel__if__4__2", "./benchmark-input//if_4", FULL);
relation* rel___dollorinter__body90__3__2__1 = new relation(2, false, 3, 307, "rel___dollorinter__body90__3__2__1", "./benchmark-input//$inter-body90_3", FULL);
relation* rel__peek__ctx__3__2__1 = new relation(2, false, 3, 403, "rel__peek__ctx__3__2__1", "./benchmark-input//peek_ctx_3", FULL);
relation* rel___dollorlst__2__1__2 = new relation(2, true, 2, 313, "rel___dollorlst__2__1__2", "./benchmark-input//$lst_2", FULL);
relation* rel___dollorinter__body15__2__1__2 = new relation(2, true, 2, 261, "rel___dollorinter__body15__2__1__2", "./benchmark-input//$inter-body15_2", FULL);
relation* rel___dollorhead__stratified16__4__ = new relation(0, false, 4, 406, "rel___dollorhead__stratified16__4__", "./benchmark-input//$head-stratified16_4", FULL);
relation* rel___dollorinter__body28__4__1 = new relation(1, false, 4, 401, "rel___dollorinter__body28__4__1", "./benchmark-input//$inter-body28_4", FULL);
relation* rel___dollorinter__body39__4__2 = new relation(1, false, 4, 431, "rel___dollorinter__body39__4__2", "./benchmark-input//$inter-body39_4", FULL);
relation* rel___dollorinter__body12__2__1__2 = new relation(2, true, 2, 342, "rel___dollorinter__body12__2__1__2", "./benchmark-input//$inter-body12_2", FULL);
relation* rel___dollorbir__sub6__3__1__2__3 = new relation(3, true, 3, 453, "rel___dollorbir__sub6__3__1__2__3", "./benchmark-input//$bir-sub6_3", FULL);
relation* rel___dollorinter__body30__3__ = new relation(0, false, 3, 390, "rel___dollorinter__body30__3__", "./benchmark-input//$inter-body30_3", FULL);
relation* rel___dollorbir__sub5__4__1 = new relation(1, false, 4, 427, "rel___dollorbir__sub5__4__1", "./benchmark-input//$bir-sub5_4", FULL);
relation* rel___dollorbir__sub2__2__1__2 = new relation(2, true, 2, 295, "rel___dollorbir__sub2__2__1__2", "./benchmark-input//$bir-sub2_2", FULL);
relation* rel___dollorinter__body24__6__1__2__3__4__5__6 = new relation(6, true, 6, 354, "rel___dollorinter__body24__6__1__2__3__4__5__6", "./benchmark-input//$inter-body24_6", FULL);
relation* rel___dollorinter__body32__4__2 = new relation(1, false, 4, 414, "rel___dollorinter__body32__4__2", "./benchmark-input//$inter-body32_4", FULL);
relation* rel__store__2__2 = new relation(1, false, 2, 437, "rel__store__2__2", "./benchmark-input//store_2", FULL);
relation* rel___dollorhead__stratified33__4__ = new relation(0, false, 4, 461, "rel___dollorhead__stratified33__4__", "./benchmark-input//$head-stratified33_4", FULL);
relation* rel___dollorinter__body42__5__1 = new relation(1, false, 5, 288, "rel___dollorinter__body42__5__1", "./benchmark-input//$inter-body42_5", FULL);
relation* rel___dollorinter__body87__3__2 = new relation(1, false, 3, 422, "rel___dollorinter__body87__3__2", "./benchmark-input//$inter-body87_3", FULL);
relation* rel___dollorinter__head17__3__1__2__3 = new relation(3, true, 3, 305, "rel___dollorinter__head17__3__1__2__3", "./benchmark-input//$inter-head17_3", FULL);
relation* rel___dollorinter__body59__2__2 = new relation(1, false, 2, 321, "rel___dollorinter__body59__2__2", "./benchmark-input//$inter-body59_2", FULL);
relation* rel___dollorinter__body14__5__1__2__3__4__5 = new relation(5, true, 5, 441, "rel___dollorinter__body14__5__1__2__3__4__5", "./benchmark-input//$inter-body14_5", FULL);
relation* rel___dollorinter__head11__3__1__2__3 = new relation(3, true, 3, 339, "rel___dollorinter__head11__3__1__2__3", "./benchmark-input//$inter-head11_3", FULL);
relation* rel__prim2__3__1__2__3 = new relation(3, true, 3, 291, "rel__prim2__3__1__2__3", "./benchmark-input//prim2_3", FULL);
relation* rel___dollorinter__body34__3__1__2__3 = new relation(3, true, 3, 396, "rel___dollorinter__body34__3__1__2__3", "./benchmark-input//$inter-body34_3", FULL);
relation* rel__primval__3__3__2__1 = new relation(3, true, 3, 345, "rel__primval__3__3__2__1", "./benchmark-input//primval_3", FULL);
relation* rel___dollorhead__stratified15__2__1__2 = new relation(2, true, 2, 336, "rel___dollorhead__stratified15__2__1__2", "./benchmark-input//$head-stratified15_2", FULL);
relation* rel___dollorinter__head15__5__1__2__3__4__5 = new relation(5, true, 5, 357, "rel___dollorinter__head15__5__1__2__3__4__5", "./benchmark-input//$inter-head15_5", FULL);
relation* rel___dollorinter__body54__4__4 = new relation(1, false, 4, 298, "rel___dollorinter__body54__4__4", "./benchmark-input//$inter-body54_4", FULL);
relation* rel__number__1__0 = new relation(1, false, 1, 343, "rel__number__1__0", "./benchmark-input//number_1", FULL);
relation* rel___dollorinter__head30__5__3__2 = new relation(2, false, 5, 278, "rel___dollorinter__head30__5__3__2", "./benchmark-input//$inter-head30_5", FULL);
relation* rel___dollorhead__stratified43__3__3__2 = new relation(2, false, 3, 384, "rel___dollorhead__stratified43__3__3__2", "./benchmark-input//$head-stratified43_3", FULL);
relation* rel__call__arg__list__3__3 = new relation(1, false, 3, 269, "rel__call__arg__list__3__3", "./benchmark-input//call_arg_list_3", FULL);
relation* rel___dollorinter__head19__5__1__2__3__4__5 = new relation(5, true, 5, 351, "rel___dollorinter__head19__5__1__2__3__4__5", "./benchmark-input//$inter-head19_5", FULL);
relation* rel___dollorinter__body66__2__1 = new relation(1, false, 2, 341, "rel___dollorinter__body66__2__1", "./benchmark-input//$inter-body66_2", FULL);
relation* rel___dollorinter__body82__3__2 = new relation(1, false, 3, 263, "rel___dollorinter__body82__3__2", "./benchmark-input//$inter-body82_3", FULL);
relation* rel___dollorinter__body51__3__1__3 = new relation(2, false, 3, 415, "rel___dollorinter__body51__3__1__3", "./benchmark-input//$inter-body51_3", FULL);
relation* rel___dollorinter__head6__4__ = new relation(0, false, 4, 322, "rel___dollorinter__head6__4__", "./benchmark-input//$inter-head6_4", FULL);
relation* rel___dollorinter__head29__3__ = new relation(0, false, 3, 439, "rel___dollorinter__head29__3__", "./benchmark-input//$inter-head29_3", FULL);
relation* rel___dollorhead__stratified19__3__1__2__3 = new relation(3, true, 3, 330, "rel___dollorhead__stratified19__3__1__2__3", "./benchmark-input//$head-stratified19_3", FULL);
relation* rel__prim2__3__3__2__1 = new relation(3, true, 3, 291, "rel__prim2__3__3__2__1", "./benchmark-input//prim2_3", FULL);
relation* rel__ifk__4__0 = new relation(1, false, 4, 366, "rel__ifk__4__0", "./benchmark-input//ifk_4", FULL);
relation* rel___dollorinter__head__5__1__2 = new relation(2, false, 5, 262, "rel___dollorinter__head__5__1__2", "./benchmark-input//$inter-head_5", FULL);
relation* rel___dollorinter__body23__3__3__2 = new relation(2, false, 3, 292, "rel___dollorinter__body23__3__3__2", "./benchmark-input//$inter-body23_3", FULL);
relation* rel__argk__4__1__2__3__4 = new relation(4, true, 4, 314, "rel__argk__4__1__2__3__4", "./benchmark-input//argk_4", FULL);
relation* rel___dollorhead__stratified38__4__4__3__1 = new relation(3, false, 4, 379, "rel___dollorhead__stratified38__4__4__3__1", "./benchmark-input//$head-stratified38_4", FULL);
relation* rel__setb__3__3 = new relation(1, false, 3, 359, "rel__setb__3__3", "./benchmark-input//setb_3", FULL);
relation* rel___dollorinter__body61__4__1__2__3__4 = new relation(4, true, 4, 407, "rel___dollorinter__body61__4__1__2__3__4", "./benchmark-input//$inter-body61_4", FULL);
relation* rel___dollorinter__body82__3__1__2__3 = new relation(3, true, 3, 263, "rel___dollorinter__body82__3__1__2__3", "./benchmark-input//$inter-body82_3", FULL);
relation* rel__lambda__3__1__2__3 = new relation(3, true, 3, 395, "rel__lambda__3__1__2__3", "./benchmark-input//lambda_3", FULL);
relation* rel___dollorinter__body26__2__1__2 = new relation(2, true, 2, 356, "rel___dollorinter__body26__2__1__2", "./benchmark-input//$inter-body26_2", FULL);
relation* rel___dollorinter__head18__5__5__3__1 = new relation(3, false, 5, 440, "rel___dollorinter__head18__5__5__3__1", "./benchmark-input//$inter-head18_5", FULL);
relation* rel___dollorbir__sub3__4__1__2__3__4 = new relation(4, true, 4, 409, "rel___dollorbir__sub3__4__1__2__3__4", "./benchmark-input//$bir-sub3_4", FULL);
relation* rel__if__4__3 = new relation(1, false, 4, 405, "rel__if__4__3", "./benchmark-input//if_4", FULL);
relation* rel___dollorinter__body50__3__1 = new relation(1, false, 3, 452, "rel___dollorinter__body50__3__1", "./benchmark-input//$inter-body50_3", FULL);
relation* rel___dollorhead__stratified35__4__4__2 = new relation(2, false, 4, 389, "rel___dollorhead__stratified35__4__4__2", "./benchmark-input//$head-stratified35_4", FULL);
relation* rel___dollorinter__body23__3__1__2__3 = new relation(3, true, 3, 292, "rel___dollorinter__body23__3__1__2__3", "./benchmark-input//$inter-body23_3", FULL);
relation* rel___dollorinter__body41__2__1 = new relation(1, false, 2, 388, "rel___dollorinter__body41__2__1", "./benchmark-input//$inter-body41_2", FULL);
relation* rel___dollorinter__head5__4__1__2__3__4 = new relation(4, true, 4, 425, "rel___dollorinter__head5__4__1__2__3__4", "./benchmark-input//$inter-head5_4", FULL);
relation* rel___dollorinter__body21__5__1__2__3__4__5 = new relation(5, true, 5, 266, "rel___dollorinter__body21__5__1__2__3__4__5", "./benchmark-input//$inter-body21_5", FULL);
relation* rel___dollorinter__head20__3__1__2__3 = new relation(3, true, 3, 423, "rel___dollorinter__head20__3__1__2__3", "./benchmark-input//$inter-head20_3", FULL);
relation* rel___dollorinter__body75__3__1__2__3 = new relation(3, true, 3, 433, "rel___dollorinter__body75__3__1__2__3", "./benchmark-input//$inter-body75_3", FULL);
relation* rel__lambda__3__2 = new relation(1, false, 3, 395, "rel__lambda__3__2", "./benchmark-input//lambda_3", FULL);
relation* rel___dollorbir__sub4__3__1 = new relation(1, false, 3, 344, "rel___dollorbir__sub4__3__1", "./benchmark-input//$bir-sub4_3", FULL);
relation* rel___dollorinter__head3__4__4__2__1 = new relation(3, false, 4, 378, "rel___dollorinter__head3__4__4__2__1", "./benchmark-input//$inter-head3_4", FULL);
relation* rel___dollorhead__stratified28__2__ = new relation(0, false, 2, 429, "rel___dollorhead__stratified28__2__", "./benchmark-input//$head-stratified28_2", FULL);
relation* rel___dollorinter__body46__4__4 = new relation(1, false, 4, 325, "rel___dollorinter__body46__4__4", "./benchmark-input//$inter-body46_4", FULL);
relation* rel___dollorinter__body69__2__1 = new relation(1, false, 2, 302, "rel___dollorinter__body69__2__1", "./benchmark-input//$inter-body69_2", FULL);
relation* rel__flow__ae__2__1__2 = new relation(2, true, 2, 259, "rel__flow__ae__2__1__2", "./benchmark-input//flow_ae_2", FULL);
relation* rel___dollorinter__head15__5__1__2 = new relation(2, false, 5, 357, "rel___dollorinter__head15__5__1__2", "./benchmark-input//$inter-head15_5", FULL);
relation* rel___dollorinter__body86__4__4__1 = new relation(2, false, 4, 460, "rel___dollorinter__body86__4__4__1", "./benchmark-input//$inter-body86_4", FULL);
relation* rel___dollorinter__body73__3__1__2__3 = new relation(3, true, 3, 418, "rel___dollorinter__body73__3__1__2__3", "./benchmark-input//$inter-body73_3", FULL);
relation* rel___dollorhead__stratified21__3__1__2__3 = new relation(3, true, 3, 297, "rel___dollorhead__stratified21__3__1__2__3", "./benchmark-input//$head-stratified21_3", FULL);
relation* rel__here__5__1__2__3__4__5 = new relation(5, true, 5, 270, "rel__here__5__1__2__3__4__5", "./benchmark-input//here_5", FULL);
relation* rel__kaddr__2__2__1 = new relation(2, true, 2, 365, "rel__kaddr__2__2__1", "./benchmark-input//kaddr_2", FULL);
relation* rel___dollorinter__body65__3__ = new relation(0, false, 3, 454, "rel___dollorinter__body65__3__", "./benchmark-input//$inter-body65_3", FULL);
relation* rel___dollorinter__body69__2__1__2 = new relation(2, true, 2, 302, "rel___dollorinter__body69__2__1__2", "./benchmark-input//$inter-body69_2", FULL);
relation* rel___dollorinter__body89__3__3__2 = new relation(2, false, 3, 347, "rel___dollorinter__body89__3__3__2", "./benchmark-input//$inter-body89_3", FULL);
relation* rel___dollorinter__body63__2__1 = new relation(1, false, 2, 369, "rel___dollorinter__body63__2__1", "./benchmark-input//$inter-body63_2", FULL);
relation* rel___dollorinter__body77__3__3__2 = new relation(2, false, 3, 426, "rel___dollorinter__body77__3__3__2", "./benchmark-input//$inter-body77_3", FULL);
relation* rel__store__2__1__2 = new relation(2, true, 2, 437, "rel__store__2__1__2", "./benchmark-input//store_2", FULL);
relation* rel___dollorinter__head4__3__ = new relation(0, false, 3, 327, "rel___dollorinter__head4__3__", "./benchmark-input//$inter-head4_3", FULL);
relation* rel___dollorinter__body67__3__1__2__3 = new relation(3, true, 3, 397, "rel___dollorinter__body67__3__1__2__3", "./benchmark-input//$inter-body67_3", FULL);
relation* rel___dollorbir__sub2__2__1 = new relation(1, false, 2, 295, "rel___dollorbir__sub2__2__1", "./benchmark-input//$bir-sub2_2", FULL);
relation* rel__argk__4__1 = new relation(1, false, 4, 314, "rel__argk__4__1", "./benchmark-input//argk_4", FULL);
relation* rel___dollorinter__head13__6__5__2__1__3 = new relation(4, false, 6, 380, "rel___dollorinter__head13__6__5__2__1__3", "./benchmark-input//$inter-head13_6", FULL);
relation* rel___dollorinter__body51__3__1__2__3 = new relation(3, true, 3, 415, "rel___dollorinter__body51__3__1__2__3", "./benchmark-input//$inter-body51_3", FULL);
relation* rel___dollorinter__body36__3__1__2__3 = new relation(3, true, 3, 381, "rel___dollorinter__body36__3__1__2__3", "./benchmark-input//$inter-body36_3", FULL);
relation* rel__kaddr__2__1__2 = new relation(2, true, 2, 365, "rel__kaddr__2__1__2", "./benchmark-input//kaddr_2", FULL);
relation* rel___dollorinter__body89__3__1__2__3 = new relation(3, true, 3, 347, "rel___dollorinter__body89__3__1__2__3", "./benchmark-input//$inter-body89_3", FULL);
relation* rel___dollorinter__body29__2__1__2 = new relation(2, true, 2, 387, "rel___dollorinter__body29__2__1__2", "./benchmark-input//$inter-body29_2", FULL);
relation* rel___dollorinter__body68__2__1 = new relation(1, false, 2, 317, "rel___dollorinter__body68__2__1", "./benchmark-input//$inter-body68_2", FULL);
relation* rel__E__3__1 = new relation(1, false, 3, 335, "rel__E__3__1", "./benchmark-input//E_3", FULL);
relation* rel___dollorinter__body65__3__1__2__3 = new relation(3, true, 3, 454, "rel___dollorinter__body65__3__1__2__3", "./benchmark-input//$inter-body65_3", FULL);
relation* rel___dollorinter__body53__2__1__2 = new relation(2, true, 2, 320, "rel___dollorinter__body53__2__1__2", "./benchmark-input//$inter-body53_2", FULL);
relation* rel___dollorinter__body5__7__1__2__3__4__5__6__7 = new relation(7, true, 7, 447, "rel___dollorinter__body5__7__1__2__3__4__5__6__7", "./benchmark-input//$inter-body5_7", FULL);
relation* rel___dollorinter__body56__3__2__3 = new relation(2, false, 3, 444, "rel___dollorinter__body56__3__2__3", "./benchmark-input//$inter-body56_3", FULL);
relation* rel___dollorinter__body39__4__1__2__3__4 = new relation(4, true, 4, 431, "rel___dollorinter__body39__4__1__2__3__4", "./benchmark-input//$inter-body39_4", FULL);
relation* rel__store__2__2__1 = new relation(2, true, 2, 437, "rel__store__2__2__1", "./benchmark-input//store_2", FULL);
relation* rel___dollorinter__head25__4__1__2__3__4 = new relation(4, true, 4, 352, "rel___dollorinter__head25__4__1__2__3__4", "./benchmark-input//$inter-head25_4", FULL);
relation* rel___dollorbir__sub__2__1__2 = new relation(2, true, 2, 272, "rel___dollorbir__sub__2__1__2", "./benchmark-input//$bir-sub_2", FULL);
relation* rel___dollorinter__body9__3__1__2__3 = new relation(3, true, 3, 391, "rel___dollorinter__body9__3__1__2__3", "./benchmark-input//$inter-body9_3", FULL);
relation* rel___dollorinter__body8__4__1__2__3__4 = new relation(4, true, 4, 311, "rel___dollorinter__body8__4__1__2__3__4", "./benchmark-input//$inter-body8_4", FULL);
relation* rel___dollorhead__stratified28__2__1__2 = new relation(2, true, 2, 429, "rel___dollorhead__stratified28__2__1__2", "./benchmark-input//$head-stratified28_2", FULL);
relation* rel___dollorinter__body46__4__1__2__3__4 = new relation(4, true, 4, 325, "rel___dollorinter__body46__4__1__2__3__4", "./benchmark-input//$inter-body46_4", FULL);
relation* rel__let__3__2 = new relation(1, false, 3, 271, "rel__let__3__2", "./benchmark-input//let_3", FULL);
relation* rel___dollorinter__body19__3__3__1 = new relation(2, false, 3, 392, "rel___dollorinter__body19__3__3__1", "./benchmark-input//$inter-body19_3", FULL);
relation* rel__var__2__1__2 = new relation(2, true, 2, 430, "rel__var__2__1__2", "./benchmark-input//var_2", FULL);
relation* rel___dollorinter__body26__2__1 = new relation(1, false, 2, 356, "rel___dollorinter__body26__2__1", "./benchmark-input//$inter-body26_2", FULL);
relation* rel___dollorinter__head26__3__1__2__3 = new relation(3, true, 3, 304, "rel___dollorinter__head26__3__1__2__3", "./benchmark-input//$inter-head26_3", FULL);
relation* rel__let__list__3__1__2__3 = new relation(3, true, 3, 393, "rel__let__list__3__1__2__3", "./benchmark-input//let_list_3", FULL);
relation* rel___dollorinter__head28__3__1__2__3 = new relation(3, true, 3, 303, "rel___dollorinter__head28__3__1__2__3", "./benchmark-input//$inter-head28_3", FULL);
relation* rel__lambda__3__1 = new relation(1, false, 3, 395, "rel__lambda__3__1", "./benchmark-input//lambda_3", FULL);
relation* rel___dollorhead__stratified43__3__1__2__3 = new relation(3, true, 3, 384, "rel___dollorhead__stratified43__3__1__2__3", "./benchmark-input//$head-stratified43_3", FULL);
relation* rel___dollorhead__stratified18__4__1__2__3__4 = new relation(4, true, 4, 319, "rel___dollorhead__stratified18__4__1__2__3__4", "./benchmark-input//$head-stratified18_4", FULL);
relation* rel___dollorinter__body58__2__2 = new relation(1, false, 2, 260, "rel___dollorinter__body58__2__2", "./benchmark-input//$inter-body58_2", FULL);
relation* rel__flow__ea__2__1__2 = new relation(2, true, 2, 410, "rel__flow__ea__2__1__2", "./benchmark-input//flow_ea_2", FULL);
relation* rel__copy__ctx__3__3__2__1 = new relation(3, true, 3, 377, "rel__copy__ctx__3__3__2__1", "./benchmark-input//copy_ctx_3", FULL);
relation* rel___dollorinter__head1__7__6__2 = new relation(2, false, 7, 376, "rel___dollorinter__head1__7__6__2", "./benchmark-input//$inter-head1_7", FULL);
relation* rel___dollorinter__body81__2__ = new relation(0, false, 2, 276, "rel___dollorinter__body81__2__", "./benchmark-input//$inter-body81_2", FULL);
relation* rel___dollorinter__body64__1__1 = new relation(1, true, 1, 300, "rel___dollorinter__body64__1__1", "./benchmark-input//$inter-body64_1", FULL);
relation* rel___dollorinter__body58__2__1__2 = new relation(2, true, 2, 260, "rel___dollorinter__body58__2__1__2", "./benchmark-input//$inter-body58_2", FULL);
relation* rel___dollorhead__stratified44__4__1__2__3__4 = new relation(4, true, 4, 455, "rel___dollorhead__stratified44__4__1__2__3__4", "./benchmark-input//$head-stratified44_4", FULL);
relation* rel___dollorlst__2__2__1 = new relation(2, true, 2, 313, "rel___dollorlst__2__2__1", "./benchmark-input//$lst_2", FULL);
relation* rel___dollorinter__body45__3__1__2__3 = new relation(3, true, 3, 435, "rel___dollorinter__body45__3__1__2__3", "./benchmark-input//$inter-body45_3", FULL);
relation* rel___dollorinter__body52__2__2 = new relation(1, false, 2, 282, "rel___dollorinter__body52__2__2", "./benchmark-input//$inter-body52_2", FULL);
relation* rel___dollorhead__stratified__4__1__2__3__4 = new relation(4, true, 4, 315, "rel___dollorhead__stratified__4__1__2__3__4", "./benchmark-input//$head-stratified_4", FULL);
relation* rel___dollorinter__body30__3__1__2__3 = new relation(3, true, 3, 390, "rel___dollorinter__body30__3__1__2__3", "./benchmark-input//$inter-body30_3", FULL);
relation* rel___dollorhead__stratified19__3__3__2 = new relation(2, false, 3, 330, "rel___dollorhead__stratified19__3__3__2", "./benchmark-input//$head-stratified19_3", FULL);
relation* rel___dollorinter__body48__3__3__1 = new relation(2, false, 3, 338, "rel___dollorinter__body48__3__3__1", "./benchmark-input//$inter-body48_3", FULL);
relation* rel___dollorinter__body25__8__2 = new relation(1, false, 8, 373, "rel___dollorinter__body25__8__2", "./benchmark-input//$inter-body25_8", FULL);
relation* rel___dollorinter__head9__4__1__2__3__4 = new relation(4, true, 4, 456, "rel___dollorinter__head9__4__1__2__3__4", "./benchmark-input//$inter-head9_4", FULL);
relation* rel___dollorinter__head16__3__1__2__3 = new relation(3, true, 3, 402, "rel___dollorinter__head16__3__1__2__3", "./benchmark-input//$inter-head16_3", FULL);
relation* rel___dollorinter__head18__5__1__2__3__4__5 = new relation(5, true, 5, 440, "rel___dollorinter__head18__5__1__2__3__4__5", "./benchmark-input//$inter-head18_5", FULL);
relation* rel___dollorinter__body47__4__1__2__3__4 = new relation(4, true, 4, 348, "rel___dollorinter__body47__4__1__2__3__4", "./benchmark-input//$inter-body47_4", FULL);
relation* rel__prim__call__3__1__2__3 = new relation(3, true, 3, 257, "rel__prim__call__3__1__2__3", "./benchmark-input//prim_call_3", FULL);
relation* rel___dollorinter__body74__4__1__2__3__4 = new relation(4, true, 4, 368, "rel___dollorinter__body74__4__1__2__3__4", "./benchmark-input//$inter-body74_4", FULL);
relation* rel___dollorinter__body50__3__1__2__3 = new relation(3, true, 3, 452, "rel___dollorinter__body50__3__1__2__3", "./benchmark-input//$inter-body50_3", FULL);
relation* rel__peek__ctx__3__2__3__1 = new relation(3, true, 3, 403, "rel__peek__ctx__3__2__3__1", "./benchmark-input//peek_ctx_3", FULL);
relation* rel___dollorinter__head25__4__4__3__1 = new relation(3, false, 4, 352, "rel___dollorinter__head25__4__4__3__1", "./benchmark-input//$inter-head25_4", FULL);
relation* rel___dollorinter__body72__1__1 = new relation(1, true, 1, 458, "rel___dollorinter__body72__1__1", "./benchmark-input//$inter-body72_1", FULL);
relation* rel___dollorinter__body62__2__1__2 = new relation(2, true, 2, 428, "rel___dollorinter__body62__2__1__2", "./benchmark-input//$inter-body62_2", FULL);
relation* rel___dollorinter__body2__4__3 = new relation(1, false, 4, 274, "rel___dollorinter__body2__4__3", "./benchmark-input//$inter-body2_4", FULL);
relation* rel__letk__4__4__3__2__1 = new relation(4, true, 4, 442, "rel__letk__4__4__3__2__1", "./benchmark-input//letk_4", FULL);
relation* rel___dollorinter__head17__3__ = new relation(0, false, 3, 305, "rel___dollorinter__head17__3__", "./benchmark-input//$inter-head17_3", FULL);
relation* rel___dollorinter__body1__3__3__1 = new relation(2, false, 3, 309, "rel___dollorinter__body1__3__3__1", "./benchmark-input//$inter-body1_3", FULL);
relation* rel___dollorhead__stratified3__2__1__2 = new relation(2, true, 2, 416, "rel___dollorhead__stratified3__2__1__2", "./benchmark-input//$head-stratified3_2", FULL);
relation* rel___dollorinter__head7__6__1__2__3__4__5__6 = new relation(6, true, 6, 264, "rel___dollorinter__head7__6__1__2__3__4__5__6", "./benchmark-input//$inter-head7_6", FULL);
relation* rel___dollorhead__stratified2__7__1__2__3__4__5__6__7 = new relation(7, true, 7, 328, "rel___dollorhead__stratified2__7__1__2__3__4__5__6__7", "./benchmark-input//$head-stratified2_7", FULL);
relation* rel___dollorbir__sub1__4__1__2__3__4 = new relation(4, true, 4, 432, "rel___dollorbir__sub1__4__1__2__3__4", "./benchmark-input//$bir-sub1_4", FULL);
relation* rel___dollorinter__head31__3__2__1 = new relation(2, false, 3, 411, "rel___dollorinter__head31__3__2__1", "./benchmark-input//$inter-head31_3", FULL);
relation* rel___dollorinter__body34__3__ = new relation(0, false, 3, 396, "rel___dollorinter__body34__3__", "./benchmark-input//$inter-body34_3", FULL);
relation* rel__flow__ee__2__1__2 = new relation(2, true, 2, 275, "rel__flow__ee__2__1__2", "./benchmark-input//flow_ee_2", FULL);
relation* rel___dollorinter__head10__3__3__2 = new relation(2, false, 3, 337, "rel___dollorinter__head10__3__3__2", "./benchmark-input//$inter-head10_3", FULL);
relation* rel___dollorinter__body86__4__1__2__3__4 = new relation(4, true, 4, 460, "rel___dollorinter__body86__4__1__2__3__4", "./benchmark-input//$inter-body86_4", FULL);
relation* rel___dollorinter__head3__4__1__2__3__4 = new relation(4, true, 4, 378, "rel___dollorinter__head3__4__1__2__3__4", "./benchmark-input//$inter-head3_4", FULL);
relation* rel___dollorinter__body5__7__3__6 = new relation(2, false, 7, 447, "rel___dollorinter__body5__7__3__6", "./benchmark-input//$inter-body5_7", FULL);
relation* rel___dollorbir__sub__2__2__1 = new relation(2, true, 2, 272, "rel___dollorbir__sub__2__2__1", "./benchmark-input//$bir-sub_2", FULL);
relation* rel___dollorinter__head27__6__6__3__2__1 = new relation(4, false, 6, 299, "rel___dollorinter__head27__6__6__3__2__1", "./benchmark-input//$inter-head27_6", FULL);
relation* rel___dollorinter__body7__4__3 = new relation(1, false, 4, 326, "rel___dollorinter__body7__4__3", "./benchmark-input//$inter-body7_4", FULL);
relation* rel__primval__3__1__2__3 = new relation(3, true, 3, 345, "rel__primval__3__1__2__3", "./benchmark-input//primval_3", FULL);
relation* rel___dollorinter__body19__3__1__2__3 = new relation(3, true, 3, 392, "rel___dollorinter__body19__3__1__2__3", "./benchmark-input//$inter-body19_3", FULL);
relation* rel__if__4__4 = new relation(1, false, 4, 405, "rel__if__4__4", "./benchmark-input//if_4", FULL);
relation* rel___dollorinter__head19__5__1__2 = new relation(2, false, 5, 351, "rel___dollorinter__head19__5__1__2", "./benchmark-input//$inter-head19_5", FULL);
relation* rel__callcc__2__1 = new relation(1, false, 2, 353, "rel__callcc__2__1", "./benchmark-input//callcc_2", FULL);
relation* rel__setb__3__1 = new relation(1, false, 3, 359, "rel__setb__3__1", "./benchmark-input//setb_3", FULL);
relation* rel__top__exp__1__ = new relation(0, false, 1, 457, "rel__top__exp__1__", "./benchmark-input//top_exp_1", FULL);
relation* rel___dollorinter__head1__7__1__2__3__4__5__6__7 = new relation(7, true, 7, 376, "rel___dollorinter__head1__7__1__2__3__4__5__6__7", "./benchmark-input//$inter-head1_7", FULL);
relation* rel___dollorinter__body8__4__3__2 = new relation(2, false, 4, 311, "rel___dollorinter__body8__4__3__2", "./benchmark-input//$inter-body8_4", FULL);
relation* rel___dollorinter__body42__5__1__2__3__4__5 = new relation(5, true, 5, 288, "rel___dollorinter__body42__5__1__2__3__4__5", "./benchmark-input//$inter-body42_5", FULL);
relation* rel___dollorinter__body75__3__2 = new relation(1, false, 3, 433, "rel___dollorinter__body75__3__2", "./benchmark-input//$inter-body75_3", FULL);
relation* rel___dollorlst__2__0 = new relation(1, false, 2, 313, "rel___dollorlst__2__0", "./benchmark-input//$lst_2", FULL);
relation* rel___dollorinter__head12__4__1__2__3__4 = new relation(4, true, 4, 421, "rel___dollorinter__head12__4__1__2__3__4", "./benchmark-input//$inter-head12_4", FULL);
relation* rel___dollorhead__stratified45__2__ = new relation(0, false, 2, 301, "rel___dollorhead__stratified45__2__", "./benchmark-input//$head-stratified45_2", FULL);
relation* rel___dollorinter__body54__4__1__2__3__4 = new relation(4, true, 4, 298, "rel___dollorinter__body54__4__1__2__3__4", "./benchmark-input//$inter-body54_4", FULL);
relation* rel___dollorbir__sub5__4__1__2__3__4 = new relation(4, true, 4, 427, "rel___dollorbir__sub5__4__1__2__3__4", "./benchmark-input//$bir-sub5_4", FULL);
relation* rel___dollorinter__body67__3__3 = new relation(1, false, 3, 397, "rel___dollorinter__body67__3__3", "./benchmark-input//$inter-body67_3", FULL);
relation* rel___dollorinter__body61__4__4 = new relation(1, false, 4, 407, "rel___dollorinter__body61__4__4", "./benchmark-input//$inter-body61_4", FULL);
relation* rel___dollorhead__stratified31__2__ = new relation(0, false, 2, 289, "rel___dollorhead__stratified31__2__", "./benchmark-input//$head-stratified31_2", FULL);
relation* rel___dollorinter__body3__4__3__4__2 = new relation(3, false, 4, 386, "rel___dollorinter__body3__4__3__4__2", "./benchmark-input//$inter-body3_4", FULL);
relation* rel__var__2__2__1 = new relation(2, true, 2, 430, "rel__var__2__2__1", "./benchmark-input//var_2", FULL);
relation* rel___dollorinter__body20__7__1__2__3__4__5__6__7 = new relation(7, true, 7, 367, "rel___dollorinter__body20__7__1__2__3__4__5__6__7", "./benchmark-input//$inter-body20_7", FULL);
relation* rel___dollorhead__stratified31__2__1__2 = new relation(2, true, 2, 289, "rel___dollorhead__stratified31__2__1__2", "./benchmark-input//$head-stratified31_2", FULL);
relation* rel__number__1__1 = new relation(1, true, 1, 343, "rel__number__1__1", "./benchmark-input//number_1", FULL);
relation* rel___dollorinter__head7__6__6__4__2__1 = new relation(4, false, 6, 264, "rel___dollorinter__head7__6__6__4__2__1", "./benchmark-input//$inter-head7_6", FULL);
relation* rel___dollorhead__stratified4__4__ = new relation(0, false, 4, 438, "rel___dollorhead__stratified4__4__", "./benchmark-input//$head-stratified4_4", FULL);
relation* rel__free__2__2 = new relation(1, false, 2, 400, "rel__free__2__2", "./benchmark-input//free_2", FULL);
relation* rel___dollorhead__stratified21__3__3__1 = new relation(2, false, 3, 297, "rel___dollorhead__stratified21__3__3__1", "./benchmark-input//$head-stratified21_3", FULL);
relation* rel___dollorinter__body__4__1__3 = new relation(2, false, 4, 364, "rel___dollorinter__body__4__1__3", "./benchmark-input//$inter-body_4", FULL);
relation* rel___dollorinter__head11__3__ = new relation(0, false, 3, 339, "rel___dollorinter__head11__3__", "./benchmark-input//$inter-head11_3", FULL);
relation* rel___dollorinter__head28__3__2 = new relation(1, false, 3, 303, "rel___dollorinter__head28__3__2", "./benchmark-input//$inter-head28_3", FULL);
relation* rel___dollorinter__body22__4__3__1 = new relation(2, false, 4, 318, "rel___dollorinter__body22__4__3__1", "./benchmark-input//$inter-body22_4", FULL);
relation* rel___dollorinter__body55__5__1 = new relation(1, false, 5, 399, "rel___dollorinter__body55__5__1", "./benchmark-input//$inter-body55_5", FULL);
relation* rel__peek__ctx__3__1__2__3 = new relation(3, true, 3, 403, "rel__peek__ctx__3__1__2__3", "./benchmark-input//peek_ctx_3", FULL);
relation* rel___dollorinter__body17__2__1__2 = new relation(2, true, 2, 258, "rel___dollorinter__body17__2__1__2", "./benchmark-input//$inter-body17_2", FULL);
relation* rel__callcc__2__2 = new relation(1, false, 2, 353, "rel__callcc__2__2", "./benchmark-input//callcc_2", FULL);
relation* rel___dollorinter__body60__2__1__2 = new relation(2, true, 2, 332, "rel___dollorinter__body60__2__1__2", "./benchmark-input//$inter-body60_2", FULL);
relation* rel__call__arg__list__3__2 = new relation(1, false, 3, 269, "rel__call__arg__list__3__2", "./benchmark-input//call_arg_list_3", FULL);
relation* rel___dollorinter__body16__3__1__2__3 = new relation(3, true, 3, 372, "rel___dollorinter__body16__3__1__2__3", "./benchmark-input//$inter-body16_3", FULL);
relation* rel___dollorinter__body37__2__1 = new relation(1, false, 2, 434, "rel___dollorinter__body37__2__1", "./benchmark-input//$inter-body37_2", FULL);
relation* rel___dollorhead__stratified9__4__1__2__3__4 = new relation(4, true, 4, 358, "rel___dollorhead__stratified9__4__1__2__3__4", "./benchmark-input//$head-stratified9_4", FULL);
relation* rel___dollorinter__body88__2__2 = new relation(1, false, 2, 370, "rel___dollorinter__body88__2__2", "./benchmark-input//$inter-body88_2", FULL);
relation* rel___dollorinter__body43__3__1__2__3 = new relation(3, true, 3, 394, "rel___dollorinter__body43__3__1__2__3", "./benchmark-input//$inter-body43_3", FULL);
relation* rel___dollorinter__body90__3__1__2__3 = new relation(3, true, 3, 307, "rel___dollorinter__body90__3__1__2__3", "./benchmark-input//$inter-body90_3", FULL);
relation* rel___dollorinter__body24__6__1__4 = new relation(2, false, 6, 354, "rel___dollorinter__body24__6__1__4", "./benchmark-input//$inter-body24_6", FULL);
relation* rel__flow__ae__2__2__1 = new relation(2, true, 2, 259, "rel__flow__ae__2__2__1", "./benchmark-input//flow_ae_2", FULL);
relation* rel___dollorinter__head8__8__2__7__6__1__4 = new relation(5, false, 8, 284, "rel___dollorinter__head8__8__2__7__6__1__4", "./benchmark-input//$inter-head8_8", FULL);
relation* rel___dollorinter__body21__5__5__1__2 = new relation(3, false, 5, 266, "rel___dollorinter__body21__5__5__1__2", "./benchmark-input//$inter-body21_5", FULL);
relation* rel___dollorinter__body35__4__1__2__3__4 = new relation(4, true, 4, 448, "rel___dollorinter__body35__4__1__2__3__4", "./benchmark-input//$inter-body35_4", FULL);
relation* rel___dollorinter__body18__4__1__2__3__4 = new relation(4, true, 4, 310, "rel___dollorinter__body18__4__1__2__3__4", "./benchmark-input//$inter-body18_4", FULL);
relation* rel___dollorinter__body63__2__1__2 = new relation(2, true, 2, 369, "rel___dollorinter__body63__2__1__2", "./benchmark-input//$inter-body63_2", FULL);
relation* rel___dollorhead__stratified38__4__1__2__3__4 = new relation(4, true, 4, 379, "rel___dollorhead__stratified38__4__1__2__3__4", "./benchmark-input//$head-stratified38_4", FULL);
relation* rel__prim1__4__1__2__3__4 = new relation(4, true, 4, 417, "rel__prim1__4__1__2__3__4", "./benchmark-input//prim1_4", FULL);
relation* rel___dollorinter__body2__4__1__2__3__4 = new relation(4, true, 4, 274, "rel___dollorinter__body2__4__1__2__3__4", "./benchmark-input//$inter-body2_4", FULL);
relation* rel___dollorinter__body70__2__2 = new relation(1, false, 2, 412, "rel___dollorinter__body70__2__2", "./benchmark-input//$inter-body70_2", FULL);
relation* rel__closure__2__2__1 = new relation(2, true, 2, 290, "rel__closure__2__2__1", "./benchmark-input//closure_2", FULL);
relation* rel__let__3__1 = new relation(1, false, 3, 271, "rel__let__3__1", "./benchmark-input//let_3", FULL);
relation* rel__A__2__2 = new relation(1, false, 2, 443, "rel__A__2__2", "./benchmark-input//A_2", FULL);
relation* rel__A__2__1__2 = new relation(2, true, 2, 443, "rel__A__2__1__2", "./benchmark-input//A_2", FULL);
relation* rel___dollorhead__stratified39__4__1__2__3__4 = new relation(4, true, 4, 293, "rel___dollorhead__stratified39__4__1__2__3__4", "./benchmark-input//$head-stratified39_4", FULL);
relation* rel___dollorinter__head26__3__ = new relation(0, false, 3, 304, "rel___dollorinter__head26__3__", "./benchmark-input//$inter-head26_3", FULL);
relation* rel__flow__ee__2__2__1 = new relation(2, true, 2, 275, "rel__flow__ee__2__2__1", "./benchmark-input//flow_ee_2", FULL);
relation* rel___dollorinter__body57__3__2 = new relation(1, false, 3, 324, "rel___dollorinter__body57__3__2", "./benchmark-input//$inter-body57_3", FULL);
relation* rel__E__3__2__1 = new relation(2, false, 3, 335, "rel__E__3__2__1", "./benchmark-input//E_3", FULL);
relation* rel__vaddr__2__2__1 = new relation(2, true, 2, 363, "rel__vaddr__2__2__1", "./benchmark-input//vaddr_2", FULL);
relation* rel__let__3__3 = new relation(1, false, 3, 271, "rel__let__3__3", "./benchmark-input//let_3", FULL);
relation* rel__ifk__4__3__2__4__1 = new relation(4, true, 4, 366, "rel__ifk__4__3__2__4__1", "./benchmark-input//ifk_4", FULL);
relation* rel___dollorinter__body40__4__2 = new relation(1, false, 4, 446, "rel___dollorinter__body40__4__2", "./benchmark-input//$inter-body40_4", FULL);
relation* rel__prim2__3__0 = new relation(1, false, 3, 291, "rel__prim2__3__0", "./benchmark-input//prim2_3", FULL);
relation* rel__bool__2__1__2 = new relation(2, true, 2, 333, "rel__bool__2__1__2", "./benchmark-input//bool_2", FULL);
relation* rel___dollorinter__head32__4__1__2 = new relation(2, false, 4, 340, "rel___dollorinter__head32__4__1__2", "./benchmark-input//$inter-head32_4", FULL);
relation* rel___dollorinter__body__4__1__2__3__4 = new relation(4, true, 4, 364, "rel___dollorinter__body__4__1__2__3__4", "./benchmark-input//$inter-body_4", FULL);
relation* rel___dollorhead__stratified4__4__1__2__3__4 = new relation(4, true, 4, 438, "rel___dollorhead__stratified4__4__1__2__3__4", "./benchmark-input//$head-stratified4_4", FULL);
relation* rel___dollorinter__head6__4__1__2__3__4 = new relation(4, true, 4, 322, "rel___dollorinter__head6__4__1__2__3__4", "./benchmark-input//$inter-head6_4", FULL);
relation* rel___dollorinter__body48__3__1__2__3 = new relation(3, true, 3, 338, "rel___dollorinter__body48__3__1__2__3", "./benchmark-input//$inter-body48_3", FULL);
relation* rel___dollorinter__body22__4__1__2__3__4 = new relation(4, true, 4, 318, "rel___dollorinter__body22__4__1__2__3__4", "./benchmark-input//$inter-body22_4", FULL);
relation* rel__kont__1__1 = new relation(1, true, 1, 382, "rel__kont__1__1", "./benchmark-input//kont_1", FULL);
relation* rel___dollorinter__body33__1__1 = new relation(1, true, 1, 459, "rel___dollorinter__body33__1__1", "./benchmark-input//$inter-body33_1", FULL);
relation* rel___dollorinter__head12__4__4__3__1 = new relation(3, false, 4, 421, "rel___dollorinter__head12__4__4__3__1", "./benchmark-input//$inter-head12_4", FULL);
relation* rel___dollorinter__body71__2__1__2 = new relation(2, true, 2, 273, "rel___dollorinter__body71__2__1__2", "./benchmark-input//$inter-body71_2", FULL);
relation* rel___dollorinter__body12__2__1 = new relation(1, false, 2, 342, "rel___dollorinter__body12__2__1", "./benchmark-input//$inter-body12_2", FULL);
relation* rel___dollorhead__stratified23__4__1__2__3__4 = new relation(4, true, 4, 308, "rel___dollorhead__stratified23__4__1__2__3__4", "./benchmark-input//$head-stratified23_4", FULL);
relation* rel___dollorinter__body57__3__1__2__3 = new relation(3, true, 3, 324, "rel___dollorinter__body57__3__1__2__3", "./benchmark-input//$inter-body57_3", FULL);
relation* rel___dollorinter__body13__3__2 = new relation(1, false, 3, 334, "rel___dollorinter__body13__3__2", "./benchmark-input//$inter-body13_3", FULL);
relation* rel___dollorinter__body44__2__1__2 = new relation(2, true, 2, 413, "rel___dollorinter__body44__2__1__2", "./benchmark-input//$inter-body44_2", FULL);
relation* rel___dollorinter__body41__2__1__2 = new relation(2, true, 2, 388, "rel___dollorinter__body41__2__1__2", "./benchmark-input//$inter-body41_2", FULL);
relation* rel___dollorinter__body14__5__5 = new relation(1, false, 5, 441, "rel___dollorinter__body14__5__5", "./benchmark-input//$inter-body14_5", FULL);
relation* rel__closure__2__0 = new relation(1, false, 2, 290, "rel__closure__2__0", "./benchmark-input//closure_2", FULL);
relation* rel__A__2__1 = new relation(1, false, 2, 443, "rel__A__2__1", "./benchmark-input//A_2", FULL);
relation* rel__let__list__3__3 = new relation(1, false, 3, 393, "rel__let__list__3__3", "./benchmark-input//let_list_3", FULL);
relation* rel___dollorinter__head24__2__ = new relation(0, false, 2, 436, "rel___dollorinter__head24__2__", "./benchmark-input//$inter-head24_2", FULL);
relation* rel___dollorinter__body80__2__ = new relation(0, false, 2, 374, "rel___dollorinter__body80__2__", "./benchmark-input//$inter-body80_2", FULL);
relation* rel___dollorhead__stratified35__4__1__2__3__4 = new relation(4, true, 4, 389, "rel___dollorhead__stratified35__4__1__2__3__4", "./benchmark-input//$head-stratified35_4", FULL);
relation* rel___dollorinter__head__5__1__2__3__4__5 = new relation(5, true, 5, 262, "rel___dollorinter__head__5__1__2__3__4__5", "./benchmark-input//$inter-head_5", FULL);
relation* rel__letk__4__0 = new relation(1, false, 4, 442, "rel__letk__4__0", "./benchmark-input//letk_4", FULL);
relation* rel___dollorinter__body31__2__2 = new relation(1, false, 2, 306, "rel___dollorinter__body31__2__2", "./benchmark-input//$inter-body31_2", FULL);
relation* rel___dollorinter__head22__4__1__2__3__4 = new relation(4, true, 4, 424, "rel___dollorinter__head22__4__1__2__3__4", "./benchmark-input//$inter-head22_4", FULL);
relation* rel___dollorinter__head23__5__ = new relation(0, false, 5, 285, "rel___dollorinter__head23__5__", "./benchmark-input//$inter-head23_5", FULL);
relation* rel___dollorhead__stratified42__4__4__2 = new relation(2, false, 4, 349, "rel___dollorhead__stratified42__4__4__2", "./benchmark-input//$head-stratified42_4", FULL);
relation* rel__fn__4__4__3__2__1 = new relation(4, true, 4, 296, "rel__fn__4__4__3__2__1", "./benchmark-input//fn_4", FULL);
relation* rel___dollorhead__stratified3__2__ = new relation(0, false, 2, 416, "rel___dollorhead__stratified3__2__", "./benchmark-input//$head-stratified3_2", FULL);
relation* rel___dollorinter__body60__2__2 = new relation(1, false, 2, 332, "rel___dollorinter__body60__2__2", "./benchmark-input//$inter-body60_2", FULL);
relation* rel__callcck__2__1__2 = new relation(2, true, 2, 265, "rel__callcck__2__1__2", "./benchmark-input//callcck_2", FULL);
relation* rel___dollorinter__body84__3__1__2__3 = new relation(3, true, 3, 329, "rel___dollorinter__body84__3__1__2__3", "./benchmark-input//$inter-body84_3", FULL);
relation* rel___dollorhead__stratified36__4__4__2 = new relation(2, false, 4, 375, "rel___dollorhead__stratified36__4__4__2", "./benchmark-input//$head-stratified36_4", FULL);
relation* rel___dollorinter__body77__3__1__2__3 = new relation(3, true, 3, 426, "rel___dollorinter__body77__3__1__2__3", "./benchmark-input//$inter-body77_3", FULL);
relation* rel___dollorhead__stratified45__2__1__2 = new relation(2, true, 2, 301, "rel___dollorhead__stratified45__2__1__2", "./benchmark-input//$head-stratified45_2", FULL);
relation* rel__vaddr__2__1__2 = new relation(2, true, 2, 363, "rel__vaddr__2__1__2", "./benchmark-input//vaddr_2", FULL);
relation* rel___dollorinter__body18__4__3__1 = new relation(2, false, 4, 310, "rel___dollorinter__body18__4__3__1", "./benchmark-input//$inter-body18_4", FULL);
relation* rel___dollorinter__head30__5__1__2__3__4__5 = new relation(5, true, 5, 278, "rel___dollorinter__head30__5__1__2__3__4__5", "./benchmark-input//$inter-head30_5", FULL);
relation* rel___dollorhead__stratified__4__ = new relation(0, false, 4, 315, "rel___dollorhead__stratified__4__", "./benchmark-input//$head-stratified_4", FULL);
relation* rel___dollorinter__body70__2__1__2 = new relation(2, true, 2, 412, "rel___dollorinter__body70__2__1__2", "./benchmark-input//$inter-body70_2", FULL);
relation* rel___dollorinter__body73__3__ = new relation(0, false, 3, 418, "rel___dollorinter__body73__3__", "./benchmark-input//$inter-body73_3", FULL);
relation* rel___dollorinter__body28__4__1__2__3__4 = new relation(4, true, 4, 401, "rel___dollorinter__body28__4__1__2__3__4", "./benchmark-input//$inter-body28_4", FULL);
relation* rel___dollorinter__body66__2__1__2 = new relation(2, true, 2, 341, "rel___dollorinter__body66__2__1__2", "./benchmark-input//$inter-body66_2", FULL);
relation* rel___dollorinter__body76__4__3__2 = new relation(2, false, 4, 346, "rel___dollorinter__body76__4__3__2", "./benchmark-input//$inter-body76_4", FULL);
relation* rel___dollorinter__body84__3__1 = new relation(1, false, 3, 329, "rel___dollorinter__body84__3__1", "./benchmark-input//$inter-body84_3", FULL);
relation* rel___dollorinter__head2__6__6__5__2__1 = new relation(4, false, 6, 355, "rel___dollorinter__head2__6__6__5__2__1", "./benchmark-input//$inter-head2_6", FULL);
relation* rel___dollorhead__stratified9__4__ = new relation(0, false, 4, 358, "rel___dollorhead__stratified9__4__", "./benchmark-input//$head-stratified9_4", FULL);
relation* rel__flow__aa__2__1__2 = new relation(2, true, 2, 449, "rel__flow__aa__2__1__2", "./benchmark-input//flow_aa_2", FULL);
relation* rel___dollorinter__body16__3__1 = new relation(1, false, 3, 372, "rel___dollorinter__body16__3__1", "./benchmark-input//$inter-body16_3", FULL);
relation* rel___dollorinter__body83__2__1 = new relation(1, false, 2, 408, "rel___dollorinter__body83__2__1", "./benchmark-input//$inter-body83_2", FULL);
relation* rel__lambda__arg__list__3__1__2__3 = new relation(3, true, 3, 420, "rel__lambda__arg__list__3__1__2__3", "./benchmark-input//lambda_arg_list_3", FULL);
relation* rel___dollorinter__body85__4__1 = new relation(1, false, 4, 312, "rel___dollorinter__body85__4__1", "./benchmark-input//$inter-body85_4", FULL);
relation* rel__top__exp__1__1 = new relation(1, true, 1, 457, "rel__top__exp__1__1", "./benchmark-input//top_exp_1", FULL);
relation* rel___dollorinter__body74__4__4__3 = new relation(2, false, 4, 368, "rel___dollorinter__body74__4__4__3", "./benchmark-input//$inter-body74_4", FULL);
relation* rel___dollorinter__body79__4__3__4 = new relation(2, false, 4, 371, "rel___dollorinter__body79__4__3__4", "./benchmark-input//$inter-body79_4", FULL);
relation* rel___dollorinter__body13__3__1__2__3 = new relation(3, true, 3, 334, "rel___dollorinter__body13__3__1__2__3", "./benchmark-input//$inter-body13_3", FULL);
relation* rel__null__0__ = new relation(0, true, 0, 361, "rel__null__0__", "./benchmark-input//null_0", FULL);
relation* rel___dollorinter__head16__3__2 = new relation(1, false, 3, 402, "rel___dollorinter__head16__3__2", "./benchmark-input//$inter-head16_3", FULL);
relation* rel___dollorinter__head9__4__3__2 = new relation(2, false, 4, 456, "rel___dollorinter__head9__4__3__2", "./benchmark-input//$inter-head9_4", FULL);
relation* rel___dollorinter__body32__4__1__2__3__4 = new relation(4, true, 4, 414, "rel___dollorinter__body32__4__1__2__3__4", "./benchmark-input//$inter-body32_4", FULL);
relation* rel___dollorhead__stratified15__2__ = new relation(0, false, 2, 336, "rel___dollorhead__stratified15__2__", "./benchmark-input//$head-stratified15_2", FULL);
relation* rel__lambda__3__3 = new relation(1, false, 3, 395, "rel__lambda__3__3", "./benchmark-input//lambda_3", FULL);
relation* rel___dollorinter__body27__3__1__2__3 = new relation(3, true, 3, 360, "rel___dollorinter__body27__3__1__2__3", "./benchmark-input//$inter-body27_3", FULL);
relation* rel__num__2__1 = new relation(1, false, 2, 267, "rel__num__2__1", "./benchmark-input//num_2", FULL);
relation* rel___dollorinter__body85__4__1__2__3__4 = new relation(4, true, 4, 312, "rel___dollorinter__body85__4__1__2__3__4", "./benchmark-input//$inter-body85_4", FULL);
relation* rel___dollorinter__body59__2__1__2 = new relation(2, true, 2, 321, "rel___dollorinter__body59__2__1__2", "./benchmark-input//$inter-body59_2", FULL);
relation* rel___dollorinter__head23__5__1__2__3__4__5 = new relation(5, true, 5, 285, "rel___dollorinter__head23__5__1__2__3__4__5", "./benchmark-input//$inter-head23_5", FULL);
relation* rel___dollorinter__head2__6__1__2__3__4__5__6 = new relation(6, true, 6, 355, "rel___dollorinter__head2__6__1__2__3__4__5__6", "./benchmark-input//$inter-head2_6", FULL);
relation* rel__call__3__2 = new relation(1, false, 3, 287, "rel__call__3__2", "./benchmark-input//call_3", FULL);
relation* rel__callcc__2__1__2 = new relation(2, true, 2, 353, "rel__callcc__2__1__2", "./benchmark-input//callcc_2", FULL);
relation* rel__call__3__1__2__3 = new relation(3, true, 3, 287, "rel__call__3__1__2__3", "./benchmark-input//call_3", FULL);
relation* rel___dollorinter__head13__6__1__2__3__4__5__6 = new relation(6, true, 6, 380, "rel___dollorinter__head13__6__1__2__3__4__5__6", "./benchmark-input//$inter-head13_6", FULL);
relation* rel___dollorinter__body3__4__1__2__3__4 = new relation(4, true, 4, 386, "rel___dollorinter__body3__4__1__2__3__4", "./benchmark-input//$inter-body3_4", FULL);
relation* rel___dollorhead__stratified11__2__ = new relation(0, false, 2, 445, "rel___dollorhead__stratified11__2__", "./benchmark-input//$head-stratified11_2", FULL);
relation* rel___dollorinter__head27__6__1__2__3__4__5__6 = new relation(6, true, 6, 299, "rel___dollorinter__head27__6__1__2__3__4__5__6", "./benchmark-input//$inter-head27_6", FULL);
relation* rel__callcc__2__0 = new relation(1, false, 2, 353, "rel__callcc__2__0", "./benchmark-input//callcc_2", FULL);
relation* rel___dollorinter__body78__3__3__1 = new relation(2, false, 3, 451, "rel___dollorinter__body78__3__3__1", "./benchmark-input//$inter-body78_3", FULL);
relation* rel___dollorhead__stratified20__4__4__1 = new relation(2, false, 4, 268, "rel___dollorhead__stratified20__4__4__1", "./benchmark-input//$head-stratified20_4", FULL);
relation* rel___dollorhead__stratified39__4__ = new relation(0, false, 4, 293, "rel___dollorhead__stratified39__4__", "./benchmark-input//$head-stratified39_4", FULL);
relation* rel__num__2__1__2 = new relation(2, true, 2, 267, "rel__num__2__1__2", "./benchmark-input//num_2", FULL);
relation* rel___dollorbir__sub6__3__1 = new relation(1, false, 3, 453, "rel___dollorbir__sub6__3__1", "./benchmark-input//$bir-sub6_3", FULL);
relation* rel__fn__4__1__2__3__4 = new relation(4, true, 4, 296, "rel__fn__4__1__2__3__4", "./benchmark-input//fn_4", FULL);
relation* rel__prim__2__1__2 = new relation(2, true, 2, 404, "rel__prim__2__1__2", "./benchmark-input//prim_2", FULL);
relation* rel___dollorinter__body37__2__1__2 = new relation(2, true, 2, 434, "rel___dollorinter__body37__2__1__2", "./benchmark-input//$inter-body37_2", FULL);
relation* rel__boolv__1__0 = new relation(1, false, 1, 331, "rel__boolv__1__0", "./benchmark-input//boolv_1", FULL);
relation* rel___dollorinter__head4__3__1__2__3 = new relation(3, true, 3, 327, "rel___dollorinter__head4__3__1__2__3", "./benchmark-input//$inter-head4_3", FULL);
relation* rel___dollorinter__body71__2__2 = new relation(1, false, 2, 273, "rel___dollorinter__body71__2__2", "./benchmark-input//$inter-body71_2", FULL);
relation* rel___dollorinter__body25__8__1__2__3__4__5__6__7__8 = new relation(8, true, 8, 373, "rel___dollorinter__body25__8__1__2__3__4__5__6__7__8", "./benchmark-input//$inter-body25_8", FULL);
relation* rel___dollorinter__body43__3__1 = new relation(1, false, 3, 394, "rel___dollorinter__body43__3__1", "./benchmark-input//$inter-body43_3", FULL);
relation* rel___dollorbir__sub3__4__3__1 = new relation(2, false, 4, 409, "rel___dollorbir__sub3__4__3__1", "./benchmark-input//$bir-sub3_4", FULL);
relation* rel___dollorhead__stratified37__4__1__2__3__4 = new relation(4, true, 4, 256, "rel___dollorhead__stratified37__4__1__2__3__4", "./benchmark-input//$head-stratified37_4", FULL);
relation* rel___dollorinter__head14__5__3__1 = new relation(2, false, 5, 286, "rel___dollorinter__head14__5__3__1", "./benchmark-input//$inter-head14_5", FULL);
relation* rel__copy__ctx__3__1 = new relation(1, false, 3, 377, "rel__copy__ctx__3__1", "./benchmark-input//copy_ctx_3", FULL);
relation* rel___dollorinter__body88__2__1__2 = new relation(2, true, 2, 370, "rel___dollorinter__body88__2__1__2", "./benchmark-input//$inter-body88_2", FULL);
relation* rel___dollorhead__stratified26__3__1__2__3 = new relation(3, true, 3, 450, "rel___dollorhead__stratified26__3__1__2__3", "./benchmark-input//$head-stratified26_3", FULL);
relation* rel___dollorinter__body62__2__2 = new relation(1, false, 2, 428, "rel___dollorinter__body62__2__2", "./benchmark-input//$inter-body62_2", FULL);
relation* rel___dollorinter__head14__5__1__2__3__4__5 = new relation(5, true, 5, 286, "rel___dollorinter__head14__5__1__2__3__4__5", "./benchmark-input//$inter-head14_5", FULL);
relation* rel___dollorhead__stratified33__4__1__2__3__4 = new relation(4, true, 4, 461, "rel___dollorhead__stratified33__4__1__2__3__4", "./benchmark-input//$head-stratified33_4", FULL);
relation* rel__call__3__3 = new relation(1, false, 3, 287, "rel__call__3__3", "./benchmark-input//call_3", FULL);
relation* rel___dollorinter__body20__7__2__1__4 = new relation(3, false, 7, 367, "rel___dollorinter__body20__7__2__1__4", "./benchmark-input//$inter-body20_7", FULL);
relation* rel___dollorinter__head24__2__1__2 = new relation(2, true, 2, 436, "rel___dollorinter__head24__2__1__2", "./benchmark-input//$inter-head24_2", FULL);
relation* rel___dollorinter__body9__3__3__2 = new relation(2, false, 3, 391, "rel___dollorinter__body9__3__3__2", "./benchmark-input//$inter-body9_3", FULL);
relation* rel___dollorinter__head32__4__1__2__3__4 = new relation(4, true, 4, 340, "rel___dollorinter__head32__4__1__2__3__4", "./benchmark-input//$inter-head32_4", FULL);
relation* rel__setk__2__1__2 = new relation(2, true, 2, 294, "rel__setk__2__1__2", "./benchmark-input//setk_2", FULL);
relation* rel___dollorinter__body83__2__1__2 = new relation(2, true, 2, 408, "rel___dollorinter__body83__2__1__2", "./benchmark-input//$inter-body83_2", FULL);
relation* rel__mt__0__ = new relation(0, true, 0, 398, "rel__mt__0__", "./benchmark-input//mt_0", FULL);
relation* rel__num__1__1 = new relation(1, true, 1, 419, "rel__num__1__1", "./benchmark-input//num_1", FULL);
relation* rel__prim1__4__3__2__1__4 = new relation(4, true, 4, 417, "rel__prim1__4__3__2__1__4", "./benchmark-input//prim1_4", FULL);
relation* rel___dollorhead__stratified12__4__1__2__3__4 = new relation(4, true, 4, 283, "rel___dollorhead__stratified12__4__1__2__3__4", "./benchmark-input//$head-stratified12_4", FULL);
relation* rel___dollorinter__head20__3__1 = new relation(1, false, 3, 423, "rel___dollorinter__head20__3__1", "./benchmark-input//$inter-head20_3", FULL);
relation* rel___dollorinter__body10__4__1__2__3__4 = new relation(4, true, 4, 350, "rel___dollorinter__body10__4__1__2__3__4", "./benchmark-input//$inter-body10_4", FULL);
relation* rel___dollorlst__2__2 = new relation(1, false, 2, 313, "rel___dollorlst__2__2", "./benchmark-input//$lst_2", FULL);
relation* rel___dollorinter__head5__4__ = new relation(0, false, 4, 425, "rel___dollorinter__head5__4__", "./benchmark-input//$inter-head5_4", FULL);
relation* rel___dollorinter__body4__7__1__2__3__4__5__6__7 = new relation(7, true, 7, 362, "rel___dollorinter__body4__7__1__2__3__4__5__6__7", "./benchmark-input//$inter-body4_7", FULL);
relation* rel___dollorinter__body6__6__6__3 = new relation(2, false, 6, 279, "rel___dollorinter__body6__6__6__3", "./benchmark-input//$inter-body6_6", FULL);
relation* rel___dollorinter__body36__3__2__3 = new relation(2, false, 3, 381, "rel___dollorinter__body36__3__2__3", "./benchmark-input//$inter-body36_3", FULL);
relation* rel___dollorinter__body76__4__1__2__3__4 = new relation(4, true, 4, 346, "rel___dollorinter__body76__4__1__2__3__4", "./benchmark-input//$inter-body76_4", FULL);
relation* rel__boolv__1__1 = new relation(1, true, 1, 331, "rel__boolv__1__1", "./benchmark-input//boolv_1", FULL);
relation* rel__E__3__3__2__1 = new relation(3, true, 3, 335, "rel__E__3__3__2__1", "./benchmark-input//E_3", FULL);
relation* rel__setk__2__0 = new relation(1, false, 2, 294, "rel__setk__2__0", "./benchmark-input//setk_2", FULL);
relation* rel___dollorhead__stratified23__4__ = new relation(0, false, 4, 308, "rel___dollorhead__stratified23__4__", "./benchmark-input//$head-stratified23_4", FULL);
relation* rel___dollorinter__head8__8__1__2__3__4__5__6__7__8 = new relation(8, true, 8, 284, "rel___dollorinter__head8__8__1__2__3__4__5__6__7__8", "./benchmark-input//$inter-head8_8", FULL);
relation* rel__bool__2__1 = new relation(1, false, 2, 333, "rel__bool__2__1", "./benchmark-input//bool_2", FULL);
relation* rel___dollorinter__body78__3__1__2__3 = new relation(3, true, 3, 451, "rel___dollorinter__body78__3__1__2__3", "./benchmark-input//$inter-body78_3", FULL);
relation* rel__prim__call__3__3 = new relation(1, false, 3, 257, "rel__prim__call__3__3", "./benchmark-input//prim_call_3", FULL);
relation* rel__let__3__1__2__3 = new relation(3, true, 3, 271, "rel__let__3__1__2__3", "./benchmark-input//let_3", FULL);
relation* rel___dollorinter__body81__2__1__2 = new relation(2, true, 2, 276, "rel___dollorinter__body81__2__1__2", "./benchmark-input//$inter-body81_2", FULL);
relation* rel__letk__4__1__2__3__4 = new relation(4, true, 4, 442, "rel__letk__4__1__2__3__4", "./benchmark-input//letk_4", FULL);
relation* rel___dollorinter__body10__4__3 = new relation(1, false, 4, 350, "rel___dollorinter__body10__4__3", "./benchmark-input//$inter-body10_4", FULL);
relation* rel___dollorinter__body15__2__1 = new relation(1, false, 2, 261, "rel___dollorinter__body15__2__1", "./benchmark-input//$inter-body15_2", FULL);
relation* rel___dollorinter__body79__4__1__2__3__4 = new relation(4, true, 4, 371, "rel___dollorinter__body79__4__1__2__3__4", "./benchmark-input//$inter-body79_4", FULL);
relation* rel___dollorinter__body1__3__1__2__3 = new relation(3, true, 3, 309, "rel___dollorinter__body1__3__1__2__3", "./benchmark-input//$inter-body1_3", FULL);
relation* rel__fn__4__1 = new relation(1, false, 4, 296, "rel__fn__4__1", "./benchmark-input//fn_4", FULL);
relation* rel___dollorinter__body27__3__1 = new relation(1, false, 3, 360, "rel___dollorinter__body27__3__1", "./benchmark-input//$inter-body27_3", FULL);
relation* rel___dollorinter__body4__7__6__2__7 = new relation(3, false, 7, 362, "rel___dollorinter__body4__7__6__2__7", "./benchmark-input//$inter-body4_7", FULL);
relation* rel___dollorbir__sub4__3__1__2__3 = new relation(3, true, 3, 344, "rel___dollorbir__sub4__3__1__2__3", "./benchmark-input//$bir-sub4_3", FULL);
relation* rel___dollorinter__body35__4__4__2 = new relation(2, false, 4, 448, "rel___dollorinter__body35__4__4__2", "./benchmark-input//$inter-body35_4", FULL);
relation* rel__here__5__5__4__3__2__1 = new relation(5, true, 5, 270, "rel__here__5__5__4__3__2__1", "./benchmark-input//here_5", FULL);
relation* rel___dollorinter__body68__2__1__2 = new relation(2, true, 2, 317, "rel___dollorinter__body68__2__1__2", "./benchmark-input//$inter-body68_2", FULL);
relation* rel___dollorinter__body87__3__1__2__3 = new relation(3, true, 3, 422, "rel___dollorinter__body87__3__1__2__3", "./benchmark-input//$inter-body87_3", FULL);
relation* rel__copy__ctx__3__1__2__3 = new relation(3, true, 3, 377, "rel__copy__ctx__3__1__2__3", "./benchmark-input//copy_ctx_3", FULL);
relation* rel___dollorinter__head31__3__1__2__3 = new relation(3, true, 3, 411, "rel___dollorinter__head31__3__1__2__3", "./benchmark-input//$inter-head31_3", FULL);
relation* rel___dollorbir__sub1__4__1 = new relation(1, false, 4, 432, "rel___dollorbir__sub1__4__1", "./benchmark-input//$bir-sub1_4", FULL);
relation* rel__call__arg__list__3__1__2__3 = new relation(3, true, 3, 269, "rel__call__arg__list__3__1__2__3", "./benchmark-input//call_arg_list_3", FULL);
relation* rel___dollorinter__head29__3__1__2__3 = new relation(3, true, 3, 439, "rel___dollorinter__head29__3__1__2__3", "./benchmark-input//$inter-head29_3", FULL);
relation* rel__free__2__1__2 = new relation(2, true, 2, 400, "rel__free__2__1__2", "./benchmark-input//free_2", FULL);
relation* rel___dollorinter__body11__2__1 = new relation(1, false, 2, 323, "rel___dollorinter__body11__2__1", "./benchmark-input//$inter-body11_2", FULL);
relation* rel__E__3__2 = new relation(1, false, 3, 335, "rel__E__3__2", "./benchmark-input//E_3", FULL);
relation* rel___dollorhead__stratified2__7__1__2__4__7 = new relation(4, false, 7, 328, "rel___dollorhead__stratified2__7__1__2__4__7", "./benchmark-input//$head-stratified2_7", FULL);
relation* rel__let__list__3__1 = new relation(1, false, 3, 393, "rel__let__list__3__1", "./benchmark-input//let_list_3", FULL);
relation* rel___dollorinter__body29__2__1 = new relation(1, false, 2, 387, "rel___dollorinter__body29__2__1", "./benchmark-input//$inter-body29_2", FULL);
relation* rel___dollorinter__body56__3__1__2__3 = new relation(3, true, 3, 444, "rel___dollorinter__body56__3__1__2__3", "./benchmark-input//$inter-body56_3", FULL);
relation* rel___dollorinter__body80__2__1__2 = new relation(2, true, 2, 374, "rel___dollorinter__body80__2__1__2", "./benchmark-input//$inter-body80_2", FULL);
relation* rel___dollorinter__head10__3__1__2__3 = new relation(3, true, 3, 337, "rel___dollorinter__head10__3__1__2__3", "./benchmark-input//$inter-head10_3", FULL);
relation* rel___dollorhead__stratified25__4__4__1 = new relation(2, false, 4, 280, "rel___dollorhead__stratified25__4__4__1", "./benchmark-input//$head-stratified25_4", FULL);
relation* rel__setk__1__1 = new relation(1, true, 1, 277, "rel__setk__1__1", "./benchmark-input//setk_1", FULL);
relation* rel__setb__3__1__2__3 = new relation(3, true, 3, 359, "rel__setb__3__1__2__3", "./benchmark-input//setb_3", FULL);
relation* rel__closure__2__1__2 = new relation(2, true, 2, 290, "rel__closure__2__1__2", "./benchmark-input//closure_2", FULL);

RAM* scc5451 = new RAM(false, 1);
scc5451->add_relation(rel__var__2__1__2, true);
scc5451->add_rule(new fact(rel__var__2__1__2, {n2d(524368), n2d(524367)}));

RAM* scc5452 = new RAM(false, 242);
scc5452->add_relation(rel__let__3__1__2__3, true);
scc5452->add_rule(new fact(rel__let__3__1__2__3, {n2d(524468), n2d(524404), n2d(524364)}));

RAM* scc5453 = new RAM(false, 5);
scc5453->add_relation(rel__let__list__3__1__2__3, true);
scc5453->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524465), n2d(524466)}));

RAM* scc5454 = new RAM(false, 9);
scc5454->add_relation(rel__var__2__1__2, true);
scc5454->add_rule(new fact(rel__var__2__1__2, {n2d(524387), n2d(524367)}));

RAM* scc5455 = new RAM(false, 131);
scc5455->add_relation(rel__num__2__1__2, true);
scc5455->add_rule(new fact(rel__num__2__1__2, {n2d(524334), n2d(19)}));

RAM* scc5456 = new RAM(false, 120);
scc5456->add_relation(rel__let__list__3__1__2__3, true);
scc5456->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524461), n2d(524462)}));

RAM* scc5457 = new RAM(false, 139);
scc5457->add_relation(rel__var__2__1__2, true);
scc5457->add_rule(new fact(rel__var__2__1__2, {n2d(524390), n2d(524367)}));

RAM* scc5458 = new RAM(false, 143);
scc5458->add_relation(rel__num__2__1__2, true);
scc5458->add_rule(new fact(rel__num__2__1__2, {n2d(524318), n2d(11)}));

RAM* scc5459 = new RAM(false, 80);
scc5459->add_relation(rel__prim__call__3__1__2__3, true);
scc5459->add_rule(new fact(rel__prim__call__3__1__2__3, {n2d(524357), n2d(524358), n2d(524284)}));

RAM* scc5460 = new RAM(false, 84);
scc5460->add_relation(rel__lambda__arg__list__3__1, true);
scc5460->add_relation(rel__lambda__arg__list__3__1__2__3, true);
scc5460->add_rule(new parallel_acopy(rel__lambda__arg__list__3__1, rel__lambda__arg__list__3__1__2__3, DELTA, {0, 3, 1, 2}));

RAM* scc5461 = new RAM(false, 13);
scc5461->add_relation(rel__mt__0__, true);
scc5461->add_rule(new parallel_copy(rel__mt__0__, rel__top__exp__1__1, FULL, {}));

RAM* scc5462 = new RAM(false, 92);
scc5462->add_relation(rel__num__2__1__2, true);
scc5462->add_rule(new fact(rel__num__2__1__2, {n2d(524312), n2d(8)}));

RAM* scc5463 = new RAM(false, 135);
scc5463->add_relation(rel__free__2__1__2, true);
scc5463->add_rule(new parallel_copy(rel__free__2__1__2, rel__var__2__1__2, FULL, {1, 0}));

RAM* scc5464 = new RAM(false, 214);
scc5464->add_relation(rel__let__list__3__1__2__3, true);
scc5464->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524457), n2d(524458)}));

RAM* scc5465 = new RAM(false, 218);
scc5465->add_relation(rel__let__list__3__1__2__3, true);
scc5465->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524411), n2d(524412)}));

RAM* scc5466 = new RAM(false, 222);
scc5466->add_relation(rel__var__2__1__2, true);
scc5466->add_relation(rel__var__2__2__1, true);
scc5466->add_rule(new parallel_acopy(rel__var__2__2__1, rel__var__2__1__2, DELTA, {1, 0, 2}));

RAM* scc5467 = new RAM(false, 163);
scc5467->add_relation(rel___dollorbir__sub4__3__1, true);
scc5467->add_relation(rel___dollorbir__sub4__3__1__2__3, true);
scc5467->add_rule(new parallel_acopy(rel___dollorbir__sub4__3__1, rel___dollorbir__sub4__3__1__2__3, DELTA, {0, 3, 1, 2}));

RAM* scc5468 = new RAM(false, 88);
scc5468->add_relation(rel___dollorhead__stratified15__2__1__2, true);
scc5468->add_relation(rel___dollorhead__stratified15__2__, true);
scc5468->add_rule(new parallel_acopy(rel___dollorhead__stratified15__2__, rel___dollorhead__stratified15__2__1__2, DELTA, {2, 0, 1}));

RAM* scc5469 = new RAM(false, 171);
scc5469->add_relation(rel___dollorinter__body81__2__, true);
scc5469->add_relation(rel___dollorinter__body81__2__1__2, true);
scc5469->add_rule(new parallel_acopy(rel___dollorinter__body81__2__, rel___dollorinter__body81__2__1__2, DELTA, {2, 0, 1}));

RAM* scc5470 = new RAM(false, 175);
scc5470->add_relation(rel___dollorinter__head21__3__, true);
scc5470->add_relation(rel___dollorinter__head21__3__1__2__3, true);
scc5470->add_rule(new parallel_acopy(rel___dollorinter__head21__3__, rel___dollorinter__head21__3__1__2__3, DELTA, {3, 0, 1, 2}));

RAM* scc5471 = new RAM(false, 33);
scc5471->add_relation(rel__var__2__1__2, true);
scc5471->add_rule(new fact(rel__var__2__1__2, {n2d(524394), n2d(524367)}));

RAM* scc5472 = new RAM(false, 210);
scc5472->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5472->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524286), n2d(0), n2d(524291)}));

RAM* scc5473 = new RAM(false, 37);
scc5473->add_relation(rel__let__list__3__1__2__3, true);
scc5473->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524429), n2d(524430)}));

RAM* scc5474 = new RAM(false, 41);
scc5474->add_relation(rel__let__list__3__1__2__3, true);
scc5474->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524439), n2d(524440)}));

RAM* scc5475 = new RAM(false, 167);
scc5475->add_relation(rel__call__3__1__2__3, true);
scc5475->add_rule(new fact(rel__call__3__1__2__3, {n2d(524438), n2d(524382), n2d(524323)}));

RAM* scc5476 = new RAM(false, 246);
scc5476->add_relation(rel___dollorinter__head4__3__, true);
scc5476->add_relation(rel___dollorinter__head4__3__1__2__3, true);
scc5476->add_rule(new parallel_acopy(rel___dollorinter__head4__3__, rel___dollorinter__head4__3__1__2__3, DELTA, {3, 0, 1, 2}));

RAM* scc5477 = new RAM(false, 250);
scc5477->add_relation(rel__num__2__1__2, true);
scc5477->add_rule(new fact(rel__num__2__1__2, {n2d(524298), n2d(1)}));

RAM* scc5478 = new RAM(false, 254);
scc5478->add_relation(rel__num__2__1__2, true);
scc5478->add_rule(new fact(rel__num__2__1__2, {n2d(524346), n2d(25)}));

RAM* scc5479 = new RAM(false, 112);
scc5479->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5479->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524341), n2d(0), n2d(524342)}));

RAM* scc5480 = new RAM(false, 116);
scc5480->add_relation(rel__let__list__3__1__2__3, true);
scc5480->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524409), n2d(524410)}));

RAM* scc5481 = new RAM(false, 45);
scc5481->add_relation(rel__prim__2__1__2, true);
scc5481->add_rule(new fact(rel__prim__2__1__2, {n2d(524358), n2d(524399)}));

RAM* scc5482 = new RAM(false, 124);
scc5482->add_relation(rel__lambda__3__1__2__3, true);
scc5482->add_rule(new fact(rel__lambda__3__1__2__3, {n2d(524471), n2d(524401), n2d(524357)}));

RAM* scc5483 = new RAM(false, 197);
scc5483->add_relation(rel___dollorhead__stratified28__2__, true);
scc5483->add_relation(rel___dollorhead__stratified28__2__1__2, true);
scc5483->add_rule(new parallel_acopy(rel___dollorhead__stratified28__2__, rel___dollorhead__stratified28__2__1__2, DELTA, {2, 0, 1}));

RAM* scc5484 = new RAM(false, 201);
scc5484->add_relation(rel__let__list__3__1__2__3, true);
scc5484->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524407), n2d(524408)}));

RAM* scc5485 = new RAM(false, 62);
scc5485->add_relation(rel__let__list__3__1__2__3, true);
scc5485->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524417), n2d(524418)}));

RAM* scc5486 = new RAM(false, 205);
scc5486->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5486->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524297), n2d(0), n2d(524298)}));

RAM* scc5487 = new RAM(false, 67);
scc5487->add_relation(rel__let__list__3__1__2__3, true);
scc5487->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524421), n2d(524422)}));

RAM* scc5488 = new RAM(false, 71);
scc5488->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5488->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524301), n2d(0), n2d(524302)}));

RAM* scc5489 = new RAM(false, 180);
scc5489->add_relation(rel__call__3__1__2__3, true);
scc5489->add_rule(new fact(rel__call__3__1__2__3, {n2d(524454), n2d(524374), n2d(524307)}));

RAM* scc5490 = new RAM(false, 79);
scc5490->add_relation(rel___dollorhead__stratified23__4__1__2__3__4, true);
scc5490->add_relation(rel___dollorhead__stratified23__4__, true);
scc5490->add_rule(new parallel_acopy(rel___dollorhead__stratified23__4__, rel___dollorhead__stratified23__4__1__2__3__4, DELTA, {4, 0, 1, 2, 3}));

RAM* scc5491 = new RAM(false, 144);
scc5491->add_relation(rel__let__list__3__1__2__3, true);
scc5491->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524413), n2d(524414)}));

RAM* scc5492 = new RAM(false, 193);
scc5492->add_relation(rel__call__3__1__2__3, true);
scc5492->add_rule(new fact(rel__call__3__1__2__3, {n2d(524420), n2d(524391), n2d(524341)}));

RAM* scc5493 = new RAM(false, 152);
scc5493->add_relation(rel__call__3__1__2__3, true);
scc5493->add_rule(new fact(rel__call__3__1__2__3, {n2d(524436), n2d(524383), n2d(524325)}));

RAM* scc5494 = new RAM(false, 156);
scc5494->add_relation(rel__num__2__1__2, true);
scc5494->add_rule(new fact(rel__num__2__1__2, {n2d(524356), n2d(30)}));

RAM* scc5495 = new RAM(false, 18);
scc5495->add_relation(rel__call__3__1__2__3, true);
scc5495->add_rule(new fact(rel__call__3__1__2__3, {n2d(524434), n2d(524384), n2d(524327)}));

RAM* scc5496 = new RAM(false, 22);
scc5496->add_relation(rel__prim__call__3__1__2__3, true);
scc5496->add_rule(new fact(rel__prim__call__3__1__2__3, {n2d(524285), n2d(524359), n2d(524286)}));

RAM* scc5497 = new RAM(false, 26);
scc5497->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5497->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524349), n2d(0), n2d(524350)}));

RAM* scc5498 = new RAM(false, 75);
scc5498->add_relation(rel__call__3__1, true);
scc5498->add_relation(rel__call__3__1__2__3, true);
scc5498->add_rule(new parallel_acopy(rel__call__3__1, rel__call__3__1__2__3, DELTA, {0, 3, 1, 2}));

RAM* scc5499 = new RAM(false, 99);
scc5499->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5499->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524293), n2d(0), n2d(524294)}));

RAM* scc5500 = new RAM(false, 103);
scc5500->add_relation(rel__lambda__arg__list__3__1__2__3, true);
scc5500->add_rule(new fact(rel__lambda__arg__list__3__1__2__3, {n2d(524400), n2d(0), n2d(524362)}));

RAM* scc5501 = new RAM(false, 148);
scc5501->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5501->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524321), n2d(0), n2d(524322)}));

RAM* scc5502 = new RAM(false, 111);
scc5502->add_relation(rel__num__2__1__2, true);
scc5502->add_rule(new fact(rel__num__2__1__2, {n2d(524310), n2d(7)}));

RAM* scc5503 = new RAM(false, 229);
scc5503->add_relation(rel__call__3__1__2__3, true);
scc5503->add_rule(new fact(rel__call__3__1__2__3, {n2d(524406), n2d(524398), n2d(524355)}));

RAM* scc5504 = new RAM(false, 233);
scc5504->add_relation(rel__prim__2__1__2, true);
scc5504->add_rule(new fact(rel__prim__2__1__2, {n2d(524360), n2d(524399)}));

RAM* scc5505 = new RAM(false, 30);
scc5505->add_relation(rel__var__2__1__2, true);
scc5505->add_rule(new fact(rel__var__2__1__2, {n2d(524396), n2d(524367)}));

RAM* scc5506 = new RAM(false, 237);
scc5506->add_relation(rel__call__3__1__2__3, true);
scc5506->add_rule(new fact(rel__call__3__1__2__3, {n2d(524462), n2d(524370), n2d(524299)}));

RAM* scc5507 = new RAM(false, 50);
scc5507->add_relation(rel__let__list__3__1__2__3, true);
scc5507->add_relation(rel__let__list__3__3, true);
scc5507->add_rule(new parallel_acopy(rel__let__list__3__3, rel__let__list__3__1__2__3, DELTA, {2, 3, 0, 1}));

RAM* scc5508 = new RAM(false, 54);
scc5508->add_relation(rel__var__2__1__2, true);
scc5508->add_rule(new fact(rel__var__2__1__2, {n2d(524361), n2d(524362)}));

RAM* scc5509 = new RAM(false, 58);
scc5509->add_relation(rel__call__3__1__2__3, true);
scc5509->add_rule(new fact(rel__call__3__1__2__3, {n2d(524450), n2d(524376), n2d(524311)}));

RAM* scc5510 = new RAM(false, 107);
scc5510->add_relation(rel___dollorinter__body60__2__1__2, true);
scc5510->add_relation(rel___dollorinter__body60__2__2, true);
scc5510->add_rule(new parallel_acopy(rel___dollorinter__body60__2__2, rel___dollorinter__body60__2__1__2, DELTA, {1, 2, 0}));

RAM* scc5511 = new RAM(false, 176);
scc5511->add_relation(rel__lambda__arg__list__3__1__2__3, true);
scc5511->add_rule(new fact(rel__lambda__arg__list__3__1__2__3, {n2d(524403), n2d(0), n2d(524367)}));

RAM* scc5512 = new RAM(false, 225);
scc5512->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5512->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524295), n2d(0), n2d(524296)}));

RAM* scc5513 = new RAM(false, 184);
scc5513->add_relation(rel__num__2__1__2, true);
scc5513->add_rule(new fact(rel__num__2__1__2, {n2d(524294), n2d(0)}));

RAM* scc5514 = new RAM(false, 188);
scc5514->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5514->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524305), n2d(0), n2d(524306)}));

RAM* scc5515 = new RAM(false, 129);
scc5515->add_relation(rel__var__2__1__2, true);
scc5515->add_rule(new fact(rel__var__2__1__2, {n2d(524377), n2d(524367)}));

RAM* scc5516 = new RAM(false, 137);
scc5516->add_relation(rel__lambda__3__1__2__3, true);
scc5516->add_rule(new fact(rel__lambda__3__1__2__3, {n2d(524472), n2d(524403), n2d(524468)}));

RAM* scc5517 = new RAM(false, 122);
scc5517->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5517->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524347), n2d(0), n2d(524348)}));

RAM* scc5518 = new RAM(false, 141);
scc5518->add_relation(rel__call__3__1__2__3, true);
scc5518->add_rule(new fact(rel__call__3__1__2__3, {n2d(524408), n2d(524397), n2d(524353)}));

RAM* scc5519 = new RAM(false, 3);
scc5519->add_relation(rel__call__arg__list__3__2, true);
scc5519->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5519->add_rule(new parallel_acopy(rel__call__arg__list__3__2, rel__call__arg__list__3__1__2__3, DELTA, {1, 3, 0, 2}));

RAM* scc5520 = new RAM(false, 240);
scc5520->add_relation(rel___dollorbir__sub5__4__1__2__3__4, true);
scc5520->add_rule(new parallel_join(rel___dollorbir__sub5__4__1__2__3__4, rel___dollorinter__body59__2__2, FULL, rel___dollorinter__body61__4__4, FULL, {6, 4, 2, 5}));

RAM* scc5521 = new RAM(false, 7);
scc5521->add_relation(rel__call__3__1__2__3, true);
scc5521->add_rule(new fact(rel__call__3__1__2__3, {n2d(524444), n2d(524379), n2d(524317)}));

RAM* scc5522 = new RAM(false, 11);
scc5522->add_relation(rel__num__2__1__2, true);
scc5522->add_rule(new fact(rel__num__2__1__2, {n2d(524324), n2d(14)}));

RAM* scc5523 = new RAM(false, 133);
scc5523->add_relation(rel__num__2__1__2, true);
scc5523->add_rule(new fact(rel__num__2__1__2, {n2d(524332), n2d(18)}));

RAM* scc5524 = new RAM(false, 212);
scc5524->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5524->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524303), n2d(0), n2d(524304)}));

RAM* scc5525 = new RAM(false, 216);
scc5525->add_relation(rel__var__2__1__2, true);
scc5525->add_rule(new fact(rel__var__2__1__2, {n2d(524388), n2d(524367)}));

RAM* scc5526 = new RAM(false, 220);
scc5526->add_relation(rel__var__2__1__2, true);
scc5526->add_rule(new fact(rel__var__2__1__2, {n2d(524366), n2d(524367)}));

RAM* scc5527 = new RAM(false, 82);
scc5527->add_relation(rel__var__2__1__2, true);
scc5527->add_rule(new fact(rel__var__2__1__2, {n2d(524379), n2d(524367)}));

RAM* scc5528 = new RAM(false, 86);
scc5528->add_relation(rel__flow__ea__2__1__2, true);
scc5528->add_rule(new parallel_copy(rel__flow__ea__2__1__2, rel___dollorinter__head11__3__1__2__3, FULL, {2, 1}));

RAM* scc5529 = new RAM(false, 15);
scc5529->add_relation(rel__num__2__1__2, true);
scc5529->add_rule(new fact(rel__num__2__1__2, {n2d(524344), n2d(24)}));

RAM* scc5530 = new RAM(false, 94);
scc5530->add_relation(rel___dollorinter__head29__3__, true);
scc5530->add_relation(rel___dollorinter__head29__3__1__2__3, true);
scc5530->add_rule(new parallel_acopy(rel___dollorinter__head29__3__, rel___dollorinter__head29__3__1__2__3, DELTA, {3, 0, 1, 2}));

RAM* scc5531 = new RAM(false, 35);
scc5531->add_relation(rel__num__2__1__2, true);
scc5531->add_rule(new fact(rel__num__2__1__2, {n2d(524316), n2d(10)}));

RAM* scc5532 = new RAM(false, 208);
scc5532->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5532->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524329), n2d(0), n2d(524330)}));

RAM* scc5533 = new RAM(false, 39);
scc5533->add_relation(rel__call__3__1__2__3, true);
scc5533->add_rule(new fact(rel__call__3__1__2__3, {n2d(524448), n2d(524377), n2d(524313)}));

RAM* scc5534 = new RAM(false, 43);
scc5534->add_relation(rel___dollorbir__sub1__4__1__2__3__4, true);
scc5534->add_relation(rel___dollorbir__sub1__4__1, true);
scc5534->add_rule(new parallel_acopy(rel___dollorbir__sub1__4__1, rel___dollorbir__sub1__4__1__2__3__4, DELTA, {0, 4, 1, 2, 3}));

RAM* scc5535 = new RAM(false, 161);
scc5535->add_relation(rel__var__2__1__2, true);
scc5535->add_rule(new fact(rel__var__2__1__2, {n2d(524370), n2d(524367)}));

RAM* scc5536 = new RAM(false, 90);
scc5536->add_relation(rel___dollorinter__body61__4__1__2__3__4, true);
scc5536->add_rule(new parallel_join(rel___dollorinter__body61__4__1__2__3__4, rel___dollorinter__body60__2__2, FULL, rel__prim__call__3__3, FULL, {5, 2, 4, 0}));

RAM* scc5537 = new RAM(false, 169);
scc5537->add_relation(rel__num__2__1__2, true);
scc5537->add_rule(new fact(rel__num__2__1__2, {n2d(524352), n2d(28)}));

RAM* scc5538 = new RAM(false, 173);
scc5538->add_relation(rel__let__list__3__1__2__3, true);
scc5538->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524365), n2d(524467)}));

RAM* scc5539 = new RAM(false, 114);
scc5539->add_relation(rel___dollorlst__2__1__2, true);
scc5539->add_rule(new parallel_copy(rel___dollorlst__2__1__2, rel___dollorhead__stratified35__4__1__2__3__4, FULL, {1, 3}));

RAM* scc5540 = new RAM(false, 118);
scc5540->add_relation(rel__var__2__1__2, true);
scc5540->add_rule(new fact(rel__var__2__1__2, {n2d(524374), n2d(524367)}));

RAM* scc5541 = new RAM(false, 47);
scc5541->add_relation(rel__num__2__1__2, true);
scc5541->add_rule(new fact(rel__num__2__1__2, {n2d(524342), n2d(23)}));

RAM* scc5542 = new RAM(false, 126);
scc5542->add_relation(rel__num__2__1__2, true);
scc5542->add_rule(new fact(rel__num__2__1__2, {n2d(524296), n2d(0)}));

RAM* scc5543 = new RAM(false, 165);
scc5543->add_relation(rel___dollorbir__sub6__3__1__2__3, true);
scc5543->add_rule(new parallel_copy(rel___dollorbir__sub6__3__1__2__3, rel__call__arg__list__3__1__2__3, FULL, {0, 1, 2}));

RAM* scc5544 = new RAM(false, 244);
scc5544->add_relation(rel__var__2__1__2, true);
scc5544->add_rule(new fact(rel__var__2__1__2, {n2d(524389), n2d(524367)}));

RAM* scc5545 = new RAM(false, 248);
scc5545->add_relation(rel___dollorinter__head26__3__1__2__3, true);
scc5545->add_relation(rel___dollorinter__head26__3__, true);
scc5545->add_rule(new parallel_acopy(rel___dollorinter__head26__3__, rel___dollorinter__head26__3__1__2__3, DELTA, {3, 0, 1, 2}));

RAM* scc5546 = new RAM(false, 252);
scc5546->add_relation(rel__call__3__1__2__3, true);
scc5546->add_rule(new fact(rel__call__3__1__2__3, {n2d(524467), n2d(524366), n2d(524293)}));

RAM* scc5547 = new RAM(false, 130);
scc5547->add_relation(rel__flow__ea__2__1__2, true);
scc5547->add_rule(new parallel_copy(rel__flow__ea__2__1__2, rel___dollorinter__head29__3__1__2__3, FULL, {2, 1}));

RAM* scc5548 = new RAM(false, 69);
scc5548->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5548->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524343), n2d(0), n2d(524344)}));

RAM* scc5549 = new RAM(false, 182);
scc5549->add_relation(rel__flow__ea__2__1__2, true);
scc5549->add_rule(new parallel_copy(rel__flow__ea__2__1__2, rel___dollorinter__head17__3__1__2__3, FULL, {2, 0}));

RAM* scc5550 = new RAM(false, 77);
scc5550->add_relation(rel___dollorinter__head23__5__, true);
scc5550->add_relation(rel___dollorinter__head23__5__1__2__3__4__5, true);
scc5550->add_rule(new parallel_acopy(rel___dollorinter__head23__5__, rel___dollorinter__head23__5__1__2__3__4__5, DELTA, {5, 0, 1, 2, 3, 4}));

RAM* scc5551 = new RAM(false, 0);
scc5551->add_relation(rel__call__3__1__2__3, true);
scc5551->add_rule(new fact(rel__call__3__1__2__3, {n2d(524424), n2d(524389), n2d(524337)}));

RAM* scc5552 = new RAM(false, 199);
scc5552->add_relation(rel___dollorbir__sub__2__1__2, true);
scc5552->add_relation(rel___dollorbir__sub__2__2__1, true);
scc5552->add_rule(new parallel_acopy(rel___dollorbir__sub__2__2__1, rel___dollorbir__sub__2__1__2, DELTA, {1, 0, 2}));

RAM* scc5553 = new RAM(false, 203);
scc5553->add_relation(rel__let__list__3__1__2__3, true);
scc5553->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524445), n2d(524446)}));

RAM* scc5554 = new RAM(false, 60);
scc5554->add_relation(rel__prim__2__1__2, true);
scc5554->add_rule(new fact(rel__prim__2__1__2, {n2d(524359), n2d(524399)}));

RAM* scc5555 = new RAM(false, 207);
scc5555->add_relation(rel__var__2__1__2, true);
scc5555->add_rule(new fact(rel__var__2__1__2, {n2d(524372), n2d(524367)}));

RAM* scc5556 = new RAM(false, 20);
scc5556->add_relation(rel__num__2__1__2, true);
scc5556->add_rule(new fact(rel__num__2__1__2, {n2d(524302), n2d(3)}));

RAM* scc5557 = new RAM(false, 24);
scc5557->add_relation(rel__flow__aa__2__1__2, true);
scc5557->add_rule(new parallel_copy(rel__flow__aa__2__1__2, rel___dollorinter__head24__2__1__2, FULL, {1, 1}));

RAM* scc5558 = new RAM(false, 73);
scc5558->add_relation(rel___dollorinter__body61__4__1__2__3__4, true);
scc5558->add_relation(rel___dollorinter__body61__4__4, true);
scc5558->add_rule(new parallel_acopy(rel___dollorinter__body61__4__4, rel___dollorinter__body61__4__1__2__3__4, DELTA, {3, 4, 0, 1, 2}));

RAM* scc5559 = new RAM(false, 81);
scc5559->add_relation(rel__let__list__3__1__2__3, true);
scc5559->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524453), n2d(524454)}));

RAM* scc5560 = new RAM(false, 154);
scc5560->add_relation(rel__var__2__1__2, true);
scc5560->add_rule(new fact(rel__var__2__1__2, {n2d(524292), n2d(524363)}));

RAM* scc5561 = new RAM(false, 158);
scc5561->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5561->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524286), n2d(1), n2d(524287)}));

RAM* scc5562 = new RAM(false, 32);
scc5562->add_relation(rel___dollorinter__head24__2__, true);
scc5562->add_relation(rel___dollorinter__head24__2__1__2, true);
scc5562->add_rule(new parallel_acopy(rel___dollorinter__head24__2__, rel___dollorinter__head24__2__1__2, DELTA, {2, 0, 1}));

RAM* scc5563 = new RAM(false, 211);
scc5563->add_relation(rel__call__3__1__2__3, true);
scc5563->add_rule(new fact(rel__call__3__1__2__3, {n2d(524416), n2d(524393), n2d(524345)}));

RAM* scc5564 = new RAM(false, 231);
scc5564->add_relation(rel__call__3__1__2__3, true);
scc5564->add_rule(new fact(rel__call__3__1__2__3, {n2d(524446), n2d(524378), n2d(524315)}));

RAM* scc5565 = new RAM(false, 235);
scc5565->add_relation(rel__var__2__1__2, true);
scc5565->add_rule(new fact(rel__var__2__1__2, {n2d(524290), n2d(524363)}));

RAM* scc5566 = new RAM(false, 28);
scc5566->add_relation(rel__lambda__arg__list__3__1__2__3, true);
scc5566->add_rule(new fact(rel__lambda__arg__list__3__1__2__3, {n2d(524401), n2d(0), n2d(524362)}));

RAM* scc5567 = new RAM(false, 239);
scc5567->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5567->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524309), n2d(0), n2d(524310)}));

RAM* scc5568 = new RAM(false, 162);
scc5568->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5568->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524284), n2d(1), n2d(524285)}));

RAM* scc5569 = new RAM(false, 101);
scc5569->add_relation(rel__var__2__1__2, true);
scc5569->add_rule(new fact(rel__var__2__1__2, {n2d(524380), n2d(524367)}));

RAM* scc5570 = new RAM(true, 150);
scc5570->add_relation(rel___dollorinter__body51__3__1__3, true);
scc5570->add_relation(rel___dollorinter__body50__3__1, true);
scc5570->add_relation(rel___dollorinter__body51__3__1__2__3, true);
scc5570->add_relation(rel___dollorinter__body56__3__2__3, true);
scc5570->add_relation(rel___dollorinter__body50__3__1__2__3, true);
scc5570->add_relation(rel__free__2__2, true);
scc5570->add_relation(rel__free__2__1__2, true);
scc5570->add_relation(rel___dollorinter__body56__3__1__2__3, true);
scc5570->add_rule(new parallel_acopy(rel___dollorinter__body50__3__1, rel___dollorinter__body50__3__1__2__3, DELTA, {0, 3, 1, 2}));
scc5570->add_rule(new parallel_join(rel___dollorinter__body56__3__1__2__3, rel__free__2__2, DELTA, rel__let__list__3__3, FULL, {4, 5, 2}));
scc5570->add_rule(new parallel_acopy(rel___dollorinter__body51__3__1__3, rel___dollorinter__body51__3__1__2__3, DELTA, {0, 2, 3, 1}));
scc5570->add_rule(new parallel_join(rel__free__2__1__2, rel__free__2__2, DELTA, rel__setb__3__3, FULL, {2, 4}));
scc5570->add_rule(new parallel_join(rel__free__2__1__2, rel__free__2__2, DELTA, rel__call__3__3, FULL, {2, 4}));
scc5570->add_rule(new parallel_join(rel__free__2__1__2, rel__free__2__2, DELTA, rel__let__3__3, FULL, {2, 4}));
scc5570->add_rule(new parallel_join(rel__free__2__1__2, rel__free__2__2, DELTA, rel__prim__call__3__3, FULL, {2, 4}));
scc5570->add_rule(new parallel_join(rel___dollorinter__body51__3__1__2__3, rel__lambda__arg__list__3__1, FULL, rel___dollorinter__body50__3__1, DELTA, {3, 5, 6}));
scc5570->add_rule(new parallel_join(rel__free__2__1__2, rel__free__2__2, DELTA, rel__call__3__2, FULL, {2, 4}));
scc5570->add_rule(new parallel_join(rel__free__2__1__2, rel__free__2__2, DELTA, rel__call__arg__list__3__3, FULL, {2, 4}));
scc5570->add_rule(new parallel_join(rel___dollorinter__body50__3__1__2__3, rel__free__2__2, DELTA, rel__lambda__3__3, FULL, {5, 4, 2}));
scc5570->add_rule(new parallel_copy_generate(rel__free__2__1__2, rel___dollorinter__body56__3__2__3, DELTA, [](const u64* data, u64* const output) -> int{
      auto args_for_old_bi = std::array<u64, 2> {data[1], data[0]};
      using TState = std::tuple<const u64*,u64*>;
      TState state = {data, output};
      auto callback = []( TState state) -> TState{
        auto [data, output] = state;
        bool compatible = true;
        if (! compatible) return state;

        auto head_tuple = output;
        head_tuple[0] = data[1];
head_tuple[1] = data[3];
        return {data, output + 2};
      };
      auto [_,new_ptr] = builtin_neq<TState>(args_for_old_bi.data(), state, callback);
      auto tuples_count = (new_ptr - output) / 2;
      return tuples_count;
    }));
scc5570->add_rule(new parallel_acopy(rel___dollorinter__body56__3__2__3, rel___dollorinter__body56__3__1__2__3, DELTA, {1, 2, 3, 0}));
scc5570->add_rule(new parallel_copy_generate(rel__free__2__1__2, rel___dollorinter__body51__3__1__3, DELTA, [](const u64* data, u64* const output) -> int{
      auto args_for_old_bi = std::array<u64, 2> {data[1], data[0]};
      using TState = std::tuple<const u64*,u64*>;
      TState state = {data, output};
      auto callback = []( TState state) -> TState{
        auto [data, output] = state;
        bool compatible = true;
        if (! compatible) return state;

        auto head_tuple = output;
        head_tuple[0] = data[1];
head_tuple[1] = data[3];
        return {data, output + 2};
      };
      auto [_,new_ptr] = builtin_neq<TState>(args_for_old_bi.data(), state, callback);
      auto tuples_count = (new_ptr - output) / 2;
      return tuples_count;
    }));
scc5570->add_rule(new parallel_acopy(rel__free__2__2, rel__free__2__1__2, DELTA, {1, 2, 0}));
scc5570->add_rule(new parallel_join(rel__free__2__1__2, rel__free__2__2, DELTA, rel__let__3__2, FULL, {2, 4}));
scc5570->add_rule(new parallel_join(rel__free__2__1__2, rel__free__2__2, DELTA, rel__if__4__3, FULL, {2, 4}));
scc5570->add_rule(new parallel_join(rel__free__2__1__2, rel__free__2__2, DELTA, rel__if__4__2, FULL, {2, 4}));
scc5570->add_rule(new parallel_join(rel__free__2__1__2, rel__free__2__2, DELTA, rel__if__4__4, FULL, {2, 4}));
scc5570->add_rule(new parallel_join(rel__free__2__1__2, rel__free__2__2, DELTA, rel__callcc__2__2, FULL, {2, 4}));

RAM* scc5571 = new RAM(false, 109);
scc5571->add_relation(rel___dollorinter__head5__4__1__2__3__4, true);
scc5571->add_relation(rel___dollorinter__head5__4__, true);
scc5571->add_rule(new parallel_acopy(rel___dollorinter__head5__4__, rel___dollorinter__head5__4__1__2__3__4, DELTA, {4, 0, 1, 2, 3}));

RAM* scc5572 = new RAM(false, 113);
scc5572->add_relation(rel___dollorhead__stratified12__4__, true);
scc5572->add_relation(rel___dollorhead__stratified12__4__1__2__3__4, true);
scc5572->add_rule(new parallel_acopy(rel___dollorhead__stratified12__4__, rel___dollorhead__stratified12__4__1__2__3__4, DELTA, {4, 0, 1, 2, 3}));

RAM* scc5573 = new RAM(false, 186);
scc5573->add_relation(rel__num__2__1__2, true);
scc5573->add_rule(new fact(rel__num__2__1__2, {n2d(524354), n2d(29)}));

RAM* scc5574 = new RAM(false, 190);
scc5574->add_relation(rel__flow__ea__2__1__2, true);
scc5574->add_rule(new parallel_copy(rel__flow__ea__2__1__2, rel___dollorinter__head26__3__1__2__3, FULL, {1, 2}));

RAM* scc5575 = new RAM(false, 243);
scc5575->add_relation(rel___dollorbir__sub3__4__1__2__3__4, true);
scc5575->add_rule(new parallel_join(rel___dollorbir__sub3__4__1__2__3__4, rel__lambda__3__2, FULL, rel__lambda__arg__list__3__1, FULL, {2, 3, 5, 6}));

RAM* scc5576 = new RAM(false, 52);
scc5576->add_relation(rel__let__list__3__1__2__3, true);
scc5576->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524443), n2d(524444)}));

RAM* scc5577 = new RAM(false, 56);
scc5577->add_relation(rel__num__2__1__2, true);
scc5577->add_rule(new fact(rel__num__2__1__2, {n2d(524314), n2d(9)}));

RAM* scc5578 = new RAM(false, 105);
scc5578->add_relation(rel__num__2__1__2, true);
scc5578->add_rule(new fact(rel__num__2__1__2, {n2d(524326), n2d(15)}));

RAM* scc5579 = new RAM(false, 261);
scc5579->add_relation(rel__let__list__3__1__2__3, true);
scc5579->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524425), n2d(524426)}));

RAM* scc5580 = new RAM(false, 257);
scc5580->add_relation(rel__var__2__1__2, true);
scc5580->add_rule(new fact(rel__var__2__1__2, {n2d(524375), n2d(524367)}));

RAM* scc5581 = new RAM(false, 265);
scc5581->add_relation(rel__var__2__1__2, true);
scc5581->add_rule(new fact(rel__var__2__1__2, {n2d(524376), n2d(524367)}));

RAM* scc5582 = new RAM(false, 269);
scc5582->add_relation(rel__lambda__3__1__2__3, true);
scc5582->add_relation(rel__lambda__3__1, true);
scc5582->add_rule(new parallel_acopy(rel__lambda__3__1, rel__lambda__3__1__2__3, DELTA, {0, 3, 1, 2}));

RAM* scc5583 = new RAM(false, 278);
scc5583->add_relation(rel__call__3__1__2__3, true);
scc5583->add_rule(new fact(rel__call__3__1__2__3, {n2d(524470), n2d(524471), n2d(524282)}));

RAM* scc5584 = new RAM(false, 274);
scc5584->add_relation(rel__call__3__1__2__3, true);
scc5584->add_rule(new fact(rel__call__3__1__2__3, {n2d(524464), n2d(524369), n2d(524297)}));

RAM* scc5585 = new RAM(false, 282);
scc5585->add_relation(rel__let__list__3__1__2__3, true);
scc5585->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524455), n2d(524456)}));

RAM* scc5586 = new RAM(false, 286);
scc5586->add_relation(rel__setb__3__3, true);
scc5586->add_relation(rel__setb__3__1__2__3, true);
scc5586->add_rule(new parallel_acopy(rel__setb__3__3, rel__setb__3__1__2__3, DELTA, {2, 3, 0, 1}));

RAM* scc5587 = new RAM(false, 263);
scc5587->add_relation(rel__call__3__1__2__3, true);
scc5587->add_rule(new fact(rel__call__3__1__2__3, {n2d(524410), n2d(524396), n2d(524351)}));

RAM* scc5588 = new RAM(false, 259);
scc5588->add_relation(rel__flow__aa__2__1__2, true);
scc5588->add_rule(new parallel_copy(rel__flow__aa__2__1__2, rel___dollorhead__stratified12__4__1__2__3__4, FULL, {0, 3}));

RAM* scc5589 = new RAM(false, 267);
scc5589->add_relation(rel__call__3__1__2__3, true);
scc5589->add_rule(new fact(rel__call__3__1__2__3, {n2d(524456), n2d(524373), n2d(524305)}));

RAM* scc5590 = new RAM(false, 271);
scc5590->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5590->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524280), n2d(0), n2d(524281)}));

RAM* scc5591 = new RAM(false, 272);
scc5591->add_relation(rel__let__list__3__1__2__3, true);
scc5591->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524459), n2d(524460)}));

RAM* scc5592 = new RAM(false, 280);
scc5592->add_relation(rel__var__2__1__2, true);
scc5592->add_rule(new fact(rel__var__2__1__2, {n2d(524382), n2d(524367)}));

RAM* scc5593 = new RAM(false, 284);
scc5593->add_relation(rel___dollorinter__head11__3__1__2__3, true);
scc5593->add_relation(rel___dollorinter__head11__3__, true);
scc5593->add_rule(new parallel_acopy(rel___dollorinter__head11__3__, rel___dollorinter__head11__3__1__2__3, DELTA, {3, 0, 1, 2}));

RAM* scc5594 = new RAM(false, 260);
scc5594->add_relation(rel__if__4__1, true);
scc5594->add_relation(rel__if__4__1__2__3__4, true);
scc5594->add_rule(new parallel_acopy(rel__if__4__1, rel__if__4__1__2__3__4, DELTA, {0, 4, 1, 2, 3}));

RAM* scc5595 = new RAM(false, 198);
scc5595->add_relation(rel__var__2__1__2, true);
scc5595->add_rule(new fact(rel__var__2__1__2, {n2d(524392), n2d(524367)}));

RAM* scc5596 = new RAM(false, 202);
scc5596->add_relation(rel__let__list__3__1__2__3, true);
scc5596->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524427), n2d(524428)}));

RAM* scc5597 = new RAM(false, 61);
scc5597->add_relation(rel__var__2__1__2, true);
scc5597->add_rule(new fact(rel__var__2__1__2, {n2d(524364), n2d(524365)}));

RAM* scc5598 = new RAM(false, 206);
scc5598->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5598->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524282), n2d(0), n2d(524283)}));

RAM* scc5599 = new RAM(false, 64);
scc5599->add_relation(rel__let__list__3__1__2__3, true);
scc5599->add_relation(rel__let__list__3__1, true);
scc5599->add_rule(new parallel_acopy(rel__let__list__3__1, rel__let__list__3__1__2__3, DELTA, {0, 3, 1, 2}));

RAM* scc5600 = new RAM(false, 68);
scc5600->add_relation(rel___dollorinter__body81__2__1__2, true);
scc5600->add_rule(new parallel_join(rel___dollorinter__body81__2__1__2, rel___dollornil__0__, FULL, rel__top__exp__1__, FULL, {0, 2}));

RAM* scc5601 = new RAM(false, 72);
scc5601->add_relation(rel___dollorhead__stratified__4__1__2__3__4, true);
scc5601->add_relation(rel___dollorhead__stratified__4__, true);
scc5601->add_rule(new parallel_acopy(rel___dollorhead__stratified__4__, rel___dollorhead__stratified__4__1__2__3__4, DELTA, {4, 0, 1, 2, 3}));

RAM* scc5602 = new RAM(false, 76);
scc5602->add_relation(rel__lambda__3__1__2__3, true);
scc5602->add_relation(rel__lambda__3__2, true);
scc5602->add_rule(new parallel_acopy(rel__lambda__3__2, rel__lambda__3__1__2__3, DELTA, {1, 3, 0, 2}));

RAM* scc5603 = new RAM(false, 147);
scc5603->add_relation(rel__num__2__1__2, true);
scc5603->add_rule(new fact(rel__num__2__1__2, {n2d(524328), n2d(16)}));

RAM* scc5604 = new RAM(false, 194);
scc5604->add_relation(rel__let__list__3__1__2__3, true);
scc5604->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524441), n2d(524442)}));

RAM* scc5605 = new RAM(false, 155);
scc5605->add_relation(rel___dollorhead__stratified9__4__1__2__3__4, true);
scc5605->add_relation(rel___dollorhead__stratified9__4__, true);
scc5605->add_rule(new parallel_acopy(rel___dollorhead__stratified9__4__, rel___dollorhead__stratified9__4__1__2__3__4, DELTA, {4, 0, 1, 2, 3}));

RAM* scc5606 = new RAM(false, 159);
scc5606->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5606->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524351), n2d(0), n2d(524352)}));

RAM* scc5607 = new RAM(false, 17);
scc5607->add_relation(rel__num__2__1__2, true);
scc5607->add_rule(new fact(rel__num__2__1__2, {n2d(524340), n2d(22)}));

RAM* scc5608 = new RAM(false, 21);
scc5608->add_relation(rel__let__list__3__1__2__3, true);
scc5608->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524463), n2d(524464)}));

RAM* scc5609 = new RAM(false, 25);
scc5609->add_relation(rel__call__arg__list__3__3, true);
scc5609->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5609->add_rule(new parallel_acopy(rel__call__arg__list__3__3, rel__call__arg__list__3__1__2__3, DELTA, {2, 3, 0, 1}));

RAM* scc5610 = new RAM(false, 29);
scc5610->add_relation(rel__call__3__1__2__3, true);
scc5610->add_rule(new fact(rel__call__3__1__2__3, {n2d(524426), n2d(524388), n2d(524335)}));

RAM* scc5611 = new RAM(false, 96);
scc5611->add_relation(rel__setb__3__1, true);
scc5611->add_relation(rel__setb__3__1__2__3, true);
scc5611->add_rule(new parallel_acopy(rel__setb__3__1, rel__setb__3__1__2__3, DELTA, {0, 3, 1, 2}));

RAM* scc5612 = new RAM(false, 100);
scc5612->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5612->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524355), n2d(0), n2d(524356)}));

RAM* scc5613 = new RAM(false, 151);
scc5613->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5613->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524313), n2d(0), n2d(524314)}));

RAM* scc5614 = new RAM(false, 108);
scc5614->add_relation(rel__top__exp__1__1, true);
scc5614->add_rule(new fact(rel__top__exp__1__1, {n2d(524469)}));

RAM* scc5615 = new RAM(false, 226);
scc5615->add_relation(rel___dollorhead__stratified16__4__1__2__3__4, true);
scc5615->add_relation(rel___dollorhead__stratified16__4__, true);
scc5615->add_rule(new parallel_acopy(rel___dollorhead__stratified16__4__, rel___dollorhead__stratified16__4__1__2__3__4, DELTA, {4, 0, 1, 2, 3}));

RAM* scc5616 = new RAM(false, 230);
scc5616->add_relation(rel__let__list__3__1__2__3, true);
scc5616->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524405), n2d(524406)}));

RAM* scc5617 = new RAM(false, 234);
scc5617->add_relation(rel___dollorinter__body59__2__1__2, true);
scc5617->add_rule(new parallel_copy_generate(rel___dollorinter__body59__2__1__2, rel__call__arg__list__3__2, FULL, [](const u64* data, u64* const output) -> int{
      auto args_for_old_bi = std::array<u64, 2> {data[0], data[1]};
      using TState = std::tuple<const u64*,u64*>;
      TState state = {data, output};
      auto callback = []( TState state) -> TState{
        auto [data, output] = state;
        bool compatible = true;
        if (! compatible) return state;

        auto head_tuple = output;
        head_tuple[0] = data[3];
head_tuple[1] = data[2];
        return {data, output + 2};
      };
      auto [_,new_ptr] = builtin_eq<TState>(args_for_old_bi.data(), state, callback);
      auto tuples_count = (new_ptr - output) / 2;
      return tuples_count;
    }));

RAM* scc5618 = new RAM(false, 238);
scc5618->add_relation(rel__num__2__1__2, true);
scc5618->add_rule(new fact(rel__num__2__1__2, {n2d(524338), n2d(21)}));

RAM* scc5619 = new RAM(false, 49);
scc5619->add_relation(rel___dollorhead__stratified33__4__, true);
scc5619->add_relation(rel___dollorhead__stratified33__4__1__2__3__4, true);
scc5619->add_rule(new parallel_acopy(rel___dollorhead__stratified33__4__, rel___dollorhead__stratified33__4__1__2__3__4, DELTA, {4, 0, 1, 2, 3}));

RAM* scc5620 = new RAM(false, 53);
scc5620->add_relation(rel__let__list__3__1__2__3, true);
scc5620->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524431), n2d(524432)}));

RAM* scc5621 = new RAM(false, 57);
scc5621->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5621->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524299), n2d(0), n2d(524300)}));

RAM* scc5622 = new RAM(false, 104);
scc5622->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5622->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524323), n2d(0), n2d(524324)}));

RAM* scc5623 = new RAM(false, 179);
scc5623->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5623->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524333), n2d(0), n2d(524334)}));

RAM* scc5624 = new RAM(false, 183);
scc5624->add_relation(rel__lambda__3__1__2__3, true);
scc5624->add_rule(new fact(rel__lambda__3__1__2__3, {n2d(524281), n2d(524402), n2d(524470)}));

RAM* scc5625 = new RAM(false, 187);
scc5625->add_relation(rel___dollornil__0__0, true);
scc5625->add_relation(rel___dollornil__0__, true);
scc5625->add_rule(new parallel_acopy(rel___dollornil__0__0, rel___dollornil__0__, DELTA, {0}));

RAM* scc5626 = new RAM(false, 191);
scc5626->add_relation(rel___dollorinter__body80__2__1__2, true);
scc5626->add_rule(new parallel_join(rel___dollorinter__body80__2__1__2, rel__mt__0__, FULL, rel__null__0__, FULL, {1, 0}));

RAM* scc5627 = new RAM(false, 2);
scc5627->add_relation(rel__call__3__1__2__3, true);
scc5627->add_rule(new fact(rel__call__3__1__2__3, {n2d(524440), n2d(524381), n2d(524321)}));

RAM* scc5628 = new RAM(false, 241);
scc5628->add_relation(rel__call__3__1__2__3, true);
scc5628->add_rule(new fact(rel__call__3__1__2__3, {n2d(524469), n2d(524472), n2d(524280)}));

RAM* scc5629 = new RAM(false, 6);
scc5629->add_relation(rel__var__2__1__2, true);
scc5629->add_rule(new fact(rel__var__2__1__2, {n2d(524291), n2d(524363)}));

RAM* scc5630 = new RAM(false, 10);
scc5630->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5630->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524325), n2d(0), n2d(524326)}));

RAM* scc5631 = new RAM(false, 128);
scc5631->add_relation(rel__let__list__3__1__2__3, true);
scc5631->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524449), n2d(524450)}));

RAM* scc5632 = new RAM(false, 132);
scc5632->add_relation(rel___dollorhead__stratified35__4__1__2__3__4, true);
scc5632->add_rule(new parallel_join(rel___dollorhead__stratified35__4__1__2__3__4, rel___dollorinter__body80__2__, FULL, rel___dollorinter__body81__2__, FULL, {5, 1, 2, 4}));

RAM* scc5633 = new RAM(false, 136);
scc5633->add_relation(rel___dollorinter__body60__2__1__2, true);
scc5633->add_rule(new parallel_copy_generate(rel___dollorinter__body60__2__1__2, rel__call__arg__list__3__2, FULL, [](const u64* data, u64* const output) -> int{
      auto args_for_old_bi = std::array<u64, 2> {data[0], data[1]};
      using TState = std::tuple<const u64*,u64*>;
      TState state = {data, output};
      auto callback = []( TState state) -> TState{
        auto [data, output] = state;
        bool compatible = true;
        if (! compatible) return state;

        auto head_tuple = output;
        head_tuple[0] = data[3];
head_tuple[1] = data[2];
        return {data, output + 2};
      };
      auto [_,new_ptr] = builtin_eq<TState>(args_for_old_bi.data(), state, callback);
      auto tuples_count = (new_ptr - output) / 2;
      return tuples_count;
    }));

RAM* scc5634 = new RAM(false, 140);
scc5634->add_relation(rel__var__2__1__2, true);
scc5634->add_rule(new fact(rel__var__2__1__2, {n2d(524289), n2d(524363)}));

RAM* scc5635 = new RAM(false, 83);
scc5635->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5635->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524288), n2d(0), n2d(524290)}));

RAM* scc5636 = new RAM(false, 87);
scc5636->add_relation(rel__var__2__1__2, true);
scc5636->add_rule(new fact(rel__var__2__1__2, {n2d(524378), n2d(524367)}));

RAM* scc5637 = new RAM(false, 14);
scc5637->add_relation(rel___dollorhead__stratified31__2__, true);
scc5637->add_relation(rel___dollorhead__stratified31__2__1__2, true);
scc5637->add_rule(new parallel_acopy(rel___dollorhead__stratified31__2__, rel___dollorhead__stratified31__2__1__2, DELTA, {2, 0, 1}));

RAM* scc5638 = new RAM(false, 95);
scc5638->add_relation(rel___dollorhead__stratified35__4__4__2, true);
scc5638->add_relation(rel___dollorhead__stratified35__4__1__2__3__4, true);
scc5638->add_rule(new parallel_acopy(rel___dollorhead__stratified35__4__4__2, rel___dollorhead__stratified35__4__1__2__3__4, DELTA, {3, 1, 4, 0, 2}));

RAM* scc5639 = new RAM(false, 209);
scc5639->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5639->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524339), n2d(0), n2d(524340)}));

RAM* scc5640 = new RAM(false, 213);
scc5640->add_relation(rel__if__4__1__2__3__4, true);
scc5640->add_relation(rel__if__4__2, true);
scc5640->add_rule(new parallel_acopy(rel__if__4__2, rel__if__4__1__2__3__4, DELTA, {1, 4, 0, 2, 3}));

RAM* scc5641 = new RAM(false, 217);
scc5641->add_relation(rel___dollorbir__sub1__4__1__2__3__4, true);
scc5641->add_rule(new parallel_join(rel___dollorbir__sub1__4__1__2__3__4, rel__let__list__3__1, FULL, rel__let__3__2, FULL, {5, 6, 2, 3}));

RAM* scc5642 = new RAM(false, 221);
scc5642->add_relation(rel__call__3__1__2__3, true);
scc5642->add_rule(new fact(rel__call__3__1__2__3, {n2d(524432), n2d(524385), n2d(524329)}));

RAM* scc5643 = new RAM(false, 160);
scc5643->add_relation(rel__call__3__1__2__3, true);
scc5643->add_rule(new fact(rel__call__3__1__2__3, {n2d(524442), n2d(524380), n2d(524319)}));

RAM* scc5644 = new RAM(false, 91);
scc5644->add_relation(rel__flow__aa__2__1__2, true);
scc5644->add_rule(new parallel_copy(rel__flow__aa__2__1__2, rel___dollorinter__head4__3__1__2__3, FULL, {2, 0}));

RAM* scc5645 = new RAM(false, 168);
scc5645->add_relation(rel___dollorbir__sub__2__1__2, true);
scc5645->add_rule(new parallel_copy(rel___dollorbir__sub__2__1__2, rel__free__2__1__2, FULL, {0, 1}));

RAM* scc5646 = new RAM(false, 172);
scc5646->add_relation(rel__num__2__1__2, true);
scc5646->add_rule(new fact(rel__num__2__1__2, {n2d(524304), n2d(4)}));

RAM* scc5647 = new RAM(false, 34);
scc5647->add_relation(rel__lambda__arg__list__3__1__2__3, true);
scc5647->add_rule(new fact(rel__lambda__arg__list__3__1__2__3, {n2d(524402), n2d(0), n2d(524363)}));

RAM* scc5648 = new RAM(false, 38);
scc5648->add_relation(rel__let__list__3__1__2__3, true);
scc5648->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524419), n2d(524420)}));

RAM* scc5649 = new RAM(false, 42);
scc5649->add_relation(rel__num__2__1__2, true);
scc5649->add_rule(new fact(rel__num__2__1__2, {n2d(524320), n2d(12)}));

RAM* scc5650 = new RAM(false, 46);
scc5650->add_relation(rel__var__2__1__2, true);
scc5650->add_rule(new fact(rel__var__2__1__2, {n2d(524397), n2d(524367)}));

RAM* scc5651 = new RAM(false, 164);
scc5651->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5651->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524288), n2d(1), n2d(524289)}));

RAM* scc5652 = new RAM(false, 245);
scc5652->add_relation(rel___dollorinter__body80__2__, true);
scc5652->add_relation(rel___dollorinter__body80__2__1__2, true);
scc5652->add_rule(new parallel_acopy(rel___dollorinter__body80__2__, rel___dollorinter__body80__2__1__2, DELTA, {2, 0, 1}));

RAM* scc5653 = new RAM(false, 249);
scc5653->add_relation(rel__let__3__2, true);
scc5653->add_relation(rel__let__3__1__2__3, true);
scc5653->add_rule(new parallel_acopy(rel__let__3__2, rel__let__3__1__2__3, DELTA, {1, 3, 0, 2}));

RAM* scc5654 = new RAM(false, 253);
scc5654->add_relation(rel___dollorbir__sub6__3__1__2__3, true);
scc5654->add_relation(rel___dollorbir__sub6__3__1, true);
scc5654->add_rule(new parallel_acopy(rel___dollorbir__sub6__3__1, rel___dollorbir__sub6__3__1__2__3, DELTA, {0, 3, 1, 2}));

RAM* scc5655 = new RAM(false, 115);
scc5655->add_relation(rel__num__2__1__2, true);
scc5655->add_rule(new fact(rel__num__2__1__2, {n2d(524350), n2d(27)}));

RAM* scc5656 = new RAM(false, 119);
scc5656->add_relation(rel__let__list__3__1__2__3, true);
scc5656->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524415), n2d(524416)}));

RAM* scc5657 = new RAM(false, 123);
scc5657->add_relation(rel__prim__call__3__1__2__3, true);
scc5657->add_relation(rel__prim__call__3__3, true);
scc5657->add_rule(new parallel_acopy(rel__prim__call__3__3, rel__prim__call__3__1__2__3, DELTA, {2, 3, 0, 1}));

RAM* scc5658 = new RAM(false, 127);
scc5658->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5658->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524317), n2d(0), n2d(524318)}));

RAM* scc5659 = new RAM(false, 66);
scc5659->add_relation(rel__call__3__2, true);
scc5659->add_relation(rel__call__3__1__2__3, true);
scc5659->add_rule(new parallel_acopy(rel__call__3__2, rel__call__3__1__2__3, DELTA, {1, 3, 0, 2}));

RAM* scc5660 = new RAM(false, 70);
scc5660->add_relation(rel__setk__2__1__2, true);
scc5660->add_relation(rel__setk__2__0, true);
scc5660->add_rule(new parallel_acopy(rel__setk__2__0, rel__setk__2__1__2, DELTA, {2, 0, 1}));

RAM* scc5661 = new RAM(false, 181);
scc5661->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5661->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524311), n2d(0), n2d(524312)}));

RAM* scc5662 = new RAM(false, 78);
scc5662->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5662->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524345), n2d(0), n2d(524346)}));

RAM* scc5663 = new RAM(false, 192);
scc5663->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5663->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524331), n2d(0), n2d(524332)}));

RAM* scc5664 = new RAM(false, 196);
scc5664->add_relation(rel___dollorinter__head17__3__1__2__3, true);
scc5664->add_relation(rel___dollorinter__head17__3__, true);
scc5664->add_rule(new parallel_acopy(rel___dollorinter__head17__3__, rel___dollorinter__head17__3__1__2__3, DELTA, {3, 0, 1, 2}));

RAM* scc5665 = new RAM(false, 200);
scc5665->add_relation(rel___dollorbir__sub3__4__1__2__3__4, true);
scc5665->add_relation(rel___dollorbir__sub3__4__3__1, true);
scc5665->add_rule(new parallel_acopy(rel___dollorbir__sub3__4__3__1, rel___dollorbir__sub3__4__1__2__3__4, DELTA, {2, 0, 4, 1, 3}));

RAM* scc5666 = new RAM(false, 204);
scc5666->add_relation(rel__top__exp__1__, true);
scc5666->add_relation(rel__top__exp__1__1, true);
scc5666->add_rule(new parallel_acopy(rel__top__exp__1__, rel__top__exp__1__1, DELTA, {1, 0}));

RAM* scc5667 = new RAM(false, 19);
scc5667->add_relation(rel__var__2__1__2, true);
scc5667->add_rule(new fact(rel__var__2__1__2, {n2d(524369), n2d(524367)}));

RAM* scc5668 = new RAM(false, 23);
scc5668->add_relation(rel__var__2__1__2, true);
scc5668->add_rule(new fact(rel__var__2__1__2, {n2d(524395), n2d(524367)}));

RAM* scc5669 = new RAM(false, 27);
scc5669->add_relation(rel__call__3__1__2__3, true);
scc5669->add_relation(rel__call__3__3, true);
scc5669->add_rule(new parallel_acopy(rel__call__3__3, rel__call__3__1__2__3, DELTA, {2, 3, 0, 1}));

RAM* scc5670 = new RAM(false, 74);
scc5670->add_relation(rel__num__2__1, true);
scc5670->add_relation(rel__num__2__1__2, true);
scc5670->add_rule(new parallel_acopy(rel__num__2__1, rel__num__2__1__2, DELTA, {0, 2, 1}));

RAM* scc5671 = new RAM(false, 145);
scc5671->add_relation(rel__call__3__1__2__3, true);
scc5671->add_rule(new fact(rel__call__3__1__2__3, {n2d(524460), n2d(524371), n2d(524301)}));

RAM* scc5672 = new RAM(false, 149);
scc5672->add_relation(rel__var__2__1__2, true);
scc5672->add_rule(new fact(rel__var__2__1__2, {n2d(524398), n2d(524367)}));

RAM* scc5673 = new RAM(false, 153);
scc5673->add_relation(rel___dollornil__0__, true);
scc5673->add_rule(new parallel_copy(rel___dollornil__0__, rel__top__exp__1__1, FULL, {}));

RAM* scc5674 = new RAM(false, 157);
scc5674->add_relation(rel___dollorhead__stratified3__2__1__2, true);
scc5674->add_relation(rel___dollorhead__stratified3__2__, true);
scc5674->add_rule(new parallel_acopy(rel___dollorhead__stratified3__2__, rel___dollorhead__stratified3__2__1__2, DELTA, {2, 0, 1}));

RAM* scc5675 = new RAM(false, 228);
scc5675->add_relation(rel__null__0__, true);
scc5675->add_rule(new parallel_copy(rel__null__0__, rel__top__exp__1__1, FULL, {}));

RAM* scc5676 = new RAM(false, 232);
scc5676->add_relation(rel__lambda__3__1__2__3, true);
scc5676->add_relation(rel__lambda__3__3, true);
scc5676->add_rule(new parallel_acopy(rel__lambda__3__3, rel__lambda__3__1__2__3, DELTA, {2, 3, 0, 1}));

RAM* scc5677 = new RAM(false, 31);
scc5677->add_relation(rel__let__3__1, true);
scc5677->add_relation(rel__let__3__1__2__3, true);
scc5677->add_rule(new parallel_acopy(rel__let__3__1, rel__let__3__1__2__3, DELTA, {0, 3, 1, 2}));

RAM* scc5678 = new RAM(false, 236);
scc5678->add_relation(rel__var__2__1__2, true);
scc5678->add_rule(new fact(rel__var__2__1__2, {n2d(524383), n2d(524367)}));

RAM* scc5679 = new RAM(false, 98);
scc5679->add_relation(rel___dollorbir__sub4__3__1__2__3, true);
scc5679->add_rule(new parallel_copy(rel___dollorbir__sub4__3__1__2__3, rel__call__3__1__2__3, FULL, {0, 1, 2}));

RAM* scc5680 = new RAM(false, 102);
scc5680->add_relation(rel__callcc__2__2, true);
scc5680->add_relation(rel__callcc__2__1__2, true);
scc5680->add_rule(new parallel_acopy(rel__callcc__2__2, rel__callcc__2__1__2, DELTA, {1, 2, 0}));

RAM* scc5681 = new RAM(true, 106);
scc5681->add_relation(rel___dollorinter__body52__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body55__5__1__2__3__4__5, true);
scc5681->add_relation(rel___dollorhead__stratified44__4__3__2__1, true);
scc5681->add_relation(rel___dollorinter__body38__6__2, true);
scc5681->add_relation(rel___dollorhead__stratified42__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorhead__stratified16__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body17__2__2, true);
scc5681->add_relation(rel___dollorinter__body44__2__2, true);
scc5681->add_relation(rel___dollorinter__body45__3__3, true);
scc5681->add_relation(rel___dollorinter__body31__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body7__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__head22__4__4__3__1, true);
scc5681->add_relation(rel___dollorhead__stratified20__4__1__2__3__4, true);
scc5681->add_relation(rel__E__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__body6__6__1__2__3__4__5__6, true);
scc5681->add_relation(rel__callcck__2__2__1, true);
scc5681->add_relation(rel___dollorhead__stratified36__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body49__5__1, true);
scc5681->add_relation(rel___dollorinter__head21__3__1__2__3, true);
scc5681->add_relation(rel__prim1__4__0, true);
scc5681->add_relation(rel___dollorinter__body49__5__1__2__3__4__5, true);
scc5681->add_relation(rel___dollorhead__stratified11__2__1__2, true);
scc5681->add_relation(rel___dollorhead__stratified37__4__4__2, true);
scc5681->add_relation(rel__vaddr__2__2, true);
scc5681->add_relation(rel___dollorinter__body11__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body47__4__3__1, true);
scc5681->add_relation(rel___dollorinter__body38__6__1__2__3__4__5__6, true);
scc5681->add_relation(rel___dollorinter__body40__4__1__2__3__4, true);
scc5681->add_relation(rel__kont__1__0, true);
scc5681->add_relation(rel___dollorhead__stratified18__4__4__2, true);
scc5681->add_relation(rel___dollorhead__stratified26__3__3__1, true);
scc5681->add_relation(rel__ifk__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body53__2__2, true);
scc5681->add_relation(rel__store__2__1, true);
scc5681->add_relation(rel___dollorhead__stratified25__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body90__3__2__1, true);
scc5681->add_relation(rel__peek__ctx__3__2__1, true);
scc5681->add_relation(rel___dollorlst__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body15__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body28__4__1, true);
scc5681->add_relation(rel___dollorinter__body39__4__2, true);
scc5681->add_relation(rel___dollorinter__body12__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body30__3__, true);
scc5681->add_relation(rel___dollorinter__body24__6__1__2__3__4__5__6, true);
scc5681->add_relation(rel___dollorinter__body32__4__2, true);
scc5681->add_relation(rel__store__2__2, true);
scc5681->add_relation(rel___dollorinter__body42__5__1, true);
scc5681->add_relation(rel___dollorinter__body87__3__2, true);
scc5681->add_relation(rel___dollorinter__head17__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__body14__5__1__2__3__4__5, true);
scc5681->add_relation(rel___dollorinter__head11__3__1__2__3, true);
scc5681->add_relation(rel__prim2__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__body34__3__1__2__3, true);
scc5681->add_relation(rel__primval__3__3__2__1, true);
scc5681->add_relation(rel___dollorhead__stratified15__2__1__2, true);
scc5681->add_relation(rel___dollorinter__head15__5__1__2__3__4__5, true);
scc5681->add_relation(rel___dollorinter__body54__4__4, true);
scc5681->add_relation(rel__number__1__0, true);
scc5681->add_relation(rel___dollorinter__head30__5__3__2, true);
scc5681->add_relation(rel___dollorhead__stratified43__3__3__2, true);
scc5681->add_relation(rel___dollorinter__head19__5__1__2__3__4__5, true);
scc5681->add_relation(rel___dollorinter__body66__2__1, true);
scc5681->add_relation(rel___dollorinter__body82__3__2, true);
scc5681->add_relation(rel___dollorhead__stratified19__3__1__2__3, true);
scc5681->add_relation(rel__prim2__3__3__2__1, true);
scc5681->add_relation(rel__ifk__4__0, true);
scc5681->add_relation(rel___dollorinter__head__5__1__2, true);
scc5681->add_relation(rel___dollorinter__body23__3__3__2, true);
scc5681->add_relation(rel__argk__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorhead__stratified38__4__4__3__1, true);
scc5681->add_relation(rel___dollorinter__body82__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__body26__2__1__2, true);
scc5681->add_relation(rel___dollorinter__head18__5__5__3__1, true);
scc5681->add_relation(rel___dollorinter__body23__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__body41__2__1, true);
scc5681->add_relation(rel___dollorinter__head5__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body21__5__1__2__3__4__5, true);
scc5681->add_relation(rel___dollorinter__head20__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__body75__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__head3__4__4__2__1, true);
scc5681->add_relation(rel___dollorinter__body46__4__4, true);
scc5681->add_relation(rel___dollorinter__body69__2__1, true);
scc5681->add_relation(rel__flow__ae__2__1__2, true);
scc5681->add_relation(rel___dollorinter__head15__5__1__2, true);
scc5681->add_relation(rel___dollorinter__body86__4__4__1, true);
scc5681->add_relation(rel___dollorinter__body73__3__1__2__3, true);
scc5681->add_relation(rel___dollorhead__stratified21__3__1__2__3, true);
scc5681->add_relation(rel__here__5__1__2__3__4__5, true);
scc5681->add_relation(rel__kaddr__2__2__1, true);
scc5681->add_relation(rel___dollorinter__body65__3__, true);
scc5681->add_relation(rel___dollorinter__body69__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body89__3__3__2, true);
scc5681->add_relation(rel___dollorinter__body63__2__1, true);
scc5681->add_relation(rel___dollorinter__body77__3__3__2, true);
scc5681->add_relation(rel__store__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body67__3__1__2__3, true);
scc5681->add_relation(rel__argk__4__1, true);
scc5681->add_relation(rel___dollorinter__head13__6__5__2__1__3, true);
scc5681->add_relation(rel___dollorinter__body36__3__1__2__3, true);
scc5681->add_relation(rel__kaddr__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body89__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__body29__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body68__2__1, true);
scc5681->add_relation(rel__E__3__1, true);
scc5681->add_relation(rel___dollorinter__body65__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__body53__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body5__7__1__2__3__4__5__6__7, true);
scc5681->add_relation(rel___dollorinter__body39__4__1__2__3__4, true);
scc5681->add_relation(rel__store__2__2__1, true);
scc5681->add_relation(rel___dollorinter__head25__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body9__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__body8__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorhead__stratified28__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body46__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body19__3__3__1, true);
scc5681->add_relation(rel___dollorinter__body26__2__1, true);
scc5681->add_relation(rel___dollorinter__head26__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__head28__3__1__2__3, true);
scc5681->add_relation(rel___dollorhead__stratified43__3__1__2__3, true);
scc5681->add_relation(rel___dollorhead__stratified18__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body58__2__2, true);
scc5681->add_relation(rel__copy__ctx__3__3__2__1, true);
scc5681->add_relation(rel___dollorinter__head1__7__6__2, true);
scc5681->add_relation(rel___dollorinter__body64__1__1, true);
scc5681->add_relation(rel___dollorinter__body58__2__1__2, true);
scc5681->add_relation(rel___dollorhead__stratified44__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorlst__2__2__1, true);
scc5681->add_relation(rel___dollorinter__body45__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__body52__2__2, true);
scc5681->add_relation(rel___dollorhead__stratified__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body30__3__1__2__3, true);
scc5681->add_relation(rel___dollorhead__stratified19__3__3__2, true);
scc5681->add_relation(rel___dollorinter__body48__3__3__1, true);
scc5681->add_relation(rel___dollorinter__body25__8__2, true);
scc5681->add_relation(rel___dollorinter__head9__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__head16__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__head18__5__1__2__3__4__5, true);
scc5681->add_relation(rel___dollorinter__body47__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body74__4__1__2__3__4, true);
scc5681->add_relation(rel__peek__ctx__3__2__3__1, true);
scc5681->add_relation(rel___dollorinter__head25__4__4__3__1, true);
scc5681->add_relation(rel___dollorinter__body72__1__1, true);
scc5681->add_relation(rel___dollorinter__body62__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body2__4__3, true);
scc5681->add_relation(rel__letk__4__4__3__2__1, true);
scc5681->add_relation(rel___dollorinter__body1__3__3__1, true);
scc5681->add_relation(rel___dollorhead__stratified3__2__1__2, true);
scc5681->add_relation(rel___dollorinter__head7__6__1__2__3__4__5__6, true);
scc5681->add_relation(rel___dollorhead__stratified2__7__1__2__3__4__5__6__7, true);
scc5681->add_relation(rel___dollorinter__head31__3__2__1, true);
scc5681->add_relation(rel___dollorinter__body34__3__, true);
scc5681->add_relation(rel__flow__ee__2__1__2, true);
scc5681->add_relation(rel___dollorinter__head10__3__3__2, true);
scc5681->add_relation(rel___dollorinter__body86__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__head3__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body5__7__3__6, true);
scc5681->add_relation(rel___dollorinter__head27__6__6__3__2__1, true);
scc5681->add_relation(rel___dollorinter__body7__4__3, true);
scc5681->add_relation(rel__primval__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__body19__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__head19__5__1__2, true);
scc5681->add_relation(rel___dollorinter__head1__7__1__2__3__4__5__6__7, true);
scc5681->add_relation(rel___dollorinter__body8__4__3__2, true);
scc5681->add_relation(rel___dollorinter__body42__5__1__2__3__4__5, true);
scc5681->add_relation(rel___dollorinter__body75__3__2, true);
scc5681->add_relation(rel___dollorlst__2__0, true);
scc5681->add_relation(rel___dollorinter__head12__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body54__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body67__3__3, true);
scc5681->add_relation(rel___dollorinter__body3__4__3__4__2, true);
scc5681->add_relation(rel___dollorinter__body20__7__1__2__3__4__5__6__7, true);
scc5681->add_relation(rel___dollorhead__stratified31__2__1__2, true);
scc5681->add_relation(rel__number__1__1, true);
scc5681->add_relation(rel___dollorinter__head7__6__6__4__2__1, true);
scc5681->add_relation(rel___dollorhead__stratified21__3__3__1, true);
scc5681->add_relation(rel___dollorinter__body__4__1__3, true);
scc5681->add_relation(rel___dollorinter__head28__3__2, true);
scc5681->add_relation(rel___dollorinter__body22__4__3__1, true);
scc5681->add_relation(rel___dollorinter__body55__5__1, true);
scc5681->add_relation(rel__peek__ctx__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__body17__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body16__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__body37__2__1, true);
scc5681->add_relation(rel___dollorhead__stratified9__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body88__2__2, true);
scc5681->add_relation(rel___dollorinter__body43__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__body90__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__body24__6__1__4, true);
scc5681->add_relation(rel__flow__ae__2__2__1, true);
scc5681->add_relation(rel___dollorinter__head8__8__2__7__6__1__4, true);
scc5681->add_relation(rel___dollorinter__body21__5__5__1__2, true);
scc5681->add_relation(rel___dollorinter__body35__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body18__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body63__2__1__2, true);
scc5681->add_relation(rel___dollorhead__stratified38__4__1__2__3__4, true);
scc5681->add_relation(rel__prim1__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body2__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body70__2__2, true);
scc5681->add_relation(rel__closure__2__2__1, true);
scc5681->add_relation(rel__A__2__2, true);
scc5681->add_relation(rel__A__2__1__2, true);
scc5681->add_relation(rel___dollorhead__stratified39__4__1__2__3__4, true);
scc5681->add_relation(rel__flow__ee__2__2__1, true);
scc5681->add_relation(rel___dollorinter__body57__3__2, true);
scc5681->add_relation(rel__E__3__2__1, true);
scc5681->add_relation(rel__vaddr__2__2__1, true);
scc5681->add_relation(rel__ifk__4__3__2__4__1, true);
scc5681->add_relation(rel___dollorinter__body40__4__2, true);
scc5681->add_relation(rel__prim2__3__0, true);
scc5681->add_relation(rel___dollorinter__head32__4__1__2, true);
scc5681->add_relation(rel___dollorinter__body__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorhead__stratified4__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__head6__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body48__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__body22__4__1__2__3__4, true);
scc5681->add_relation(rel__kont__1__1, true);
scc5681->add_relation(rel___dollorinter__body33__1__1, true);
scc5681->add_relation(rel___dollorinter__head12__4__4__3__1, true);
scc5681->add_relation(rel___dollorinter__body71__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body12__2__1, true);
scc5681->add_relation(rel___dollorhead__stratified23__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body57__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__body13__3__2, true);
scc5681->add_relation(rel___dollorinter__body44__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body41__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body14__5__5, true);
scc5681->add_relation(rel__closure__2__0, true);
scc5681->add_relation(rel__A__2__1, true);
scc5681->add_relation(rel___dollorinter__head__5__1__2__3__4__5, true);
scc5681->add_relation(rel__letk__4__0, true);
scc5681->add_relation(rel___dollorinter__body31__2__2, true);
scc5681->add_relation(rel___dollorinter__head22__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorhead__stratified42__4__4__2, true);
scc5681->add_relation(rel__fn__4__4__3__2__1, true);
scc5681->add_relation(rel__callcck__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body84__3__1__2__3, true);
scc5681->add_relation(rel___dollorhead__stratified36__4__4__2, true);
scc5681->add_relation(rel___dollorinter__body77__3__1__2__3, true);
scc5681->add_relation(rel___dollorhead__stratified45__2__1__2, true);
scc5681->add_relation(rel__vaddr__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body18__4__3__1, true);
scc5681->add_relation(rel___dollorinter__head30__5__1__2__3__4__5, true);
scc5681->add_relation(rel___dollorinter__body70__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body73__3__, true);
scc5681->add_relation(rel___dollorinter__body28__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body66__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body76__4__3__2, true);
scc5681->add_relation(rel___dollorinter__body84__3__1, true);
scc5681->add_relation(rel___dollorinter__head2__6__6__5__2__1, true);
scc5681->add_relation(rel___dollorinter__body16__3__1, true);
scc5681->add_relation(rel___dollorinter__body83__2__1, true);
scc5681->add_relation(rel___dollorinter__body85__4__1, true);
scc5681->add_relation(rel___dollorinter__body74__4__4__3, true);
scc5681->add_relation(rel___dollorinter__body79__4__3__4, true);
scc5681->add_relation(rel___dollorinter__body13__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__head16__3__2, true);
scc5681->add_relation(rel___dollorinter__head9__4__3__2, true);
scc5681->add_relation(rel___dollorinter__body32__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body27__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__body85__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__head23__5__1__2__3__4__5, true);
scc5681->add_relation(rel___dollorinter__head2__6__1__2__3__4__5__6, true);
scc5681->add_relation(rel___dollorinter__head13__6__1__2__3__4__5__6, true);
scc5681->add_relation(rel___dollorinter__body3__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__head27__6__1__2__3__4__5__6, true);
scc5681->add_relation(rel___dollorinter__body78__3__3__1, true);
scc5681->add_relation(rel___dollorhead__stratified20__4__4__1, true);
scc5681->add_relation(rel__fn__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body37__2__1__2, true);
scc5681->add_relation(rel__boolv__1__0, true);
scc5681->add_relation(rel___dollorinter__head4__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__body71__2__2, true);
scc5681->add_relation(rel___dollorinter__body25__8__1__2__3__4__5__6__7__8, true);
scc5681->add_relation(rel___dollorinter__body43__3__1, true);
scc5681->add_relation(rel___dollorhead__stratified37__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__head14__5__3__1, true);
scc5681->add_relation(rel__copy__ctx__3__1, true);
scc5681->add_relation(rel___dollorinter__body88__2__1__2, true);
scc5681->add_relation(rel___dollorhead__stratified26__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__body62__2__2, true);
scc5681->add_relation(rel___dollorinter__head14__5__1__2__3__4__5, true);
scc5681->add_relation(rel___dollorhead__stratified33__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body20__7__2__1__4, true);
scc5681->add_relation(rel___dollorinter__head24__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body9__3__3__2, true);
scc5681->add_relation(rel___dollorinter__head32__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body83__2__1__2, true);
scc5681->add_relation(rel__num__1__1, true);
scc5681->add_relation(rel__prim1__4__3__2__1__4, true);
scc5681->add_relation(rel___dollorhead__stratified12__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__head20__3__1, true);
scc5681->add_relation(rel___dollorinter__body10__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorlst__2__2, true);
scc5681->add_relation(rel___dollorinter__body4__7__1__2__3__4__5__6__7, true);
scc5681->add_relation(rel___dollorinter__body6__6__6__3, true);
scc5681->add_relation(rel___dollorinter__body36__3__2__3, true);
scc5681->add_relation(rel___dollorinter__body76__4__1__2__3__4, true);
scc5681->add_relation(rel__boolv__1__1, true);
scc5681->add_relation(rel__E__3__3__2__1, true);
scc5681->add_relation(rel___dollorinter__head8__8__1__2__3__4__5__6__7__8, true);
scc5681->add_relation(rel___dollorinter__body78__3__1__2__3, true);
scc5681->add_relation(rel__letk__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body10__4__3, true);
scc5681->add_relation(rel___dollorinter__body15__2__1, true);
scc5681->add_relation(rel___dollorinter__body79__4__1__2__3__4, true);
scc5681->add_relation(rel___dollorinter__body1__3__1__2__3, true);
scc5681->add_relation(rel__fn__4__1, true);
scc5681->add_relation(rel___dollorinter__body27__3__1, true);
scc5681->add_relation(rel___dollorinter__body4__7__6__2__7, true);
scc5681->add_relation(rel___dollorinter__body35__4__4__2, true);
scc5681->add_relation(rel__here__5__5__4__3__2__1, true);
scc5681->add_relation(rel___dollorinter__body68__2__1__2, true);
scc5681->add_relation(rel___dollorinter__body87__3__1__2__3, true);
scc5681->add_relation(rel__copy__ctx__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__head31__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__head29__3__1__2__3, true);
scc5681->add_relation(rel___dollorinter__body11__2__1, true);
scc5681->add_relation(rel__E__3__2, true);
scc5681->add_relation(rel___dollorhead__stratified2__7__1__2__4__7, true);
scc5681->add_relation(rel___dollorinter__body29__2__1, true);
scc5681->add_relation(rel___dollorinter__head10__3__1__2__3, true);
scc5681->add_relation(rel___dollorhead__stratified25__4__4__1, true);
scc5681->add_relation(rel__setk__1__1, true);
scc5681->add_relation(rel__closure__2__1__2, true);
scc5681->add_rule(new parallel_join(rel___dollorinter__body78__3__1__2__3, rel__peek__ctx__3__2__3__1, FULL, rel___dollorhead__stratified38__4__4__3__1, DELTA, {0, 5, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body7__4__1__2__3__4, rel__E__3__2__1, DELTA, rel__peek__ctx__3__2__1, FULL, {3, 5, 1, 0}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body4__7__6__2__7, rel___dollorinter__body4__7__1__2__3__4__5__6__7, DELTA, {5, 1, 6, 7, 0, 2, 3, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head19__5__1__2__3__4__5, rel___dollorinter__body52__2__2, DELTA, rel___dollorinter__body55__5__1, FULL, {2, 4, 5, 6, 7}));
scc5681->add_rule(new parallel_copy(rel__A__2__1__2, rel___dollorinter__head21__3__1__2__3, DELTA, {0, 1}));
scc5681->add_rule(new parallel_copy(rel__A__2__1__2, rel___dollorhead__stratified12__4__1__2__3__4, DELTA, {2, 1}));
scc5681->add_rule(new parallel_acopy(rel__primval__3__3__2__1, rel__primval__3__1__2__3, DELTA, {2, 1, 0, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body26__2__1__2, rel__store__2__1, DELTA, rel__A__2__2, DELTA, {2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body89__3__1__2__3, rel__flow__ee__2__2__1, DELTA, rel___dollorinter__head32__4__1__2, FULL, {0, 4, 5}));
scc5681->add_rule(new parallel_copy(rel__flow__ae__2__1__2, rel___dollorhead__stratified31__2__1__2, DELTA, {1, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body69__2__1__2, rel__store__2__1, DELTA, rel__A__2__2, FULL, {2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body42__5__1__2__3__4__5, rel___dollorinter__body41__2__1, DELTA, rel___dollorinter__body40__4__2, DELTA, {4, 0, 5, 6, 2}));
scc5681->add_rule(new parallel_copy(rel__store__2__1__2, rel___dollorhead__stratified3__2__1__2, DELTA, {0, 1}));
scc5681->add_rule(new parallel_join(rel__peek__ctx__3__1__2__3, rel___dollorlst__2__2__1, DELTA, rel___dollorhead__stratified21__3__3__1, FULL, {1, 4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body28__4__1__2__3__4, rel__number__1__1, DELTA, rel___dollorinter__body27__3__1, FULL, {0, 1, 3, 4}));
scc5681->add_rule(new parallel_acopy(rel__A__2__2, rel__A__2__1__2, DELTA, {1, 2, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body71__2__1__2, rel__store__2__1, DELTA, rel__A__2__2, DELTA, {2, 4}));
scc5681->add_rule(new parallel_copy(rel__store__2__1__2, rel___dollorhead__stratified9__4__1__2__3__4, DELTA, {2, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head31__3__1__2__3, rel__store__2__1, DELTA, rel___dollorinter__body87__3__2, DELTA, {4, 5, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body20__7__1__2__3__4__5__6__7, rel__here__5__5__4__3__2__1, DELTA, rel___dollorinter__head8__8__2__7__6__1__4, DELTA, {0, 7, 4, 8, 2, 1, 9}));
scc5681->add_rule(new parallel_copy(rel__store__2__1__2, rel___dollorhead__stratified23__4__1__2__3__4, DELTA, {3, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head19__5__1__2, rel___dollorinter__head19__5__1__2__3__4__5, DELTA, {0, 1, 5, 2, 3, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head30__5__1__2__3__4__5, rel___dollorinter__body83__2__1, DELTA, rel___dollorinter__body85__4__1, DELTA, {0, 4, 5, 6, 2}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified2__7__1__2__3__4__5__6__7, rel__kaddr__2__2__1, DELTA, rel___dollorinter__body6__6__6__3, DELTA, {4, 5, 1, 6, 0, 2, 7}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body18__4__3__1, rel___dollorinter__body18__4__1__2__3__4, DELTA, {2, 0, 4, 1, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body83__2__1__2, rel__lambda__3__1, FULL, rel__E__3__1, DELTA, {5, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body24__6__1__2__3__4__5__6, rel__closure__2__0, DELTA, rel__fn__4__1, FULL, {4, 3, 5, 1, 2, 6}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body42__5__1, rel___dollorinter__body42__5__1__2__3__4__5, DELTA, {0, 5, 1, 2, 3, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head23__5__1__2__3__4__5, rel___dollorinter__body66__2__1, DELTA, rel__letk__4__0, DELTA, {4, 3, 2, 5, 6}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body10__4__1__2__3__4, rel__E__3__2__1, DELTA, rel__peek__ctx__3__2__1, DELTA, {3, 5, 1, 0}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified16__4__1__2__3__4, rel__kaddr__2__2__1, FULL, rel___dollorinter__body36__3__2__3, DELTA, {0, 1, 2, 4}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body1__3__3__1, rel___dollorinter__body1__3__1__2__3, DELTA, {2, 0, 3, 1}));
scc5681->add_rule(new parallel_copy(rel__flow__ee__2__1__2, rel___dollorinter__head7__6__1__2__3__4__5__6, DELTA, {4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head8__8__1__2__3__4__5__6__7__8, rel___dollorinter__body26__2__1, DELTA, rel___dollorinter__body25__8__2, DELTA, {4, 5, 6, 7, 8, 9, 2, 10}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body29__2__1__2, rel__A__2__2, DELTA, rel__store__2__1, DELTA, {4, 2}));
scc5681->add_rule(new parallel_copy(rel__store__2__1__2, rel___dollorhead__stratified11__2__1__2, DELTA, {1, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body5__7__1__2__3__4__5__6__7, rel__copy__ctx__3__3__2__1, DELTA, rel___dollorinter__body4__7__6__2__7, DELTA, {5, 1, 6, 7, 8, 0, 2}));
scc5681->add_rule(new parallel_acopy(rel__kaddr__2__2__1, rel__kaddr__2__1__2, DELTA, {1, 0, 2}));
scc5681->add_rule(new parallel_acopy(rel__store__2__1, rel__store__2__1__2, DELTA, {0, 2, 1}));
scc5681->add_rule(new parallel_join(rel__peek__ctx__3__1__2__3, rel___dollorlst__2__2__1, DELTA, rel___dollorhead__stratified19__3__3__2, DELTA, {1, 4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified25__4__1__2__3__4, rel___dollorlst__2__2__1, FULL, rel___dollorinter__head19__5__1__2, DELTA, {4, 5, 6, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body38__6__1__2__3__4__5__6, rel___dollorbir__sub6__3__1, FULL, rel__argk__4__1, DELTA, {2, 4, 6, 5, 7, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head31__3__1__2__3, rel__store__2__1, FULL, rel___dollorinter__body87__3__2, DELTA, {4, 5, 2}));
scc5681->add_rule(new parallel_copy(rel__E__3__1__2__3, rel___dollorhead__stratified23__4__1__2__3__4, DELTA, {0, 1, 3}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified3__2__1__2, rel__E__3__3__2__1, DELTA, rel___dollorinter__body3__4__3__4__2, FULL, {0, 5}));
scc5681->add_rule(new parallel_copy_generate(rel___dollorinter__head5__4__1__2__3__4, rel___dollorinter__body14__5__5, DELTA, [](const u64* data, u64* const output) -> int{
      auto args_for_old_bi = std::array<u64, 2> {data[0], data[1]};
      using TState = std::tuple<const u64*,u64*>;
      TState state = {data, output};
      auto callback = []( TState state) -> TState{
        auto [data, output] = state;
        bool compatible = true;
        if (! compatible) return state;

        auto head_tuple = output;
        head_tuple[0] = data[2];
head_tuple[1] = data[3];
head_tuple[2] = data[4];
head_tuple[3] = data[5];
        return {data, output + 4};
      };
      auto [_,new_ptr] = builtin_eq<TState>(args_for_old_bi.data(), state, callback);
      auto tuples_count = (new_ptr - output) / 4;
      return tuples_count;
    }));
scc5681->add_rule(new parallel_join(rel___dollorinter__head8__8__1__2__3__4__5__6__7__8, rel___dollorinter__body26__2__1, FULL, rel___dollorinter__body25__8__2, DELTA, {4, 5, 6, 7, 8, 9, 2, 10}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified12__4__1__2__3__4, rel__num__1__1, DELTA, rel___dollorinter__body28__4__1, DELTA, {4, 5, 3, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body9__3__1__2__3, rel__flow__ee__2__2__1, DELTA, rel___dollorinter__body8__4__3__2, FULL, {4, 0, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body21__5__1__2__3__4__5, rel__copy__ctx__3__3__2__1, DELTA, rel___dollorinter__body20__7__2__1__4, FULL, {1, 5, 6, 7, 8}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body73__3__1__2__3, rel___dollorinter__body72__1__1, DELTA, rel__ifk__4__0, FULL, {2, 4, 5}));
scc5681->add_rule(new parallel_copy(rel___dollorlst__2__1__2, rel___dollorinter__head15__5__1__2__3__4__5, DELTA, {1, 0}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body2__4__3, rel___dollorinter__body2__4__1__2__3__4, DELTA, {2, 4, 0, 1, 3}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified12__4__1__2__3__4, rel__num__1__1, DELTA, rel___dollorinter__body28__4__1, FULL, {4, 5, 3, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body35__4__1__2__3__4, rel__fn__4__4__3__2__1, DELTA, rel___dollorinter__head13__6__5__2__1__3, DELTA, {4, 3, 6, 7}));
scc5681->add_rule(new parallel_copy(rel__kaddr__2__1__2, rel___dollorinter__head1__7__1__2__3__4__5__6__7, DELTA, {2, 6}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body__4__1__3, rel___dollorinter__body__4__1__2__3__4, DELTA, {0, 2, 4, 1, 3}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified21__3__1__2__3, rel___dollorlst__2__2__1, DELTA, rel___dollorhead__stratified20__4__4__1, FULL, {4, 5, 2}));
scc5681->add_rule(new parallel_join(rel__peek__ctx__3__1__2__3, rel___dollorlst__2__2__1, DELTA, rel___dollorhead__stratified19__3__3__2, FULL, {1, 4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified44__4__1__2__3__4, rel__kaddr__2__2__1, FULL, rel___dollorinter__body90__3__2__1, DELTA, {1, 0, 2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified3__2__1__2, rel__E__3__3__2__1, FULL, rel___dollorinter__body3__4__3__4__2, DELTA, {0, 5}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified38__4__1__2__3__4, rel___dollorlst__2__2__1, DELTA, rel___dollorinter__body79__4__3__4, FULL, {1, 5, 2, 4}));
scc5681->add_rule(new parallel_acopy(rel___dollorhead__stratified19__3__3__2, rel___dollorhead__stratified19__3__1__2__3, DELTA, {2, 1, 3, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body__4__1__2__3__4, rel__callcck__2__2__1, DELTA, rel___dollorinter__head__5__1__2, DELTA, {4, 2, 5, 6}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body70__2__1__2, rel__E__3__3__2__1, FULL, rel___dollorinter__head25__4__4__3__1, DELTA, {2, 5}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body43__3__1, rel___dollorinter__body43__3__1__2__3, DELTA, {0, 3, 1, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body63__2__1, rel___dollorinter__body63__2__1__2, DELTA, {0, 2, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body4__7__1__2__3__4__5__6__7, rel__vaddr__2__1__2, DELTA, rel___dollorinter__head1__7__6__2, FULL, {4, 1, 5, 6, 2, 7, 8}));
scc5681->add_rule(new parallel_acopy(rel__boolv__1__0, rel__boolv__1__1, DELTA, {1, 0}));
scc5681->add_rule(new parallel_copy(rel__E__3__1__2__3, rel___dollorhead__stratified__4__1__2__3__4, DELTA, {0, 1, 3}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body53__2__2, rel___dollorinter__body53__2__1__2, DELTA, {1, 2, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head11__3__1__2__3, rel__closure__2__2__1, DELTA, rel___dollorinter__head10__3__3__2, FULL, {4, 2, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body31__2__1__2, rel__E__3__3__2__1, DELTA, rel___dollorinter__head12__4__4__3__1, FULL, {2, 5}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body55__5__1, rel___dollorinter__body55__5__1__2__3__4__5, DELTA, {0, 5, 1, 2, 3, 4}));
scc5681->add_rule(new parallel_copy(rel__prim2__3__1__2__3, rel___dollorinter__head18__5__1__2__3__4__5, DELTA, {0, 2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body12__2__1__2, rel__A__2__2, DELTA, rel__store__2__1, DELTA, {2, 4}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body49__5__1, rel___dollorinter__body49__5__1__2__3__4__5, DELTA, {0, 5, 1, 2, 3, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body30__3__1__2__3, rel__setk__2__0, FULL, rel___dollorinter__body29__2__1, DELTA, {1, 4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head32__4__1__2__3__4, rel__setb__3__1, FULL, rel__E__3__1, DELTA, {3, 0, 2, 5}));
scc5681->add_rule(new parallel_copy(rel__boolv__1__1, rel___dollorinter__head16__3__1__2__3, DELTA, {1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body22__4__1__2__3__4, rel__E__3__3__2__1, DELTA, rel___dollorinter__body21__5__5__1__2, DELTA, {1, 2, 5, 6}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head17__3__1__2__3, rel__boolv__1__1, FULL, rel___dollorinter__head16__3__2, DELTA, {1, 3, 4}));
scc5681->add_rule(new parallel_copy(rel__num__1__1, rel___dollorinter__head9__4__1__2__3__4, DELTA, {0}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified18__4__1__2__3__4, rel___dollorlst__2__1__2, DELTA, rel___dollorinter__head14__5__3__1, DELTA, {4, 5, 6, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head15__5__1__2, rel___dollorinter__head15__5__1__2__3__4__5, DELTA, {0, 1, 5, 2, 3, 4}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified44__4__1__2__3__4, rel__kaddr__2__2__1, DELTA, rel___dollorinter__body90__3__2__1, FULL, {1, 0, 2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body63__2__1__2, rel__A__2__2, DELTA, rel__store__2__1, DELTA, {2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head4__3__1__2__3, rel__primval__3__3__2__1, FULL, rel___dollorinter__head3__4__4__2__1, DELTA, {3, 5, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head6__4__1__2__3__4, rel___dollorinter__body17__2__2, DELTA, rel__ifk__4__0, FULL, {3, 2, 5, 6}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body8__4__1__2__3__4, rel__argk__4__1__2__3__4, DELTA, rel___dollorinter__head2__6__6__5__2__1, FULL, {4, 6, 7, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body66__2__1__2, rel__A__2__2, FULL, rel__store__2__1, DELTA, {4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head20__3__1__2__3, rel__kont__1__0, DELTA, rel___dollorinter__body58__2__2, FULL, {3, 1, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body26__2__1__2, rel__store__2__1, FULL, rel__A__2__2, DELTA, {2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body15__2__1__2, rel__A__2__2, DELTA, rel__store__2__1, FULL, {2, 4}));
scc5681->add_rule(new parallel_copy(rel__peek__ctx__3__1__2__3, rel___dollorhead__stratified38__4__1__2__3__4, DELTA, {0, 3, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head19__5__1__2__3__4__5, rel___dollorinter__body52__2__2, FULL, rel___dollorinter__body55__5__1, DELTA, {2, 4, 5, 6, 7}));
scc5681->add_rule(new parallel_acopy(rel___dollorhead__stratified36__4__4__2, rel___dollorhead__stratified36__4__1__2__3__4, DELTA, {3, 1, 4, 0, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body71__2__1__2, rel__store__2__1, DELTA, rel__A__2__2, FULL, {2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body3__4__1__2__3__4, rel__letk__4__4__3__2__1, DELTA, rel___dollorhead__stratified2__7__1__2__4__7, DELTA, {4, 6, 8, 7}));
scc5681->add_rule(new parallel_copy(rel__primval__3__1__2__3, rel___dollorinter__head3__4__1__2__3__4, DELTA, {0, 1, 3}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head7__6__6__4__2__1, rel___dollorinter__head7__6__1__2__3__4__5__6, DELTA, {5, 3, 1, 0, 6, 2, 4}));
scc5681->add_rule(new parallel_acopy(rel__prim2__3__0, rel__prim2__3__1__2__3, DELTA, {3, 0, 1, 2}));
scc5681->add_rule(new parallel_copy(rel__argk__4__1__2__3__4, rel___dollorinter__head2__6__1__2__3__4__5__6, DELTA, {5, 4, 1, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body27__3__1__2__3, rel__store__2__2__1, DELTA, rel___dollorinter__head9__4__3__2, DELTA, {4, 0, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body40__4__1__2__3__4, rel___dollorlst__2__0, DELTA, rel___dollorinter__body39__4__2, FULL, {2, 4, 5, 6}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body47__4__1__2__3__4, rel__prim2__3__3__2__1, FULL, rel___dollorinter__head18__5__5__3__1, DELTA, {5, 3, 1, 6}));
scc5681->add_rule(new parallel_copy(rel__kaddr__2__1__2, rel___dollorinter__head2__6__1__2__3__4__5__6, DELTA, {3, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body65__3__1__2__3, rel___dollorinter__body64__1__1, DELTA, rel__ifk__4__0, FULL, {2, 4, 5}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body22__4__3__1, rel___dollorinter__body22__4__1__2__3__4, DELTA, {2, 0, 4, 1, 3}));
scc5681->add_rule(new parallel_copy(rel__flow__ae__2__1__2, rel___dollorinter__head6__4__1__2__3__4, DELTA, {1, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body40__4__1__2__3__4, rel___dollorlst__2__0, DELTA, rel___dollorinter__body39__4__2, DELTA, {2, 4, 5, 6}));
scc5681->add_rule(new parallel_copy(rel__flow__ae__2__1__2, rel___dollorinter__head13__6__1__2__3__4__5__6, DELTA, {2, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body24__6__1__2__3__4__5__6, rel__closure__2__0, DELTA, rel__fn__4__1, DELTA, {4, 3, 5, 1, 2, 6}));
scc5681->add_rule(new parallel_copy(rel__ifk__4__1__2__3__4, rel___dollorinter__head7__6__1__2__3__4__5__6, DELTA, {0, 3, 5, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body6__6__1__2__3__4__5__6, rel__flow__ee__2__2__1, DELTA, rel___dollorinter__body5__7__3__6, FULL, {4, 5, 0, 6, 7, 8}));
scc5681->add_rule(new parallel_copy(rel__copy__ctx__3__1__2__3, rel___dollorinter__head1__7__1__2__3__4__5__6__7, DELTA, {6, 1, 4}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body9__3__3__2, rel___dollorinter__body9__3__1__2__3, DELTA, {2, 1, 3, 0}));
scc5681->add_rule(new parallel_copy(rel___dollorlst__2__1__2, rel___dollorinter__head19__5__1__2__3__4__5, DELTA, {1, 0}));
scc5681->add_rule(new parallel_copy(rel__A__2__1__2, rel___dollorinter__head4__3__1__2__3, DELTA, {0, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body57__3__1__2__3, rel__A__2__2, DELTA, rel__store__2__1, FULL, {0, 4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body1__3__1__2__3, rel__flow__ee__2__2__1, FULL, rel___dollorinter__body__4__1__3, DELTA, {0, 4, 5}));
scc5681->add_rule(new parallel_copy(rel__kaddr__2__1__2, rel___dollorinter__head__5__1__2__3__4__5, DELTA, {2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body46__4__1__2__3__4, rel___dollorlst__2__2, FULL, rel___dollorinter__body45__3__3, DELTA, {4, 5, 2, 1}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body35__4__4__2, rel___dollorinter__body35__4__1__2__3__4, DELTA, {3, 1, 4, 0, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body78__3__1__2__3, rel__peek__ctx__3__2__3__1, DELTA, rel___dollorhead__stratified38__4__4__3__1, DELTA, {0, 5, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body69__2__1, rel___dollorinter__body69__2__1__2, DELTA, {0, 2, 1}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body47__4__3__1, rel___dollorinter__body47__4__1__2__3__4, DELTA, {2, 0, 4, 1, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head3__4__1__2__3__4, rel___dollorinter__body11__2__1, DELTA, rel__prim2__3__0, FULL, {3, 4, 5, 2}));
scc5681->add_rule(new parallel_copy(rel__store__2__1__2, rel___dollorinter__head9__4__1__2__3__4, DELTA, {1, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body54__4__4, rel___dollorinter__body54__4__1__2__3__4, DELTA, {3, 4, 0, 1, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body6__6__6__3, rel___dollorinter__body6__6__1__2__3__4__5__6, DELTA, {5, 2, 6, 0, 1, 3, 4}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified12__4__1__2__3__4, rel__num__1__1, FULL, rel___dollorinter__body28__4__1, DELTA, {4, 5, 3, 1}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body27__3__1, rel___dollorinter__body27__3__1__2__3, DELTA, {0, 3, 1, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body22__4__1__2__3__4, rel__E__3__3__2__1, DELTA, rel___dollorinter__body21__5__5__1__2, FULL, {1, 2, 5, 6}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body58__2__2, rel___dollorinter__body58__2__1__2, DELTA, {1, 2, 0}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified11__2__1__2, rel__flow__ae__2__1__2, DELTA, rel___dollorinter__body23__3__3__2, DELTA, {0, 4}));
scc5681->add_rule(new parallel_copy(rel__boolv__1__1, rel___dollorinter__head12__4__1__2__3__4, DELTA, {1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body77__3__1__2__3, rel__flow__ee__2__1__2, DELTA, rel___dollorinter__body76__4__3__2, FULL, {4, 1, 5}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified11__2__1__2, rel__flow__ae__2__1__2, DELTA, rel___dollorinter__body23__3__3__2, FULL, {0, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body8__4__1__2__3__4, rel__argk__4__1__2__3__4, FULL, rel___dollorinter__head2__6__6__5__2__1, DELTA, {4, 6, 7, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head31__3__1__2__3, rel__store__2__1, DELTA, rel___dollorinter__body87__3__2, FULL, {4, 5, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorhead__stratified37__4__4__2, rel___dollorhead__stratified37__4__1__2__3__4, DELTA, {3, 1, 4, 0, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body90__3__2__1, rel___dollorinter__body90__3__1__2__3, DELTA, {1, 0, 3, 2}));
scc5681->add_rule(new parallel_join(rel__peek__ctx__3__1__2__3, rel___dollorlst__2__2__1, FULL, rel___dollorhead__stratified19__3__3__2, DELTA, {1, 4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified23__4__1__2__3__4, rel__kaddr__2__2__1, DELTA, rel___dollorinter__body48__3__3__1, FULL, {1, 0, 4, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head28__3__2, rel___dollorinter__head28__3__1__2__3, DELTA, {1, 3, 0, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body39__4__1__2__3__4, rel___dollorlst__2__0, DELTA, rel___dollorlst__2__2, FULL, {3, 2, 1, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head21__3__1__2__3, rel__kont__1__1, FULL, rel___dollorinter__head20__3__1, DELTA, {1, 3, 4}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body76__4__3__2, rel___dollorinter__body76__4__1__2__3__4, DELTA, {2, 1, 4, 0, 3}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body67__3__3, rel___dollorinter__body67__3__1__2__3, DELTA, {2, 3, 0, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head18__5__1__2__3__4__5, rel__A__2__2, DELTA, rel___dollorinter__body49__5__1, DELTA, {4, 5, 2, 6, 7}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified20__4__1__2__3__4, rel___dollorlst__2__2__1, FULL, rel___dollorinter__head15__5__1__2, DELTA, {4, 5, 6, 2}));
scc5681->add_rule(new parallel_copy(rel___dollorlst__2__1__2, rel___dollorhead__stratified36__4__1__2__3__4, DELTA, {1, 3}));
scc5681->add_rule(new parallel_acopy(rel__argk__4__1, rel__argk__4__1__2__3__4, DELTA, {0, 4, 1, 2, 3}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified31__2__1__2, rel__boolv__1__1, DELTA, rel___dollorinter__body70__2__2, DELTA, {3, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head15__5__1__2__3__4__5, rel___dollorinter__body44__2__2, FULL, rel___dollorinter__body46__4__4, DELTA, {4, 5, 6, 2, 0}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head32__4__1__2, rel___dollorinter__head32__4__1__2__3__4, DELTA, {0, 1, 4, 2, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body19__3__1__2__3, rel__flow__ee__2__1__2, DELTA, rel___dollorinter__body18__4__3__1, DELTA, {1, 4, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body74__4__1__2__3__4, rel__vaddr__2__2, DELTA, rel__E__3__2, DELTA, {5, 1, 4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head4__3__1__2__3, rel__primval__3__3__2__1, DELTA, rel___dollorinter__head3__4__4__2__1, FULL, {3, 5, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body55__5__1__2__3__4__5, rel___dollorlst__2__0, DELTA, rel___dollorinter__body54__4__4, DELTA, {2, 1, 4, 5, 6}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body74__4__4__3, rel___dollorinter__body74__4__1__2__3__4, DELTA, {3, 2, 4, 0, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body40__4__1__2__3__4, rel___dollorlst__2__0, FULL, rel___dollorinter__body39__4__2, DELTA, {2, 4, 5, 6}));
scc5681->add_rule(new parallel_acopy(rel___dollorhead__stratified2__7__1__2__4__7, rel___dollorhead__stratified2__7__1__2__3__4__5__6__7, DELTA, {0, 1, 3, 6, 7, 2, 4, 5}));
scc5681->add_rule(new parallel_copy(rel__kaddr__2__1__2, rel___dollorinter__head7__6__1__2__3__4__5__6, DELTA, {2, 5}));
scc5681->add_rule(new parallel_acopy(rel__closure__2__2__1, rel__closure__2__1__2, DELTA, {1, 0, 2}));
scc5681->add_rule(new parallel_copy(rel__E__3__1__2__3, rel___dollorinter__head25__4__1__2__3__4, DELTA, {0, 2, 3}));
scc5681->add_rule(new parallel_copy(rel__flow__ee__2__1__2, rel___dollorinter__head1__7__1__2__3__4__5__6__7, DELTA, {4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body46__4__1__2__3__4, rel___dollorlst__2__2, DELTA, rel___dollorinter__body45__3__3, DELTA, {4, 5, 2, 1}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body52__2__2, rel___dollorinter__body52__2__1__2, DELTA, {1, 2, 0}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body25__8__2, rel___dollorinter__body25__8__1__2__3__4__5__6__7__8, DELTA, {1, 8, 0, 2, 3, 4, 5, 6, 7}));
scc5681->add_rule(new parallel_acopy(rel__copy__ctx__3__1, rel__copy__ctx__3__1__2__3, DELTA, {0, 3, 1, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body19__3__1__2__3, rel__flow__ee__2__1__2, FULL, rel___dollorinter__body18__4__3__1, DELTA, {1, 4, 5}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body36__3__2__3, rel___dollorinter__body36__3__1__2__3, DELTA, {1, 2, 3, 0}));
scc5681->add_rule(new parallel_copy(rel__vaddr__2__1__2, rel___dollorinter__head8__8__1__2__3__4__5__6__7__8, DELTA, {5, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body14__5__1__2__3__4__5, rel___dollorinter__body13__3__2, DELTA, rel__ifk__4__0, FULL, {4, 2, 6, 7, 3}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified4__4__1__2__3__4, rel__kaddr__2__2__1, DELTA, rel___dollorinter__body9__3__3__2, FULL, {1, 0, 2, 4}));
scc5681->add_rule(new parallel_acopy(rel__prim2__3__3__2__1, rel__prim2__3__1__2__3, DELTA, {2, 1, 0, 3}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified39__4__1__2__3__4, rel__kaddr__2__1__2, FULL, rel___dollorinter__body78__3__3__1, DELTA, {0, 4, 1, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body27__3__1__2__3, rel__store__2__2__1, DELTA, rel___dollorinter__head9__4__3__2, FULL, {4, 0, 5}));
scc5681->add_rule(new parallel_copy(rel__number__1__1, rel___dollorinter__head28__3__1__2__3, DELTA, {1}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified19__3__1__2__3, rel___dollorlst__2__2__1, DELTA, rel___dollorhead__stratified18__4__4__2, FULL, {4, 5, 2}));
scc5681->add_rule(new parallel_join(rel__store__2__1__2, rel__vaddr__2__2__1, FULL, rel___dollorinter__head31__3__2__1, DELTA, {2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head27__6__1__2__3__4__5__6, rel__E__3__1, DELTA, rel___dollorbir__sub5__4__1, FULL, {3, 5, 7, 6, 0, 2}));
scc5681->add_rule(new parallel_copy(rel__flow__ee__2__1__2, rel___dollorinter__head32__4__1__2__3__4, DELTA, {1, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head__5__1__2__3__4__5, rel___dollorbir__sub2__2__1, FULL, rel___dollorinter__body2__4__3, DELTA, {4, 5, 2, 0, 6}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body82__3__1__2__3, rel___dollorlst__2__0, FULL, rel___dollorlst__2__2, DELTA, {3, 2, 4}));
scc5681->add_rule(new parallel_acopy(rel__callcck__2__2__1, rel__callcck__2__1__2, DELTA, {1, 0, 2}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified21__3__1__2__3, rel___dollorlst__2__2__1, DELTA, rel___dollorhead__stratified20__4__4__1, DELTA, {4, 5, 2}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified42__4__1__2__3__4, rel___dollorlst__2__1__2, FULL, rel___dollorinter__head30__5__3__2, DELTA, {4, 5, 6, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head8__8__2__7__6__1__4, rel___dollorinter__head8__8__1__2__3__4__5__6__7__8, DELTA, {1, 6, 5, 0, 3, 8, 2, 4, 7}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified33__4__1__2__3__4, rel__kaddr__2__2__1, DELTA, rel___dollorinter__body77__3__3__2, DELTA, {1, 0, 2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body4__7__1__2__3__4__5__6__7, rel__vaddr__2__1__2, DELTA, rel___dollorinter__head1__7__6__2, DELTA, {4, 1, 5, 6, 2, 7, 8}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body68__2__1, rel___dollorinter__body68__2__1__2, DELTA, {0, 2, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head8__8__1__2__3__4__5__6__7__8, rel___dollorinter__body26__2__1, DELTA, rel___dollorinter__body25__8__2, FULL, {4, 5, 6, 7, 8, 9, 2, 10}));
scc5681->add_rule(new parallel_acopy(rel__prim1__4__0, rel__prim1__4__1__2__3__4, DELTA, {4, 0, 1, 2, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body53__2__1__2, rel__callcc__2__1, FULL, rel__E__3__1, DELTA, {0, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body52__2__1__2, rel___dollornil__0__0, FULL, rel___dollorlst__2__2, DELTA, {0, 1}));
scc5681->add_rule(new parallel_copy(rel__kaddr__2__1__2, rel___dollorinter__head13__6__1__2__3__4__5__6, DELTA, {5, 3}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body12__2__1, rel___dollorinter__body12__2__1__2, DELTA, {0, 2, 1}));
scc5681->add_rule(new parallel_acopy(rel__E__3__2, rel__E__3__1__2__3, DELTA, {1, 3, 0, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body76__4__1__2__3__4, rel__prim1__4__3__2__1__4, DELTA, rel___dollorinter__head27__6__6__3__2__1, DELTA, {4, 6, 7, 0}));
scc5681->add_rule(new parallel_copy(rel__E__3__1__2__3, rel___dollorhead__stratified44__4__1__2__3__4, DELTA, {0, 1, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body65__3__1__2__3, rel___dollorinter__body64__1__1, DELTA, rel__ifk__4__0, DELTA, {2, 4, 5}));
scc5681->add_rule(new parallel_copy(rel__A__2__1__2, rel___dollorinter__head11__3__1__2__3, DELTA, {1, 0}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head30__5__3__2, rel___dollorinter__head30__5__1__2__3__4__5, DELTA, {2, 1, 5, 0, 3, 4}));
scc5681->add_rule(new parallel_copy(rel__flow__ee__2__1__2, rel___dollorinter__head27__6__1__2__3__4__5__6, DELTA, {4, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body16__3__1__2__3, rel__boolv__1__0, FULL, rel___dollorinter__body15__2__1, DELTA, {1, 0, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body76__4__1__2__3__4, rel__prim1__4__3__2__1__4, FULL, rel___dollorinter__head27__6__6__3__2__1, DELTA, {4, 6, 7, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body3__4__1__2__3__4, rel__letk__4__4__3__2__1, DELTA, rel___dollorhead__stratified2__7__1__2__4__7, FULL, {4, 6, 8, 7}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body88__2__1__2, rel__E__3__3__2__1, DELTA, rel___dollorhead__stratified44__4__3__2__1, FULL, {0, 5}));
scc5681->add_rule(new parallel_copy(rel__flow__ae__2__1__2, rel___dollorinter__head18__5__1__2__3__4__5, DELTA, {2, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body1__3__1__2__3, rel__flow__ee__2__2__1, DELTA, rel___dollorinter__body__4__1__3, DELTA, {0, 4, 5}));
scc5681->add_rule(new parallel_copy(rel__vaddr__2__1__2, rel___dollorinter__head32__4__1__2__3__4, DELTA, {2, 3}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body73__3__, rel___dollorinter__body73__3__1__2__3, DELTA, {3, 0, 1, 2}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified11__2__1__2, rel__flow__ae__2__1__2, FULL, rel___dollorinter__body23__3__3__2, DELTA, {0, 4}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified16__4__1__2__3__4, rel__kaddr__2__2__1, DELTA, rel___dollorinter__body36__3__2__3, FULL, {0, 1, 2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body2__4__1__2__3__4, rel__peek__ctx__3__2__1, DELTA, rel__E__3__2__1, DELTA, {5, 3, 1, 0}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body30__3__, rel___dollorinter__body30__3__1__2__3, DELTA, {3, 0, 1, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body13__3__1__2__3, rel__boolv__1__0, FULL, rel___dollorinter__body12__2__1, DELTA, {0, 3, 1}));
scc5681->add_rule(new parallel_copy_generate(rel___dollorinter__head22__4__1__2__3__4, rel___dollorinter__body65__3__, DELTA, [](const u64* data, u64* const output) -> int{
      auto args_for_old_bi = std::array<u64, 1> {data[0]};
      using TState = std::tuple<const u64*,u64*>;
      TState state = {data, output};
      auto callback = [](u64 res_0,  TState state) -> TState{
        auto [data, output] = state;
        bool compatible = true;
        if (! compatible) return state;

        auto head_tuple = output;
        head_tuple[0] = data[1];
head_tuple[1] = res_0;
head_tuple[2] = data[2];
head_tuple[3] = data[3];
        return {data, output + 4};
      };
      auto [_,new_ptr] = builtin_eq_1<TState>(args_for_old_bi.data(), state, callback);
      auto tuples_count = (new_ptr - output) / 4;
      return tuples_count;
    }));
scc5681->add_rule(new parallel_join(rel___dollorinter__body55__5__1__2__3__4__5, rel___dollorlst__2__0, FULL, rel___dollorinter__body54__4__4, DELTA, {2, 1, 4, 5, 6}));
scc5681->add_rule(new parallel_copy(rel__E__3__1__2__3, rel___dollorhead__stratified2__7__1__2__3__4__5__6__7, DELTA, {2, 4, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body73__3__1__2__3, rel___dollorinter__body72__1__1, DELTA, rel__ifk__4__0, DELTA, {2, 4, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body70__2__1__2, rel__E__3__3__2__1, DELTA, rel___dollorinter__head25__4__4__3__1, FULL, {2, 5}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified19__3__1__2__3, rel___dollorlst__2__2__1, DELTA, rel___dollorhead__stratified18__4__4__2, DELTA, {4, 5, 2}));
scc5681->add_rule(new parallel_copy_generate(rel___dollorinter__body17__2__1__2, rel___dollorinter__body16__3__1, DELTA, [](const u64* data, u64* const output) -> int{
      auto args_for_old_bi = std::array<u64, 2> {data[0], data[1]};
      using TState = std::tuple<const u64*,u64*>;
      TState state = {data, output};
      auto callback = []( TState state) -> TState{
        auto [data, output] = state;
        bool compatible = true;
        if (! compatible) return state;

        auto head_tuple = output;
        head_tuple[0] = data[2];
head_tuple[1] = data[3];
        return {data, output + 2};
      };
      auto [_,new_ptr] = builtin_eq<TState>(args_for_old_bi.data(), state, callback);
      auto tuples_count = (new_ptr - output) / 2;
      return tuples_count;
    }));
scc5681->add_rule(new parallel_acopy(rel___dollorhead__stratified18__4__4__2, rel___dollorhead__stratified18__4__1__2__3__4, DELTA, {3, 1, 4, 0, 2}));
scc5681->add_rule(new parallel_acopy(rel__ifk__4__3__2__4__1, rel__ifk__4__1__2__3__4, DELTA, {2, 1, 3, 0, 4}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head16__3__2, rel___dollorinter__head16__3__1__2__3, DELTA, {1, 3, 0, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body39__4__2, rel___dollorinter__body39__4__1__2__3__4, DELTA, {1, 4, 0, 2, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head23__5__1__2__3__4__5, rel___dollorinter__body66__2__1, DELTA, rel__letk__4__0, FULL, {4, 3, 2, 5, 6}));
scc5681->add_rule(new parallel_join(rel__peek__ctx__3__1__2__3, rel___dollorlst__2__2__1, DELTA, rel___dollorhead__stratified26__3__3__1, FULL, {1, 4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified39__4__1__2__3__4, rel__kaddr__2__1__2, DELTA, rel___dollorinter__body78__3__3__1, DELTA, {0, 4, 1, 2}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified26__3__1__2__3, rel___dollorlst__2__2__1, DELTA, rel___dollorhead__stratified25__4__4__1, FULL, {4, 5, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body32__4__1__2__3__4, rel__store__2__2, FULL, rel__ifk__4__0, DELTA, {3, 2, 5, 6}));
scc5681->add_rule(new parallel_join(rel__store__2__1__2, rel__vaddr__2__2__1, DELTA, rel___dollorinter__head31__3__2__1, FULL, {2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified16__4__1__2__3__4, rel__kaddr__2__2__1, DELTA, rel___dollorinter__body36__3__2__3, DELTA, {0, 1, 2, 4}));
scc5681->add_rule(new parallel_acopy(rel__kont__1__0, rel__kont__1__1, DELTA, {1, 0}));
scc5681->add_rule(new parallel_copy(rel__store__2__1__2, rel___dollorhead__stratified33__4__1__2__3__4, DELTA, {2, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body37__2__1__2, rel__A__2__2, FULL, rel__store__2__1, DELTA, {4, 2}));
scc5681->add_rule(new parallel_acopy(rel__E__3__1, rel__E__3__1__2__3, DELTA, {0, 3, 1, 2}));
scc5681->add_rule(new parallel_copy(rel__store__2__1__2, rel___dollorhead__stratified__4__1__2__3__4, DELTA, {3, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body1__3__1__2__3, rel__flow__ee__2__2__1, DELTA, rel___dollorinter__body__4__1__3, FULL, {0, 4, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body7__4__1__2__3__4, rel__E__3__2__1, DELTA, rel__peek__ctx__3__2__1, DELTA, {3, 5, 1, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body41__2__1__2, rel__E__3__1, DELTA, rel__let__3__1, FULL, {2, 0}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head25__4__4__3__1, rel___dollorinter__head25__4__1__2__3__4, DELTA, {3, 2, 0, 4, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body9__3__1__2__3, rel__flow__ee__2__2__1, DELTA, rel___dollorinter__body8__4__3__2, DELTA, {4, 0, 5}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified43__3__1__2__3, rel___dollorlst__2__2__1, FULL, rel___dollorhead__stratified42__4__4__2, DELTA, {4, 5, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorlst__2__2__1, rel___dollorlst__2__1__2, DELTA, {1, 0, 2}));
scc5681->add_rule(new parallel_copy(rel___dollorlst__2__1__2, rel___dollorhead__stratified21__3__1__2__3, DELTA, {0, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body34__3__1__2__3, rel___dollorinter__body33__1__1, DELTA, rel___dollorinter__body32__4__2, FULL, {3, 4, 5}));
scc5681->add_rule(new parallel_copy(rel___dollorlst__2__1__2, rel___dollorhead__stratified20__4__1__2__3__4, DELTA, {0, 3}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified43__3__1__2__3, rel___dollorlst__2__2__1, DELTA, rel___dollorhead__stratified42__4__4__2, FULL, {4, 5, 2}));
scc5681->add_rule(new parallel_join(rel__peek__ctx__3__1__2__3, rel___dollorlst__2__2__1, FULL, rel___dollorhead__stratified21__3__3__1, DELTA, {1, 4, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body75__3__2, rel___dollorinter__body75__3__1__2__3, DELTA, {1, 3, 0, 2}));
scc5681->add_rule(new parallel_copy(rel__kaddr__2__1__2, rel___dollorinter__head32__4__1__2__3__4, DELTA, {0, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body20__7__1__2__3__4__5__6__7, rel__here__5__5__4__3__2__1, DELTA, rel___dollorinter__head8__8__2__7__6__1__4, FULL, {0, 7, 4, 8, 2, 1, 9}));
scc5681->add_rule(new parallel_join(rel__peek__ctx__3__1__2__3, rel___dollorlst__2__2__1, DELTA, rel___dollorhead__stratified43__3__3__2, FULL, {1, 4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body86__4__1__2__3__4, rel__vaddr__2__2, FULL, rel__copy__ctx__3__1, DELTA, {2, 1, 4, 5}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body34__3__, rel___dollorinter__body34__3__1__2__3, DELTA, {3, 0, 1, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head26__3__1__2__3, rel__store__2__1, DELTA, rel___dollorinter__body75__3__2, FULL, {4, 5, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body37__2__1__2, rel__A__2__2, DELTA, rel__store__2__1, DELTA, {4, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body10__4__3, rel___dollorinter__body10__4__1__2__3__4, DELTA, {2, 4, 0, 1, 3}));
scc5681->add_rule(new parallel_acopy(rel___dollorhead__stratified25__4__4__1, rel___dollorhead__stratified25__4__1__2__3__4, DELTA, {3, 0, 4, 1, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head6__4__1__2__3__4, rel___dollorinter__body17__2__2, FULL, rel__ifk__4__0, DELTA, {3, 2, 5, 6}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head17__3__1__2__3, rel__boolv__1__1, DELTA, rel___dollorinter__head16__3__2, DELTA, {1, 3, 4}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body7__4__3, rel___dollorinter__body7__4__1__2__3__4, DELTA, {2, 4, 0, 1, 3}));
scc5681->add_rule(new parallel_copy(rel__vaddr__2__1__2, rel___dollorinter__head31__3__1__2__3, DELTA, {0, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body36__3__1__2__3, rel__flow__ae__2__2__1, DELTA, rel___dollorinter__body35__4__4__2, FULL, {4, 5, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body64__1__1, rel__closure__2__0, DELTA, rel___dollorinter__body63__2__1, DELTA, {4}));
scc5681->add_rule(new parallel_join(rel__peek__ctx__3__1__2__3, rel___dollorlst__2__2__1, FULL, rel___dollorhead__stratified43__3__3__2, DELTA, {1, 4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body31__2__1__2, rel__E__3__3__2__1, FULL, rel___dollorinter__head12__4__4__3__1, DELTA, {2, 5}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body37__2__1, rel___dollorinter__body37__2__1__2, DELTA, {0, 2, 1}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body77__3__3__2, rel___dollorinter__body77__3__1__2__3, DELTA, {2, 1, 3, 0}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head13__6__5__2__1__3, rel___dollorinter__head13__6__1__2__3__4__5__6, DELTA, {4, 1, 0, 2, 6, 3, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body27__3__1__2__3, rel__store__2__2__1, FULL, rel___dollorinter__head9__4__3__2, DELTA, {4, 0, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body10__4__1__2__3__4, rel__E__3__2__1, FULL, rel__peek__ctx__3__2__1, DELTA, {3, 5, 1, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body74__4__1__2__3__4, rel__vaddr__2__2, FULL, rel__E__3__2, DELTA, {5, 1, 4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head20__3__1__2__3, rel__kont__1__0, FULL, rel___dollorinter__body58__2__2, DELTA, {3, 1, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body79__4__1__2__3__4, rel___dollorlst__2__2__1, DELTA, rel___dollorhead__stratified37__4__4__2, DELTA, {2, 5, 0, 4}));
scc5681->add_rule(new parallel_copy(rel___dollorlst__2__1__2, rel___dollorhead__stratified19__3__1__2__3, DELTA, {1, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body72__1__1, rel__kont__1__0, DELTA, rel___dollorinter__body71__2__2, FULL, {3}));
scc5681->add_rule(new parallel_acopy(rel__E__3__2__1, rel__E__3__1__2__3, DELTA, {1, 0, 3, 2}));
scc5681->add_rule(new parallel_copy(rel__E__3__1__2__3, rel___dollorhead__stratified33__4__1__2__3__4, DELTA, {0, 1, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body15__2__1__2, rel__A__2__2, FULL, rel__store__2__1, DELTA, {2, 4}));
scc5681->add_rule(new parallel_copy(rel___dollorlst__2__1__2, rel___dollorhead__stratified43__3__1__2__3, DELTA, {1, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body18__4__1__2__3__4, rel__ifk__4__3__2__4__1, DELTA, rel___dollorinter__head7__6__6__4__2__1, FULL, {6, 4, 7, 0}));
scc5681->add_rule(new parallel_copy_generate(rel___dollorinter__head25__4__1__2__3__4, rel___dollorinter__body73__3__, DELTA, [](const u64* data, u64* const output) -> int{
      auto args_for_old_bi = std::array<u64, 1> {data[0]};
      using TState = std::tuple<const u64*,u64*>;
      TState state = {data, output};
      auto callback = [](u64 res_0,  TState state) -> TState{
        auto [data, output] = state;
        bool compatible = true;
        if (! compatible) return state;

        auto head_tuple = output;
        head_tuple[0] = data[1];
head_tuple[1] = res_0;
head_tuple[2] = data[2];
head_tuple[3] = data[3];
        return {data, output + 4};
      };
      auto [_,new_ptr] = builtin_eq_1<TState>(args_for_old_bi.data(), state, callback);
      auto tuples_count = (new_ptr - output) / 4;
      return tuples_count;
    }));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body21__5__5__1__2, rel___dollorinter__body21__5__1__2__3__4__5, DELTA, {4, 0, 1, 5, 2, 3}));
scc5681->add_rule(new parallel_acopy(rel__flow__ae__2__2__1, rel__flow__ae__2__1__2, DELTA, {1, 0, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body37__2__1__2, rel__A__2__2, DELTA, rel__store__2__1, FULL, {4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body69__2__1__2, rel__store__2__1, FULL, rel__A__2__2, DELTA, {2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body15__2__1__2, rel__A__2__2, DELTA, rel__store__2__1, DELTA, {2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body28__4__1__2__3__4, rel__number__1__1, FULL, rel___dollorinter__body27__3__1, DELTA, {0, 1, 3, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body76__4__1__2__3__4, rel__prim1__4__3__2__1__4, DELTA, rel___dollorinter__head27__6__6__3__2__1, FULL, {4, 6, 7, 0}));
scc5681->add_rule(new parallel_acopy(rel__vaddr__2__2, rel__vaddr__2__1__2, DELTA, {1, 2, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head26__3__1__2__3, rel__store__2__1, DELTA, rel___dollorinter__body75__3__2, DELTA, {4, 5, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body32__4__2, rel___dollorinter__body32__4__1__2__3__4, DELTA, {1, 4, 0, 2, 3}));
scc5681->add_rule(new parallel_copy(rel__kaddr__2__1__2, rel___dollorinter__head18__5__1__2__3__4__5, DELTA, {1, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body89__3__1__2__3, rel__flow__ee__2__2__1, DELTA, rel___dollorinter__head32__4__1__2, DELTA, {0, 4, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body33__1__1, rel__number__1__0, FULL, rel__A__2__1, DELTA, {3}));
scc5681->add_rule(new parallel_copy(rel__flow__ae__2__1__2, rel___dollorhead__stratified15__2__1__2, DELTA, {1, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body25__8__1__2__3__4__5__6__7__8, rel___dollorbir__sub3__4__3__1, FULL, rel___dollorinter__body24__6__1__4, DELTA, {0, 6, 7, 1, 3, 8, 4, 9}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head16__3__1__2__3, rel__bool__2__1, FULL, rel__E__3__1, DELTA, {5, 2, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head3__4__1__2__3__4, rel___dollorinter__body11__2__1, FULL, rel__prim2__3__0, DELTA, {3, 4, 5, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body23__3__3__2, rel___dollorinter__body23__3__1__2__3, DELTA, {2, 1, 3, 0}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified23__4__1__2__3__4, rel__kaddr__2__2__1, FULL, rel___dollorinter__body48__3__3__1, DELTA, {1, 0, 4, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head14__5__3__1, rel___dollorinter__head14__5__1__2__3__4__5, DELTA, {2, 0, 5, 1, 3, 4}));
scc5681->add_rule(new parallel_acopy(rel___dollorhead__stratified20__4__4__1, rel___dollorhead__stratified20__4__1__2__3__4, DELTA, {3, 0, 4, 1, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body21__5__1__2__3__4__5, rel__copy__ctx__3__3__2__1, DELTA, rel___dollorinter__body20__7__2__1__4, DELTA, {1, 5, 6, 7, 8}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body66__2__1, rel___dollorinter__body66__2__1__2, DELTA, {0, 2, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body33__1__1, rel__number__1__0, DELTA, rel__A__2__1, DELTA, {3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body89__3__1__2__3, rel__flow__ee__2__2__1, FULL, rel___dollorinter__head32__4__1__2, DELTA, {0, 4, 5}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body40__4__2, rel___dollorinter__body40__4__1__2__3__4, DELTA, {1, 4, 0, 2, 3}));
scc5681->add_rule(new parallel_acopy(rel___dollorhead__stratified38__4__4__3__1, rel___dollorhead__stratified38__4__1__2__3__4, DELTA, {3, 2, 0, 4, 1}));
scc5681->add_rule(new parallel_copy(rel___dollorlst__2__1__2, rel___dollorinter__head14__5__1__2__3__4__5, DELTA, {2, 0}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified15__2__1__2, rel__boolv__1__1, DELTA, rel___dollorinter__body31__2__2, FULL, {3, 1}));
scc5681->add_rule(new parallel_copy(rel__boolv__1__1, rel___dollorinter__head22__4__1__2__3__4, DELTA, {1}));
scc5681->add_rule(new parallel_copy(rel__E__3__1__2__3, rel___dollorinter__head23__5__1__2__3__4__5, DELTA, {0, 3, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body39__4__1__2__3__4, rel___dollorlst__2__0, DELTA, rel___dollorlst__2__2, DELTA, {3, 2, 1, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body43__3__1__2__3, rel___dollorlst__2__2, FULL, rel___dollorlst__2__0, DELTA, {4, 2, 1}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified45__2__1__2, rel__setk__1__1, FULL, rel___dollorinter__body88__2__2, DELTA, {3, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head20__3__1__2__3, rel__kont__1__0, DELTA, rel___dollorinter__body58__2__2, DELTA, {3, 1, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body47__4__1__2__3__4, rel__prim2__3__3__2__1, DELTA, rel___dollorinter__head18__5__5__3__1, FULL, {5, 3, 1, 6}));
scc5681->add_rule(new parallel_copy(rel__E__3__1__2__3, rel___dollorhead__stratified39__4__1__2__3__4, DELTA, {0, 2, 3}));
scc5681->add_rule(new parallel_copy(rel__number__1__1, rel___dollorinter__head9__4__1__2__3__4, DELTA, {0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body13__3__1__2__3, rel__boolv__1__0, DELTA, rel___dollorinter__body12__2__1, FULL, {0, 3, 1}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body24__6__1__4, rel___dollorinter__body24__6__1__2__3__4__5__6, DELTA, {0, 3, 6, 1, 2, 4, 5}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified18__4__1__2__3__4, rel___dollorlst__2__1__2, DELTA, rel___dollorinter__head14__5__3__1, FULL, {4, 5, 6, 2}));
scc5681->add_rule(new parallel_acopy(rel__letk__4__4__3__2__1, rel__letk__4__1__2__3__4, DELTA, {3, 2, 1, 0, 4}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified42__4__1__2__3__4, rel___dollorlst__2__1__2, DELTA, rel___dollorinter__head30__5__3__2, FULL, {4, 5, 6, 2}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified3__2__1__2, rel__E__3__3__2__1, DELTA, rel___dollorinter__body3__4__3__4__2, DELTA, {0, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body88__2__1__2, rel__E__3__3__2__1, FULL, rel___dollorhead__stratified44__4__3__2__1, DELTA, {0, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head21__3__1__2__3, rel__kont__1__1, DELTA, rel___dollorinter__head20__3__1, DELTA, {1, 3, 4}));
scc5681->add_rule(new parallel_acopy(rel__store__2__2, rel__store__2__1__2, DELTA, {1, 2, 0}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified37__4__1__2__3__4, rel___dollorlst__2__2__1, DELTA, rel___dollorhead__stratified36__4__4__2, FULL, {4, 1, 5, 2}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified37__4__1__2__3__4, rel___dollorlst__2__2__1, DELTA, rel___dollorhead__stratified36__4__4__2, DELTA, {4, 1, 5, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head18__5__1__2__3__4__5, rel__A__2__2, FULL, rel___dollorinter__body49__5__1, DELTA, {4, 5, 2, 6, 7}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head15__5__1__2__3__4__5, rel___dollorinter__body44__2__2, DELTA, rel___dollorinter__body46__4__4, DELTA, {4, 5, 6, 2, 0}));
scc5681->add_rule(new parallel_copy_generate(rel___dollorinter__head12__4__1__2__3__4, rel___dollorinter__body34__3__, DELTA, [](const u64* data, u64* const output) -> int{
      auto args_for_old_bi = std::array<u64, 1> {data[0]};
      using TState = std::tuple<const u64*,u64*>;
      TState state = {data, output};
      auto callback = [](u64 res_0,  TState state) -> TState{
        auto [data, output] = state;
        bool compatible = true;
        if (! compatible) return state;

        auto head_tuple = output;
        head_tuple[0] = data[1];
head_tuple[1] = res_0;
head_tuple[2] = data[2];
head_tuple[3] = data[3];
        return {data, output + 4};
      };
      auto [_,new_ptr] = builtin_eq_1<TState>(args_for_old_bi.data(), state, callback);
      auto tuples_count = (new_ptr - output) / 4;
      return tuples_count;
    }));
scc5681->add_rule(new parallel_join(rel___dollorinter__body66__2__1__2, rel__A__2__2, DELTA, rel__store__2__1, DELTA, {4, 2}));
scc5681->add_rule(new parallel_copy(rel__boolv__1__1, rel___dollorinter__head25__4__1__2__3__4, DELTA, {1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body21__5__1__2__3__4__5, rel__copy__ctx__3__3__2__1, FULL, rel___dollorinter__body20__7__2__1__4, DELTA, {1, 5, 6, 7, 8}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body3__4__3__4__2, rel___dollorinter__body3__4__1__2__3__4, DELTA, {2, 3, 1, 4, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body31__2__1__2, rel__E__3__3__2__1, DELTA, rel___dollorinter__head12__4__4__3__1, DELTA, {2, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head13__6__1__2__3__4__5__6, rel___dollorinter__body37__2__1, DELTA, rel___dollorinter__body38__6__2, DELTA, {4, 5, 2, 6, 7, 8}));
scc5681->add_rule(new parallel_copy(rel__letk__4__1__2__3__4, rel___dollorhead__stratified2__7__1__2__3__4__5__6__7, DELTA, {6, 3, 1, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body64__1__1, rel__closure__2__0, DELTA, rel___dollorinter__body63__2__1, FULL, {4}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified9__4__1__2__3__4, rel__kaddr__2__2__1, DELTA, rel___dollorinter__body19__3__3__1, DELTA, {1, 0, 2, 4}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body29__2__1, rel___dollorinter__body29__2__1__2, DELTA, {0, 2, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body48__3__1__2__3, rel__flow__ae__2__1__2, FULL, rel___dollorinter__body47__4__3__1, DELTA, {1, 4, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body18__4__1__2__3__4, rel__ifk__4__3__2__4__1, FULL, rel___dollorinter__head7__6__6__4__2__1, DELTA, {6, 4, 7, 0}));
scc5681->add_rule(new parallel_copy(rel__store__2__1__2, rel___dollorhead__stratified16__4__1__2__3__4, DELTA, {2, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body82__3__1__2__3, rel___dollorlst__2__0, DELTA, rel___dollorlst__2__2, DELTA, {3, 2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body33__1__1, rel__number__1__0, DELTA, rel__A__2__1, FULL, {3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body45__3__1__2__3, rel___dollornil__0__0, FULL, rel___dollorinter__body43__3__1, DELTA, {0, 2, 3}));
scc5681->add_rule(new parallel_copy(rel__flow__ee__2__1__2, rel___dollorinter__head2__6__1__2__3__4__5__6, DELTA, {2, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body62__2__1__2, rel__E__3__3__2__1, FULL, rel___dollorinter__head22__4__4__3__1, DELTA, {2, 5}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body45__3__3, rel___dollorinter__body45__3__1__2__3, DELTA, {2, 3, 0, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head7__6__1__2__3__4__5__6, rel__E__3__1, DELTA, rel__if__4__1, FULL, {6, 3, 5, 7, 0, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body71__2__2, rel___dollorinter__body71__2__1__2, DELTA, {1, 2, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body54__4__1__2__3__4, rel___dollorinter__body53__2__2, DELTA, rel___dollorlst__2__0, FULL, {3, 2, 0, 4}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head31__3__2__1, rel___dollorinter__head31__3__1__2__3, DELTA, {1, 0, 3, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head30__5__1__2__3__4__5, rel___dollorinter__body83__2__1, DELTA, rel___dollorinter__body85__4__1, FULL, {0, 4, 5, 6, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body79__4__1__2__3__4, rel___dollorlst__2__2__1, DELTA, rel___dollorhead__stratified37__4__4__2, FULL, {2, 5, 0, 4}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body79__4__3__4, rel___dollorinter__body79__4__1__2__3__4, DELTA, {2, 3, 4, 0, 1}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body88__2__2, rel___dollorinter__body88__2__1__2, DELTA, {1, 2, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body84__3__1__2__3, rel___dollornil__0__0, FULL, rel___dollorinter__body82__3__2, DELTA, {2, 0, 3}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified44__4__1__2__3__4, rel__kaddr__2__2__1, DELTA, rel___dollorinter__body90__3__2__1, DELTA, {1, 0, 2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified38__4__1__2__3__4, rel___dollorlst__2__2__1, DELTA, rel___dollorinter__body79__4__3__4, DELTA, {1, 5, 2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head21__3__1__2__3, rel__kont__1__1, DELTA, rel___dollorinter__head20__3__1, FULL, {1, 3, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head24__2__1__2, rel___dollorinter__body68__2__1, DELTA, rel___dollorinter__body69__2__1, FULL, {2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body3__4__1__2__3__4, rel__letk__4__4__3__2__1, FULL, rel___dollorhead__stratified2__7__1__2__4__7, DELTA, {4, 6, 8, 7}));
scc5681->add_rule(new parallel_copy(rel__flow__ae__2__1__2, rel___dollorinter__head8__8__1__2__3__4__5__6__7__8, DELTA, {6, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body34__3__1__2__3, rel___dollorinter__body33__1__1, DELTA, rel___dollorinter__body32__4__2, DELTA, {3, 4, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body72__1__1, rel__kont__1__0, FULL, rel___dollorinter__body71__2__2, DELTA, {3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body63__2__1__2, rel__A__2__2, FULL, rel__store__2__1, DELTA, {2, 4}));
scc5681->add_rule(new parallel_copy(rel__fn__4__1__2__3__4, rel___dollorinter__head13__6__1__2__3__4__5__6, DELTA, {2, 0, 1, 4}));
scc5681->add_rule(new parallel_copy(rel__flow__ae__2__1__2, rel___dollorhead__stratified28__2__1__2, DELTA, {1, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body26__2__1__2, rel__store__2__1, DELTA, rel__A__2__2, FULL, {2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body23__3__1__2__3, rel__vaddr__2__1__2, DELTA, rel___dollorinter__body22__4__3__1, FULL, {2, 4, 5}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head10__3__3__2, rel___dollorinter__head10__3__1__2__3, DELTA, {2, 1, 3, 0}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body26__2__1, rel___dollorinter__body26__2__1__2, DELTA, {0, 2, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head17__3__1__2__3, rel__boolv__1__1, DELTA, rel___dollorinter__head16__3__2, FULL, {1, 3, 4}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified39__4__1__2__3__4, rel__kaddr__2__1__2, DELTA, rel___dollorinter__body78__3__3__1, FULL, {0, 4, 1, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body__4__1__2__3__4, rel__callcck__2__2__1, FULL, rel___dollorinter__head__5__1__2, DELTA, {4, 2, 5, 6}));
scc5681->add_rule(new parallel_copy(rel__store__2__1__2, rel___dollorhead__stratified39__4__1__2__3__4, DELTA, {3, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body11__2__1__2, rel__store__2__1, FULL, rel__A__2__2, DELTA, {2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified__4__1__2__3__4, rel__kaddr__2__2__1, DELTA, rel___dollorinter__body1__3__3__1, FULL, {1, 0, 4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body10__4__1__2__3__4, rel__E__3__2__1, DELTA, rel__peek__ctx__3__2__1, FULL, {3, 5, 1, 0}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified20__4__1__2__3__4, rel___dollorlst__2__2__1, DELTA, rel___dollorinter__head15__5__1__2, DELTA, {4, 5, 6, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head11__3__1__2__3, rel__closure__2__2__1, FULL, rel___dollorinter__head10__3__3__2, DELTA, {4, 2, 1}));
scc5681->add_rule(new parallel_copy(rel__E__3__1__2__3, rel___dollorinter__head12__4__1__2__3__4, DELTA, {0, 2, 3}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body48__3__3__1, rel___dollorinter__body48__3__1__2__3, DELTA, {2, 0, 3, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body4__7__1__2__3__4__5__6__7, rel__vaddr__2__1__2, FULL, rel___dollorinter__head1__7__6__2, DELTA, {4, 1, 5, 6, 2, 7, 8}));
scc5681->add_rule(new parallel_copy(rel__E__3__1__2__3, rel___dollorhead__stratified16__4__1__2__3__4, DELTA, {1, 0, 2}));
scc5681->add_rule(new parallel_join(rel__peek__ctx__3__1__2__3, rel___dollorlst__2__2__1, DELTA, rel___dollorhead__stratified21__3__3__1, DELTA, {1, 4, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body20__7__2__1__4, rel___dollorinter__body20__7__1__2__3__4__5__6__7, DELTA, {1, 0, 3, 7, 2, 4, 5, 6}));
scc5681->add_rule(new parallel_copy(rel__copy__ctx__3__1__2__3, rel___dollorinter__head8__8__1__2__3__4__5__6__7__8, DELTA, {4, 1, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body49__5__1__2__3__4__5, rel__store__2__2, DELTA, rel__prim1__4__0, FULL, {2, 3, 4, 5, 6}));
scc5681->add_rule(new parallel_copy(rel__store__2__1__2, rel___dollorinter__head23__5__1__2__3__4__5, DELTA, {1, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorhead__stratified21__3__3__1, rel___dollorhead__stratified21__3__1__2__3, DELTA, {2, 0, 3, 1}));
scc5681->add_rule(new parallel_copy(rel__store__2__1__2, rel___dollorhead__stratified4__4__1__2__3__4, DELTA, {2, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body49__5__1__2__3__4__5, rel__store__2__2, FULL, rel__prim1__4__0, DELTA, {2, 3, 4, 5, 6}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body64__1__1, rel__closure__2__0, FULL, rel___dollorinter__body63__2__1, DELTA, {4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body2__4__1__2__3__4, rel__peek__ctx__3__2__1, DELTA, rel__E__3__2__1, FULL, {5, 3, 1, 0}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head22__4__4__3__1, rel___dollorinter__head22__4__1__2__3__4, DELTA, {3, 2, 0, 4, 1}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified15__2__1__2, rel__boolv__1__1, DELTA, rel___dollorinter__body31__2__2, DELTA, {3, 1}));
scc5681->add_rule(new parallel_acopy(rel__ifk__4__0, rel__ifk__4__1__2__3__4, DELTA, {4, 0, 1, 2, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head24__2__1__2, rel___dollorinter__body68__2__1, DELTA, rel___dollorinter__body69__2__1, DELTA, {2, 4}));
scc5681->add_rule(new parallel_acopy(rel___dollorlst__2__0, rel___dollorlst__2__1__2, DELTA, {2, 0, 1}));
scc5681->add_rule(new parallel_copy(rel__E__3__1__2__3, rel___dollorinter__head22__4__1__2__3__4, DELTA, {0, 2, 3}));
scc5681->add_rule(new parallel_copy(rel__E__3__1__2__3, rel___dollorinter__head6__4__1__2__3__4, DELTA, {0, 2, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body87__3__1__2__3, rel___dollorbir__sub__2__2__1, FULL, rel___dollorinter__body86__4__4__1, DELTA, {1, 4, 5}));
scc5681->add_rule(new parallel_copy(rel__E__3__1__2__3, rel___dollorinter__head8__8__1__2__3__4__5__6__7__8, DELTA, {3, 1, 7}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head29__3__1__2__3, rel__number__1__1, FULL, rel___dollorinter__head28__3__2, DELTA, {3, 1, 4}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified15__2__1__2, rel__boolv__1__1, FULL, rel___dollorinter__body31__2__2, DELTA, {3, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body70__2__1__2, rel__E__3__3__2__1, DELTA, rel___dollorinter__head25__4__4__3__1, DELTA, {2, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body55__5__1__2__3__4__5, rel___dollorlst__2__0, DELTA, rel___dollorinter__body54__4__4, FULL, {2, 1, 4, 5, 6}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified38__4__1__2__3__4, rel___dollorlst__2__2__1, FULL, rel___dollorinter__body79__4__3__4, DELTA, {1, 5, 2, 4}));
scc5681->add_rule(new parallel_join(rel__peek__ctx__3__1__2__3, rel___dollorlst__2__2__1, DELTA, rel___dollorhead__stratified26__3__3__1, DELTA, {1, 4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body90__3__1__2__3, rel__vaddr__2__2__1, DELTA, rel___dollorinter__body89__3__3__2, FULL, {4, 0, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body48__3__1__2__3, rel__flow__ae__2__1__2, DELTA, rel___dollorinter__body47__4__3__1, FULL, {1, 4, 5}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body17__2__2, rel___dollorinter__body17__2__1__2, DELTA, {1, 2, 0}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body85__4__1, rel___dollorinter__body85__4__1__2__3__4, DELTA, {0, 4, 1, 2, 3}));
scc5681->add_rule(new parallel_acopy(rel__number__1__0, rel__number__1__1, DELTA, {1, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head11__3__1__2__3, rel__closure__2__2__1, DELTA, rel___dollorinter__head10__3__3__2, DELTA, {4, 2, 1}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body62__2__2, rel___dollorinter__body62__2__1__2, DELTA, {1, 2, 0}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body83__2__1, rel___dollorinter__body83__2__1__2, DELTA, {0, 2, 1}));
scc5681->add_rule(new parallel_copy(rel__kaddr__2__1__2, rel___dollorinter__head27__6__1__2__3__4__5__6, DELTA, {3, 5}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified9__4__1__2__3__4, rel__kaddr__2__2__1, FULL, rel___dollorinter__body19__3__3__1, DELTA, {1, 0, 2, 4}));
scc5681->add_rule(new parallel_copy(rel__E__3__1__2__3, rel___dollorhead__stratified9__4__1__2__3__4, DELTA, {0, 1, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body44__2__1__2, rel__call__3__1, FULL, rel__E__3__1, DELTA, {0, 5}));
scc5681->add_rule(new parallel_acopy(rel__copy__ctx__3__3__2__1, rel__copy__ctx__3__1__2__3, DELTA, {2, 1, 0, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body90__3__1__2__3, rel__vaddr__2__2__1, DELTA, rel___dollorinter__body89__3__3__2, DELTA, {4, 0, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body36__3__1__2__3, rel__flow__ae__2__2__1, DELTA, rel___dollorinter__body35__4__4__2, DELTA, {4, 5, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body29__2__1__2, rel__A__2__2, FULL, rel__store__2__1, DELTA, {4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body67__3__1__2__3, rel__kont__1__0, DELTA, rel__fn__4__1, DELTA, {2, 1, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body88__2__1__2, rel__E__3__3__2__1, DELTA, rel___dollorhead__stratified44__4__3__2__1, DELTA, {0, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body11__2__1__2, rel__store__2__1, DELTA, rel__A__2__2, DELTA, {2, 4}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head1__7__6__2, rel___dollorinter__head1__7__1__2__3__4__5__6__7, DELTA, {5, 1, 7, 0, 2, 3, 4, 6}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head2__6__1__2__3__4__5__6, rel___dollorbir__sub4__3__1, FULL, rel___dollorinter__body10__4__3, DELTA, {5, 6, 0, 2, 7, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body6__6__1__2__3__4__5__6, rel__flow__ee__2__2__1, FULL, rel___dollorinter__body5__7__3__6, DELTA, {4, 5, 0, 6, 7, 8}));
scc5681->add_rule(new parallel_acopy(rel__fn__4__4__3__2__1, rel__fn__4__1__2__3__4, DELTA, {3, 2, 1, 0, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body42__5__1__2__3__4__5, rel___dollorinter__body41__2__1, DELTA, rel___dollorinter__body40__4__2, FULL, {4, 0, 5, 6, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body72__1__1, rel__kont__1__0, DELTA, rel___dollorinter__body71__2__2, DELTA, {3}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body31__2__2, rel___dollorinter__body31__2__1__2, DELTA, {1, 2, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head29__3__1__2__3, rel__number__1__1, DELTA, rel___dollorinter__head28__3__2, FULL, {3, 1, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body90__3__1__2__3, rel__vaddr__2__2__1, FULL, rel___dollorinter__body89__3__3__2, DELTA, {4, 0, 2}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified4__4__1__2__3__4, rel__kaddr__2__2__1, DELTA, rel___dollorinter__body9__3__3__2, DELTA, {1, 0, 2, 4}));
scc5681->add_rule(new parallel_copy(rel___dollorlst__2__1__2, rel___dollorhead__stratified37__4__1__2__3__4, DELTA, {1, 3}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified25__4__1__2__3__4, rel___dollorlst__2__2__1, DELTA, rel___dollorinter__head19__5__1__2, FULL, {4, 5, 6, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head26__3__1__2__3, rel__store__2__1, FULL, rel___dollorinter__body75__3__2, DELTA, {4, 5, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body14__5__5, rel___dollorinter__body14__5__1__2__3__4__5, DELTA, {4, 5, 0, 1, 2, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body18__4__1__2__3__4, rel__ifk__4__3__2__4__1, DELTA, rel___dollorinter__head7__6__6__4__2__1, DELTA, {6, 4, 7, 0}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head18__5__5__3__1, rel___dollorinter__head18__5__1__2__3__4__5, DELTA, {4, 2, 0, 5, 1, 3}));
scc5681->add_rule(new parallel_acopy(rel___dollorhead__stratified26__3__3__1, rel___dollorhead__stratified26__3__1__2__3, DELTA, {2, 0, 3, 1}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified33__4__1__2__3__4, rel__kaddr__2__2__1, DELTA, rel___dollorinter__body77__3__3__2, FULL, {1, 0, 2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body8__4__1__2__3__4, rel__argk__4__1__2__3__4, DELTA, rel___dollorinter__head2__6__6__5__2__1, DELTA, {4, 6, 7, 1}));
scc5681->add_rule(new parallel_copy(rel___dollorlst__2__1__2, rel___dollorhead__stratified37__4__1__2__3__4, DELTA, {0, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head30__5__1__2__3__4__5, rel___dollorinter__body83__2__1, FULL, rel___dollorinter__body85__4__1, DELTA, {0, 4, 5, 6, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body75__3__1__2__3, rel__var__2__2__1, FULL, rel___dollorinter__body74__4__4__3, DELTA, {4, 5, 1}));
scc5681->add_rule(new parallel_copy(rel__prim1__4__1__2__3__4, rel___dollorinter__head27__6__1__2__3__4__5__6, DELTA, {1, 2, 5, 0}));
scc5681->add_rule(new parallel_copy(rel__E__3__1__2__3, rel___dollorinter__head5__4__1__2__3__4, DELTA, {0, 2, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head18__5__1__2__3__4__5, rel__A__2__2, DELTA, rel___dollorinter__body49__5__1, FULL, {4, 5, 2, 6, 7}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified36__4__1__2__3__4, rel___dollorlst__2__2__1, DELTA, rel___dollorhead__stratified35__4__4__2, FULL, {4, 1, 5, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body44__2__2, rel___dollorinter__body44__2__1__2, DELTA, {1, 2, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head4__3__1__2__3, rel__primval__3__3__2__1, DELTA, rel___dollorinter__head3__4__4__2__1, DELTA, {3, 5, 0}));
scc5681->add_rule(new parallel_copy_generate(rel___dollorinter__head9__4__1__2__3__4, rel___dollorinter__body30__3__, DELTA, [](const u64* data, u64* const output) -> int{
      auto args_for_old_bi = std::array<u64, 1> {data[0]};
      using TState = std::tuple<const u64*,u64*>;
      TState state = {data, output};
      auto callback = [](u64 res_0,  TState state) -> TState{
        auto [data, output] = state;
        bool compatible = true;
        if (! compatible) return state;

        auto head_tuple = output;
        head_tuple[0] = res_0;
head_tuple[1] = data[1];
head_tuple[2] = data[2];
head_tuple[3] = data[3];
        return {data, output + 4};
      };
      auto [_,new_ptr] = builtin_eq_1<TState>(args_for_old_bi.data(), state, callback);
      auto tuples_count = (new_ptr - output) / 4;
      return tuples_count;
    }));
scc5681->add_rule(new parallel_join(rel___dollorinter__body43__3__1__2__3, rel___dollorlst__2__2, DELTA, rel___dollorlst__2__0, FULL, {4, 2, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head3__4__1__2__3__4, rel___dollorinter__body11__2__1, DELTA, rel__prim2__3__0, DELTA, {3, 4, 5, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body54__4__1__2__3__4, rel___dollorinter__body53__2__2, FULL, rel___dollorlst__2__0, DELTA, {3, 2, 0, 4}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified9__4__1__2__3__4, rel__kaddr__2__2__1, DELTA, rel___dollorinter__body19__3__3__1, FULL, {1, 0, 2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified37__4__1__2__3__4, rel___dollorlst__2__2__1, FULL, rel___dollorhead__stratified36__4__4__2, DELTA, {4, 1, 5, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body71__2__1__2, rel__store__2__1, FULL, rel__A__2__2, DELTA, {2, 4}));
scc5681->add_rule(new parallel_copy(rel___dollorlst__2__1__2, rel___dollorhead__stratified18__4__1__2__3__4, DELTA, {1, 3}));
scc5681->add_rule(new parallel_acopy(rel__E__3__3__2__1, rel__E__3__1__2__3, DELTA, {2, 1, 0, 3}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified33__4__1__2__3__4, rel__kaddr__2__2__1, FULL, rel___dollorinter__body77__3__3__2, DELTA, {1, 0, 2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified26__3__1__2__3, rel___dollorlst__2__2__1, FULL, rel___dollorhead__stratified25__4__4__1, DELTA, {4, 5, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body16__3__1, rel___dollorinter__body16__3__1__2__3, DELTA, {0, 3, 1, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head1__7__1__2__3__4__5__6__7, rel___dollorbir__sub1__4__1, FULL, rel___dollorinter__body7__4__3, DELTA, {6, 7, 4, 2, 0, 3, 8}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body5__7__3__6, rel___dollorinter__body5__7__1__2__3__4__5__6__7, DELTA, {2, 5, 7, 0, 1, 3, 4, 6}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body49__5__1__2__3__4__5, rel__store__2__2, DELTA, rel__prim1__4__0, DELTA, {2, 3, 4, 5, 6}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body78__3__3__1, rel___dollorinter__body78__3__1__2__3, DELTA, {2, 0, 3, 1}));
scc5681->add_rule(new parallel_copy(rel__here__5__1__2__3__4__5, rel___dollorinter__head8__8__1__2__3__4__5__6__7__8, DELTA, {3, 0, 5, 6, 1}));
scc5681->add_rule(new parallel_acopy(rel___dollorhead__stratified42__4__4__2, rel___dollorhead__stratified42__4__1__2__3__4, DELTA, {3, 1, 4, 0, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body8__4__3__2, rel___dollorinter__body8__4__1__2__3__4, DELTA, {2, 1, 4, 0, 3}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head9__4__3__2, rel___dollorinter__head9__4__1__2__3__4, DELTA, {2, 1, 4, 0, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body22__4__1__2__3__4, rel__E__3__3__2__1, FULL, rel___dollorinter__body21__5__5__1__2, DELTA, {1, 2, 5, 6}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body67__3__1__2__3, rel__kont__1__0, DELTA, rel__fn__4__1, FULL, {2, 1, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body69__2__1__2, rel__store__2__1, DELTA, rel__A__2__2, DELTA, {2, 4}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body41__2__1, rel___dollorinter__body41__2__1__2, DELTA, {0, 2, 1}));
scc5681->add_rule(new parallel_acopy(rel__vaddr__2__2__1, rel__vaddr__2__1__2, DELTA, {1, 0, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body19__3__1__2__3, rel__flow__ee__2__1__2, DELTA, rel___dollorinter__body18__4__3__1, FULL, {1, 4, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body32__4__1__2__3__4, rel__store__2__2, DELTA, rel__ifk__4__0, DELTA, {3, 2, 5, 6}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body13__3__1__2__3, rel__boolv__1__0, DELTA, rel___dollorinter__body12__2__1, DELTA, {0, 3, 1}));
scc5681->add_rule(new parallel_copy(rel__kont__1__1, rel___dollorinter__head20__3__1__2__3, DELTA, {0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body34__3__1__2__3, rel___dollorinter__body33__1__1, FULL, rel___dollorinter__body32__4__2, DELTA, {3, 4, 5}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified42__4__1__2__3__4, rel___dollorlst__2__1__2, DELTA, rel___dollorinter__head30__5__3__2, DELTA, {4, 5, 6, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head13__6__1__2__3__4__5__6, rel___dollorinter__body37__2__1, FULL, rel___dollorinter__body38__6__2, DELTA, {4, 5, 2, 6, 7, 8}));
scc5681->add_rule(new parallel_copy_generate(rel___dollorinter__body68__2__1__2, rel___dollorinter__body67__3__3, DELTA, [](const u64* data, u64* const output) -> int{
      auto args_for_old_bi = std::array<u64, 2> {data[0], data[1]};
      using TState = std::tuple<const u64*,u64*>;
      TState state = {data, output};
      auto callback = []( TState state) -> TState{
        auto [data, output] = state;
        bool compatible = true;
        if (! compatible) return state;

        auto head_tuple = output;
        head_tuple[0] = data[2];
head_tuple[1] = data[3];
        return {data, output + 2};
      };
      auto [_,new_ptr] = builtin_eq<TState>(args_for_old_bi.data(), state, callback);
      auto tuples_count = (new_ptr - output) / 2;
      return tuples_count;
    }));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body11__2__1, rel___dollorinter__body11__2__1__2, DELTA, {0, 2, 1}));
scc5681->add_rule(new parallel_copy(rel__A__2__1__2, rel___dollorinter__head29__3__1__2__3, DELTA, {1, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body65__3__1__2__3, rel___dollorinter__body64__1__1, FULL, rel__ifk__4__0, DELTA, {2, 4, 5}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified20__4__1__2__3__4, rel___dollorlst__2__2__1, DELTA, rel___dollorinter__head15__5__1__2, FULL, {4, 5, 6, 2}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified18__4__1__2__3__4, rel___dollorlst__2__1__2, FULL, rel___dollorinter__head14__5__3__1, DELTA, {4, 5, 6, 2}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified31__2__1__2, rel__boolv__1__1, DELTA, rel___dollorinter__body70__2__2, FULL, {3, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head6__4__1__2__3__4, rel___dollorinter__body17__2__2, DELTA, rel__ifk__4__0, DELTA, {3, 2, 5, 6}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head20__3__1, rel___dollorinter__head20__3__1__2__3, DELTA, {0, 3, 1, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body5__7__1__2__3__4__5__6__7, rel__copy__ctx__3__3__2__1, FULL, rel___dollorinter__body4__7__6__2__7, DELTA, {5, 1, 6, 7, 8, 0, 2}));
scc5681->add_rule(new parallel_acopy(rel__closure__2__0, rel__closure__2__1__2, DELTA, {2, 0, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body__4__1__2__3__4, rel__callcck__2__2__1, DELTA, rel___dollorinter__head__5__1__2, FULL, {4, 2, 5, 6}));
scc5681->add_rule(new parallel_join(rel__peek__ctx__3__1__2__3, rel___dollorlst__2__2__1, FULL, rel___dollorhead__stratified26__3__3__1, DELTA, {1, 4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body63__2__1__2, rel__A__2__2, DELTA, rel__store__2__1, FULL, {2, 4}));
scc5681->add_rule(new parallel_copy(rel__store__2__1__2, rel___dollorhead__stratified45__2__1__2, DELTA, {0, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body7__4__1__2__3__4, rel__E__3__2__1, FULL, rel__peek__ctx__3__2__1, DELTA, {3, 5, 1, 0}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified45__2__1__2, rel__setk__1__1, DELTA, rel___dollorinter__body88__2__2, DELTA, {3, 1}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified4__4__1__2__3__4, rel__kaddr__2__2__1, FULL, rel___dollorinter__body9__3__3__2, DELTA, {1, 0, 2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head13__6__1__2__3__4__5__6, rel___dollorinter__body37__2__1, DELTA, rel___dollorinter__body38__6__2, FULL, {4, 5, 2, 6, 7, 8}));
scc5681->add_rule(new parallel_copy(rel__flow__ae__2__1__2, rel___dollorinter__head5__4__1__2__3__4, DELTA, {1, 0}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body13__3__2, rel___dollorinter__body13__3__1__2__3, DELTA, {1, 3, 0, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body86__4__1__2__3__4, rel__vaddr__2__2, DELTA, rel__copy__ctx__3__1, DELTA, {2, 1, 4, 5}));
scc5681->add_rule(new parallel_copy(rel__A__2__1__2, rel___dollorinter__head26__3__1__2__3, DELTA, {2, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body16__3__1__2__3, rel__boolv__1__0, DELTA, rel___dollorinter__body15__2__1, DELTA, {1, 0, 3}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified2__7__1__2__3__4__5__6__7, rel__kaddr__2__2__1, DELTA, rel___dollorinter__body6__6__6__3, FULL, {4, 5, 1, 6, 0, 2, 7}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body11__2__1__2, rel__store__2__1, DELTA, rel__A__2__2, FULL, {2, 4}));
scc5681->add_rule(new parallel_copy(rel___dollorlst__2__1__2, rel___dollorhead__stratified42__4__1__2__3__4, DELTA, {1, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head19__5__1__2__3__4__5, rel___dollorinter__body52__2__2, DELTA, rel___dollorinter__body55__5__1, DELTA, {2, 4, 5, 6, 7}));
scc5681->add_rule(new parallel_acopy(rel___dollorhead__stratified43__3__3__2, rel___dollorhead__stratified43__3__1__2__3, DELTA, {2, 1, 3, 0}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head2__6__6__5__2__1, rel___dollorinter__head2__6__1__2__3__4__5__6, DELTA, {5, 4, 1, 0, 6, 2, 3}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified__4__1__2__3__4, rel__kaddr__2__2__1, FULL, rel___dollorinter__body1__3__3__1, DELTA, {1, 0, 4, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body65__3__, rel___dollorinter__body65__3__1__2__3, DELTA, {3, 0, 1, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body9__3__1__2__3, rel__flow__ee__2__2__1, FULL, rel___dollorinter__body8__4__3__2, DELTA, {4, 0, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body12__2__1__2, rel__A__2__2, FULL, rel__store__2__1, DELTA, {2, 4}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified28__2__1__2, rel__boolv__1__1, DELTA, rel___dollorinter__body62__2__2, DELTA, {3, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body85__4__1__2__3__4, rel___dollorlst__2__2, FULL, rel___dollorinter__body84__3__1, DELTA, {1, 4, 5, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head29__3__1__2__3, rel__number__1__1, DELTA, rel___dollorinter__head28__3__2, DELTA, {3, 1, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head28__3__1__2__3, rel__num__2__1, FULL, rel__E__3__1, DELTA, {5, 2, 0}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head12__4__4__3__1, rel___dollorinter__head12__4__1__2__3__4, DELTA, {3, 2, 0, 4, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body54__4__1__2__3__4, rel___dollorinter__body53__2__2, DELTA, rel___dollorlst__2__0, DELTA, {3, 2, 0, 4}));
scc5681->add_rule(new parallel_copy(rel__closure__2__1__2, rel___dollorinter__head10__3__1__2__3, DELTA, {1, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body74__4__1__2__3__4, rel__vaddr__2__2, DELTA, rel__E__3__2, FULL, {5, 1, 4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body43__3__1__2__3, rel___dollorlst__2__2, DELTA, rel___dollorlst__2__0, DELTA, {4, 2, 1}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body46__4__4, rel___dollorinter__body46__4__1__2__3__4, DELTA, {3, 4, 0, 1, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head15__5__1__2__3__4__5, rel___dollorinter__body44__2__2, DELTA, rel___dollorinter__body46__4__4, FULL, {4, 5, 6, 2, 0}));
scc5681->add_rule(new parallel_join(rel__peek__ctx__3__1__2__3, rel___dollorlst__2__2__1, DELTA, rel___dollorhead__stratified43__3__3__2, DELTA, {1, 4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body14__5__1__2__3__4__5, rel___dollorinter__body13__3__2, DELTA, rel__ifk__4__0, DELTA, {4, 2, 6, 7, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body23__3__1__2__3, rel__vaddr__2__1__2, FULL, rel___dollorinter__body22__4__3__1, DELTA, {2, 4, 5}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body19__3__3__1, rel___dollorinter__body19__3__1__2__3, DELTA, {2, 0, 3, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body73__3__1__2__3, rel___dollorinter__body72__1__1, FULL, rel__ifk__4__0, DELTA, {2, 4, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body85__4__1__2__3__4, rel___dollorlst__2__2, DELTA, rel___dollorinter__body84__3__1, DELTA, {1, 4, 5, 2}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified28__2__1__2, rel__boolv__1__1, DELTA, rel___dollorinter__body62__2__2, FULL, {3, 1}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified26__3__1__2__3, rel___dollorlst__2__2__1, DELTA, rel___dollorhead__stratified25__4__4__1, DELTA, {4, 5, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body14__5__1__2__3__4__5, rel___dollorinter__body13__3__2, FULL, rel__ifk__4__0, DELTA, {4, 2, 6, 7, 3}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified21__3__1__2__3, rel___dollorlst__2__2__1, FULL, rel___dollorhead__stratified20__4__4__1, DELTA, {4, 5, 2}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified2__7__1__2__3__4__5__6__7, rel__kaddr__2__2__1, FULL, rel___dollorinter__body6__6__6__3, DELTA, {4, 5, 1, 6, 0, 2, 7}));
scc5681->add_rule(new parallel_copy(rel__E__3__1__2__3, rel___dollorhead__stratified4__4__1__2__3__4, DELTA, {0, 1, 2}));
scc5681->add_rule(new parallel_acopy(rel__A__2__1, rel__A__2__1__2, DELTA, {0, 2, 1}));
scc5681->add_rule(new parallel_acopy(rel__prim1__4__3__2__1__4, rel__prim1__4__1__2__3__4, DELTA, {2, 1, 0, 3, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head10__3__1__2__3, rel__E__3__1, DELTA, rel__lambda__3__1, FULL, {3, 0, 2}));
scc5681->add_rule(new parallel_copy(rel__vaddr__2__1__2, rel___dollorinter__head1__7__1__2__3__4__5__6__7, DELTA, {5, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head24__2__1__2, rel___dollorinter__body68__2__1, FULL, rel___dollorinter__body69__2__1, DELTA, {2, 4}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body57__3__2, rel___dollorinter__body57__3__1__2__3, DELTA, {1, 3, 0, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body77__3__1__2__3, rel__flow__ee__2__1__2, FULL, rel___dollorinter__body76__4__3__2, DELTA, {4, 1, 5}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body38__6__2, rel___dollorinter__body38__6__1__2__3__4__5__6, DELTA, {1, 6, 0, 2, 3, 4, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head14__5__1__2__3__4__5, rel___dollornil__0__0, FULL, rel___dollorinter__body42__5__1, DELTA, {0, 2, 3, 4, 5}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body84__3__1, rel___dollorinter__body84__3__1__2__3, DELTA, {0, 3, 1, 2}));
scc5681->add_rule(new parallel_acopy(rel__store__2__2__1, rel__store__2__1__2, DELTA, {1, 0, 2}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified23__4__1__2__3__4, rel__kaddr__2__2__1, DELTA, rel___dollorinter__body48__3__3__1, DELTA, {1, 0, 4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body66__2__1__2, rel__A__2__2, DELTA, rel__store__2__1, FULL, {4, 2}));
scc5681->add_rule(new parallel_acopy(rel__peek__ctx__3__2__3__1, rel__peek__ctx__3__1__2__3, DELTA, {1, 2, 0, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body58__2__1__2, rel__callcc__2__0, FULL, rel___dollorinter__body57__3__2, DELTA, {4, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body57__3__1__2__3, rel__A__2__2, FULL, rel__store__2__1, DELTA, {0, 4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body67__3__1__2__3, rel__kont__1__0, FULL, rel__fn__4__1, DELTA, {2, 1, 3}));
scc5681->add_rule(new parallel_copy(rel___dollorlst__2__1__2, rel___dollorhead__stratified26__3__1__2__3, DELTA, {0, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head3__4__4__2__1, rel___dollorinter__head3__4__1__2__3__4, DELTA, {3, 1, 0, 4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body77__3__1__2__3, rel__flow__ee__2__1__2, DELTA, rel___dollorinter__body76__4__3__2, DELTA, {4, 1, 5}));
scc5681->add_rule(new parallel_copy(rel__flow__ee__2__1__2, rel___dollorinter__head__5__1__2__3__4__5, DELTA, {3, 2}));
scc5681->add_rule(new parallel_copy(rel__kaddr__2__1__2, rel___dollorhead__stratified38__4__1__2__3__4, DELTA, {0, 3}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body70__2__2, rel___dollorinter__body70__2__1__2, DELTA, {1, 2, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body85__4__1__2__3__4, rel___dollorlst__2__2, DELTA, rel___dollorinter__body84__3__1, FULL, {1, 4, 5, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body6__6__1__2__3__4__5__6, rel__flow__ee__2__2__1, DELTA, rel___dollorinter__body5__7__3__6, DELTA, {4, 5, 0, 6, 7, 8}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body47__4__1__2__3__4, rel__prim2__3__3__2__1, DELTA, rel___dollorinter__head18__5__5__3__1, DELTA, {5, 3, 1, 6}));
scc5681->add_rule(new parallel_acopy(rel___dollorhead__stratified44__4__3__2__1, rel___dollorhead__stratified44__4__1__2__3__4, DELTA, {2, 1, 0, 4, 3}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body15__2__1, rel___dollorinter__body15__2__1__2, DELTA, {0, 2, 1}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified28__2__1__2, rel__boolv__1__1, FULL, rel___dollorinter__body62__2__2, DELTA, {3, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body48__3__1__2__3, rel__flow__ae__2__1__2, DELTA, rel___dollorinter__body47__4__3__1, DELTA, {1, 4, 5}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body82__3__2, rel___dollorinter__body82__3__1__2__3, DELTA, {1, 3, 0, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body46__4__1__2__3__4, rel___dollorlst__2__2, DELTA, rel___dollorinter__body45__3__3, FULL, {4, 5, 2, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body62__2__1__2, rel__E__3__3__2__1, DELTA, rel___dollorinter__head22__4__4__3__1, FULL, {2, 5}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head27__6__6__3__2__1, rel___dollorinter__head27__6__1__2__3__4__5__6, DELTA, {5, 2, 1, 0, 6, 3, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body79__4__1__2__3__4, rel___dollorlst__2__2__1, FULL, rel___dollorhead__stratified37__4__4__2, DELTA, {2, 5, 0, 4}));
scc5681->add_rule(new parallel_acopy(rel___dollorlst__2__2, rel___dollorlst__2__1__2, DELTA, {1, 2, 0}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified25__4__1__2__3__4, rel___dollorlst__2__2__1, DELTA, rel___dollorinter__head19__5__1__2, DELTA, {4, 5, 6, 2}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified19__3__1__2__3, rel___dollorlst__2__2__1, FULL, rel___dollorhead__stratified18__4__4__2, DELTA, {4, 5, 2}));
scc5681->add_rule(new parallel_copy(rel__A__2__1__2, rel___dollorinter__head17__3__1__2__3, DELTA, {0, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body24__6__1__2__3__4__5__6, rel__closure__2__0, FULL, rel__fn__4__1, DELTA, {4, 3, 5, 1, 2, 6}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body35__4__1__2__3__4, rel__fn__4__4__3__2__1, FULL, rel___dollorinter__head13__6__5__2__1__3, DELTA, {4, 3, 6, 7}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body2__4__1__2__3__4, rel__peek__ctx__3__2__1, FULL, rel__E__3__2__1, DELTA, {5, 3, 1, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body12__2__1__2, rel__A__2__2, DELTA, rel__store__2__1, FULL, {2, 4}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body28__4__1, rel___dollorinter__body28__4__1__2__3__4, DELTA, {0, 4, 1, 2, 3}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body29__2__1__2, rel__A__2__2, DELTA, rel__store__2__1, FULL, {4, 2}));
scc5681->add_rule(new parallel_copy(rel__flow__ae__2__1__2, rel___dollorinter__head23__5__1__2__3__4__5, DELTA, {2, 0}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body87__3__2, rel___dollorinter__body87__3__1__2__3, DELTA, {1, 3, 0, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__head__5__1__2, rel___dollorinter__head__5__1__2__3__4__5, DELTA, {0, 1, 5, 2, 3, 4}));
scc5681->add_rule(new parallel_copy(rel___dollorlst__2__1__2, rel___dollorinter__head30__5__1__2__3__4__5, DELTA, {2, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body28__4__1__2__3__4, rel__number__1__1, DELTA, rel___dollorinter__body27__3__1, DELTA, {0, 1, 3, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body36__3__1__2__3, rel__flow__ae__2__2__1, FULL, rel___dollorinter__body35__4__4__2, DELTA, {4, 5, 0}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body89__3__3__2, rel___dollorinter__body89__3__1__2__3, DELTA, {2, 1, 3, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body16__3__1__2__3, rel__boolv__1__0, DELTA, rel___dollorinter__body15__2__1, FULL, {1, 0, 3}));
scc5681->add_rule(new parallel_acopy(rel__fn__4__1, rel__fn__4__1__2__3__4, DELTA, {0, 4, 1, 2, 3}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified__4__1__2__3__4, rel__kaddr__2__2__1, DELTA, rel___dollorinter__body1__3__3__1, DELTA, {1, 0, 4, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body57__3__1__2__3, rel__A__2__2, DELTA, rel__store__2__1, DELTA, {0, 4, 2}));
scc5681->add_rule(new parallel_acopy(rel__letk__4__0, rel__letk__4__1__2__3__4, DELTA, {4, 0, 1, 2, 3}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified43__3__1__2__3, rel___dollorlst__2__2__1, DELTA, rel___dollorhead__stratified42__4__4__2, DELTA, {4, 5, 2}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified31__2__1__2, rel__boolv__1__1, FULL, rel___dollorinter__body70__2__2, DELTA, {3, 1}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body35__4__1__2__3__4, rel__fn__4__4__3__2__1, DELTA, rel___dollorinter__head13__6__5__2__1__3, FULL, {4, 3, 6, 7}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body20__7__1__2__3__4__5__6__7, rel__here__5__5__4__3__2__1, FULL, rel___dollorinter__head8__8__2__7__6__1__4, DELTA, {0, 7, 4, 8, 2, 1, 9}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body78__3__1__2__3, rel__peek__ctx__3__2__3__1, DELTA, rel___dollorhead__stratified38__4__4__3__1, FULL, {0, 5, 2}));
scc5681->add_rule(new parallel_acopy(rel___dollorinter__body86__4__4__1, rel___dollorinter__body86__4__1__2__3__4, DELTA, {3, 0, 4, 1, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body86__4__1__2__3__4, rel__vaddr__2__2, DELTA, rel__copy__ctx__3__1, FULL, {2, 1, 4, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body82__3__1__2__3, rel___dollorlst__2__0, DELTA, rel___dollorlst__2__2, FULL, {3, 2, 4}));
scc5681->add_rule(new parallel_acopy(rel__here__5__5__4__3__2__1, rel__here__5__1__2__3__4__5, DELTA, {4, 3, 2, 1, 0, 5}));
scc5681->add_rule(new parallel_acopy(rel__peek__ctx__3__2__1, rel__peek__ctx__3__1__2__3, DELTA, {1, 0, 3, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body39__4__1__2__3__4, rel___dollorlst__2__0, FULL, rel___dollorlst__2__2, DELTA, {3, 2, 1, 4}));
scc5681->add_rule(new parallel_join(rel___dollorinter__head23__5__1__2__3__4__5, rel___dollorinter__body66__2__1, FULL, rel__letk__4__0, DELTA, {4, 3, 2, 5, 6}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body42__5__1__2__3__4__5, rel___dollorinter__body41__2__1, FULL, rel___dollorinter__body40__4__2, DELTA, {4, 0, 5, 6, 2}));
scc5681->add_rule(new parallel_copy(rel__setk__1__1, rel___dollorhead__stratified44__4__1__2__3__4, DELTA, {3}));
scc5681->add_rule(new parallel_join(rel___dollorhead__stratified45__2__1__2, rel__setk__1__1, DELTA, rel___dollorinter__body88__2__2, FULL, {3, 1}));
scc5681->add_rule(new parallel_join(rel__store__2__1__2, rel__vaddr__2__2__1, DELTA, rel___dollorinter__head31__3__2__1, DELTA, {2, 4}));
scc5681->add_rule(new parallel_copy(rel__A__2__1__2, rel___dollorinter__head24__2__1__2, DELTA, {1, 0}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body32__4__1__2__3__4, rel__store__2__2, DELTA, rel__ifk__4__0, FULL, {3, 2, 5, 6}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body5__7__1__2__3__4__5__6__7, rel__copy__ctx__3__3__2__1, DELTA, rel___dollorinter__body4__7__6__2__7, FULL, {5, 1, 6, 7, 8, 0, 2}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body23__3__1__2__3, rel__vaddr__2__1__2, DELTA, rel___dollorinter__body22__4__3__1, DELTA, {2, 4, 5}));
scc5681->add_rule(new parallel_join(rel___dollorinter__body62__2__1__2, rel__E__3__3__2__1, DELTA, rel___dollorinter__head22__4__4__3__1, DELTA, {2, 5}));
scc5681->add_rule(new parallel_copy(rel__callcck__2__1__2, rel___dollorinter__head__5__1__2__3__4__5, DELTA, {1, 0}));
scc5681->add_rule(new parallel_copy(rel___dollorlst__2__1__2, rel___dollorhead__stratified25__4__1__2__3__4, DELTA, {0, 3}));
scc5681->add_rule(new parallel_acopy(rel__flow__ee__2__2__1, rel__flow__ee__2__1__2, DELTA, {1, 0, 2}));

RAM* scc5682 = new RAM(false, 110);
scc5682->add_relation(rel__var__2__1__2, true);
scc5682->add_rule(new fact(rel__var__2__1__2, {n2d(524381), n2d(524367)}));

RAM* scc5683 = new RAM(false, 177);
scc5683->add_relation(rel__let__3__3, true);
scc5683->add_relation(rel__let__3__1__2__3, true);
scc5683->add_rule(new parallel_acopy(rel__let__3__3, rel__let__3__1__2__3, DELTA, {2, 3, 0, 1}));

RAM* scc5684 = new RAM(false, 224);
scc5684->add_relation(rel__num__2__1__2, true);
scc5684->add_rule(new fact(rel__num__2__1__2, {n2d(524322), n2d(13)}));

RAM* scc5685 = new RAM(false, 185);
scc5685->add_relation(rel__bool__2__1__2, true);
scc5685->add_relation(rel__bool__2__1, true);
scc5685->add_rule(new parallel_acopy(rel__bool__2__1, rel__bool__2__1__2, DELTA, {0, 2, 1}));

RAM* scc5686 = new RAM(false, 189);
scc5686->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5686->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524284), n2d(0), n2d(524292)}));

RAM* scc5687 = new RAM(false, 51);
scc5687->add_relation(rel__call__3__1__2__3, true);
scc5687->add_rule(new fact(rel__call__3__1__2__3, {n2d(524458), n2d(524372), n2d(524303)}));

RAM* scc5688 = new RAM(false, 55);
scc5688->add_relation(rel___dollorbir__sub2__2__1__2, true);
scc5688->add_rule(new parallel_copy(rel___dollorbir__sub2__2__1__2, rel__callcc__2__1__2, FULL, {0, 1}));

RAM* scc5689 = new RAM(false, 59);
scc5689->add_relation(rel__var__2__1__2, true);
scc5689->add_rule(new fact(rel__var__2__1__2, {n2d(524391), n2d(524367)}));

RAM* scc5690 = new RAM(false, 63);
scc5690->add_relation(rel__var__2__1__2, true);
scc5690->add_rule(new fact(rel__var__2__1__2, {n2d(524385), n2d(524367)}));

RAM* scc5691 = new RAM(false, 121);
scc5691->add_relation(rel__let__list__3__1__2__3, true);
scc5691->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524447), n2d(524448)}));

RAM* scc5692 = new RAM(false, 138);
scc5692->add_relation(rel__num__2__1__2, true);
scc5692->add_rule(new fact(rel__num__2__1__2, {n2d(524300), n2d(2)}));

RAM* scc5693 = new RAM(false, 142);
scc5693->add_relation(rel__var__2__1__2, true);
scc5693->add_rule(new fact(rel__var__2__1__2, {n2d(524393), n2d(524367)}));

RAM* scc5694 = new RAM(false, 65);
scc5694->add_relation(rel__num__2__1__2, true);
scc5694->add_rule(new fact(rel__num__2__1__2, {n2d(524306), n2d(5)}));

RAM* scc5695 = new RAM(false, 4);
scc5695->add_relation(rel__var__2__1__2, true);
scc5695->add_rule(new fact(rel__var__2__1__2, {n2d(524386), n2d(524367)}));

RAM* scc5696 = new RAM(false, 8);
scc5696->add_relation(rel__flow__aa__2__1__2, true);
scc5696->add_rule(new parallel_copy(rel__flow__aa__2__1__2, rel___dollorinter__head21__3__1__2__3, FULL, {2, 0}));

RAM* scc5697 = new RAM(false, 12);
scc5697->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5697->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524337), n2d(0), n2d(524338)}));

RAM* scc5698 = new RAM(false, 146);
scc5698->add_relation(rel___dollorbir__sub2__2__1__2, true);
scc5698->add_relation(rel___dollorbir__sub2__2__1, true);
scc5698->add_rule(new parallel_acopy(rel___dollorbir__sub2__2__1, rel___dollorbir__sub2__2__1__2, DELTA, {0, 2, 1}));

RAM* scc5699 = new RAM(false, 195);
scc5699->add_relation(rel__call__3__1__2__3, true);
scc5699->add_rule(new fact(rel__call__3__1__2__3, {n2d(524430), n2d(524386), n2d(524331)}));

RAM* scc5700 = new RAM(false, 134);
scc5700->add_relation(rel__var__2__1__2, true);
scc5700->add_rule(new fact(rel__var__2__1__2, {n2d(524384), n2d(524367)}));

RAM* scc5701 = new RAM(false, 215);
scc5701->add_relation(rel___dollorbir__sub5__4__1, true);
scc5701->add_relation(rel___dollorbir__sub5__4__1__2__3__4, true);
scc5701->add_rule(new parallel_acopy(rel___dollorbir__sub5__4__1, rel___dollorbir__sub5__4__1__2__3__4, DELTA, {0, 4, 1, 2, 3}));

RAM* scc5702 = new RAM(false, 219);
scc5702->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5702->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524327), n2d(0), n2d(524328)}));

RAM* scc5703 = new RAM(false, 223);
scc5703->add_relation(rel__let__list__3__1__2__3, true);
scc5703->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524423), n2d(524424)}));

RAM* scc5704 = new RAM(false, 16);
scc5704->add_relation(rel___dollorhead__stratified4__4__, true);
scc5704->add_relation(rel___dollorhead__stratified4__4__1__2__3__4, true);
scc5704->add_rule(new parallel_acopy(rel___dollorhead__stratified4__4__, rel___dollorhead__stratified4__4__1__2__3__4, DELTA, {4, 0, 1, 2, 3}));

RAM* scc5705 = new RAM(false, 85);
scc5705->add_relation(rel__callcc__2__1, true);
scc5705->add_relation(rel__callcc__2__1__2, true);
scc5705->add_rule(new parallel_acopy(rel__callcc__2__1, rel__callcc__2__1__2, DELTA, {0, 2, 1}));

RAM* scc5706 = new RAM(false, 89);
scc5706->add_relation(rel__if__4__1__2__3__4, true);
scc5706->add_relation(rel__if__4__4, true);
scc5706->add_rule(new parallel_acopy(rel__if__4__4, rel__if__4__1__2__3__4, DELTA, {3, 4, 0, 1, 2}));

RAM* scc5707 = new RAM(false, 93);
scc5707->add_relation(rel___dollorhead__stratified45__2__, true);
scc5707->add_relation(rel___dollorhead__stratified45__2__1__2, true);
scc5707->add_rule(new parallel_acopy(rel___dollorhead__stratified45__2__, rel___dollorhead__stratified45__2__1__2, DELTA, {2, 0, 1}));

RAM* scc5708 = new RAM(false, 97);
scc5708->add_relation(rel__call__3__1__2__3, true);
scc5708->add_rule(new fact(rel__call__3__1__2__3, {n2d(524422), n2d(524390), n2d(524339)}));

RAM* scc5709 = new RAM(false, 36);
scc5709->add_relation(rel__num__2__1__2, true);
scc5709->add_rule(new fact(rel__num__2__1__2, {n2d(524330), n2d(17)}));

RAM* scc5710 = new RAM(false, 40);
scc5710->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5710->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524307), n2d(0), n2d(524308)}));

RAM* scc5711 = new RAM(false, 227);
scc5711->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5711->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524335), n2d(0), n2d(524336)}));

RAM* scc5712 = new RAM(false, 166);
scc5712->add_relation(rel__let__list__3__1__2__3, true);
scc5712->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524437), n2d(524438)}));

RAM* scc5713 = new RAM(false, 170);
scc5713->add_relation(rel__callcc__2__1__2, true);
scc5713->add_relation(rel__callcc__2__0, true);
scc5713->add_rule(new parallel_acopy(rel__callcc__2__0, rel__callcc__2__1__2, DELTA, {2, 0, 1}));

RAM* scc5714 = new RAM(false, 174);
scc5714->add_relation(rel__prim__call__3__1__2__3, true);
scc5714->add_rule(new fact(rel__prim__call__3__1__2__3, {n2d(524287), n2d(524360), n2d(524288)}));

RAM* scc5715 = new RAM(false, 48);
scc5715->add_relation(rel__let__list__3__1__2__3, true);
scc5715->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524435), n2d(524436)}));

RAM* scc5716 = new RAM(false, 117);
scc5716->add_relation(rel__num__2__1__2, true);
scc5716->add_rule(new fact(rel__num__2__1__2, {n2d(524308), n2d(6)}));

RAM* scc5717 = new RAM(false, 44);
scc5717->add_relation(rel__num__2__1__2, true);
scc5717->add_rule(new fact(rel__num__2__1__2, {n2d(524336), n2d(20)}));

RAM* scc5718 = new RAM(false, 125);
scc5718->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5718->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524315), n2d(0), n2d(524316)}));

RAM* scc5719 = new RAM(false, 178);
scc5719->add_relation(rel__var__2__1__2, true);
scc5719->add_rule(new fact(rel__var__2__1__2, {n2d(524371), n2d(524367)}));

RAM* scc5720 = new RAM(false, 247);
scc5720->add_relation(rel__call__3__1__2__3, true);
scc5720->add_rule(new fact(rel__call__3__1__2__3, {n2d(524418), n2d(524392), n2d(524343)}));

RAM* scc5721 = new RAM(false, 251);
scc5721->add_relation(rel__lambda__3__1__2__3, true);
scc5721->add_rule(new fact(rel__lambda__3__1__2__3, {n2d(524283), n2d(524400), n2d(524361)}));

RAM* scc5722 = new RAM(false, 255);
scc5722->add_relation(rel__call__3__1__2__3, true);
scc5722->add_rule(new fact(rel__call__3__1__2__3, {n2d(524412), n2d(524395), n2d(524349)}));

RAM* scc5723 = new RAM(false, 277);
scc5723->add_relation(rel__let__list__3__1__2__3, true);
scc5723->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524433), n2d(524434)}));

RAM* scc5724 = new RAM(false, 273);
scc5724->add_relation(rel__call__3__1__2__3, true);
scc5724->add_rule(new fact(rel__call__3__1__2__3, {n2d(524428), n2d(524387), n2d(524333)}));

RAM* scc5725 = new RAM(false, 285);
scc5725->add_relation(rel__let__list__3__1__2__3, true);
scc5725->add_rule(new fact(rel__let__list__3__1__2__3, {n2d(524404), n2d(524451), n2d(524452)}));

RAM* scc5726 = new RAM(false, 281);
scc5726->add_relation(rel__call__3__1__2__3, true);
scc5726->add_rule(new fact(rel__call__3__1__2__3, {n2d(524466), n2d(524368), n2d(524295)}));

RAM* scc5727 = new RAM(false, 262);
scc5727->add_relation(rel__if__4__1__2__3__4, true);
scc5727->add_relation(rel__if__4__3, true);
scc5727->add_rule(new parallel_acopy(rel__if__4__3, rel__if__4__1__2__3__4, DELTA, {2, 4, 0, 1, 3}));

RAM* scc5728 = new RAM(false, 258);
scc5728->add_relation(rel___dollorinter__head6__4__, true);
scc5728->add_relation(rel___dollorinter__head6__4__1__2__3__4, true);
scc5728->add_rule(new parallel_acopy(rel___dollorinter__head6__4__, rel___dollorinter__head6__4__1__2__3__4, DELTA, {4, 0, 1, 2, 3}));

RAM* scc5729 = new RAM(false, 266);
scc5729->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5729->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524353), n2d(0), n2d(524354)}));

RAM* scc5730 = new RAM(false, 270);
scc5730->add_relation(rel__call__arg__list__3__1__2__3, true);
scc5730->add_rule(new fact(rel__call__arg__list__3__1__2__3, {n2d(524319), n2d(0), n2d(524320)}));

RAM* scc5731 = new RAM(false, 279);
scc5731->add_relation(rel___dollorhead__stratified11__2__1__2, true);
scc5731->add_relation(rel___dollorhead__stratified11__2__, true);
scc5731->add_rule(new parallel_acopy(rel___dollorhead__stratified11__2__, rel___dollorhead__stratified11__2__1__2, DELTA, {2, 0, 1}));

RAM* scc5732 = new RAM(false, 275);
scc5732->add_relation(rel__var__2__1__2, true);
scc5732->add_rule(new fact(rel__var__2__1__2, {n2d(524373), n2d(524367)}));

RAM* scc5733 = new RAM(false, 283);
scc5733->add_relation(rel__call__3__1__2__3, true);
scc5733->add_rule(new fact(rel__call__3__1__2__3, {n2d(524452), n2d(524375), n2d(524309)}));

RAM* scc5734 = new RAM(false, 276);
scc5734->add_relation(rel___dollorinter__body59__2__2, true);
scc5734->add_relation(rel___dollorinter__body59__2__1__2, true);
scc5734->add_rule(new parallel_acopy(rel___dollorinter__body59__2__2, rel___dollorinter__body59__2__1__2, DELTA, {1, 2, 0}));

RAM* scc5735 = new RAM(false, 256);
scc5735->add_relation(rel__num__2__1__2, true);
scc5735->add_rule(new fact(rel__num__2__1__2, {n2d(524348), n2d(26)}));

RAM* scc5736 = new RAM(false, 268);
scc5736->add_relation(rel__call__3__1__2__3, true);
scc5736->add_rule(new fact(rel__call__3__1__2__3, {n2d(524414), n2d(524394), n2d(524347)}));

RAM* scc5737 = new RAM(false, 264);
scc5737->add_relation(rel___dollorhead__stratified39__4__1__2__3__4, true);
scc5737->add_relation(rel___dollorhead__stratified39__4__, true);
scc5737->add_rule(new parallel_acopy(rel___dollorhead__stratified39__4__, rel___dollorhead__stratified39__4__1__2__3__4, DELTA, {4, 0, 1, 2, 3}));

LIE* lie = new LIE();
lie->add_relation(rel___dollorinter__body52__2__1__2);
lie->add_relation(rel___dollorinter__body55__5__1__2__3__4__5);
lie->add_relation(rel___dollorhead__stratified44__4__3__2__1);
lie->add_relation(rel___dollorinter__body38__6__2);
lie->add_relation(rel___dollorhead__stratified42__4__1__2__3__4);
lie->add_relation(rel___dollorhead__stratified16__4__1__2__3__4);
lie->add_relation(rel___dollorinter__head21__3__);
lie->add_relation(rel___dollorinter__body17__2__2);
lie->add_relation(rel___dollorinter__body44__2__2);
lie->add_relation(rel___dollorinter__body45__3__3);
lie->add_relation(rel___dollorhead__stratified12__4__);
lie->add_relation(rel___dollorinter__body31__2__1__2);
lie->add_relation(rel__lambda__arg__list__3__1);
lie->add_relation(rel___dollorinter__body7__4__1__2__3__4);
lie->add_relation(rel___dollorinter__head22__4__4__3__1);
lie->add_relation(rel__if__4__1);
lie->add_relation(rel___dollorhead__stratified20__4__1__2__3__4);
lie->add_relation(rel__E__3__1__2__3);
lie->add_relation(rel___dollorinter__body6__6__1__2__3__4__5__6);
lie->add_relation(rel__callcck__2__2__1);
lie->add_relation(rel__call__3__1);
lie->add_relation(rel___dollorhead__stratified36__4__1__2__3__4);
lie->add_relation(rel___dollorinter__body49__5__1);
lie->add_relation(rel___dollorinter__head21__3__1__2__3);
lie->add_relation(rel__prim1__4__0);
lie->add_relation(rel___dollorinter__body49__5__1__2__3__4__5);
lie->add_relation(rel___dollorhead__stratified11__2__1__2);
lie->add_relation(rel___dollornil__0__0);
lie->add_relation(rel___dollorhead__stratified37__4__4__2);
lie->add_relation(rel__vaddr__2__2);
lie->add_relation(rel___dollorinter__body11__2__1__2);
lie->add_relation(rel___dollorinter__body47__4__3__1);
lie->add_relation(rel___dollorinter__body38__6__1__2__3__4__5__6);
lie->add_relation(rel___dollorinter__body40__4__1__2__3__4);
lie->add_relation(rel__kont__1__0);
lie->add_relation(rel___dollorhead__stratified18__4__4__2);
lie->add_relation(rel___dollorhead__stratified26__3__3__1);
lie->add_relation(rel__ifk__4__1__2__3__4);
lie->add_relation(rel___dollorinter__body53__2__2);
lie->add_relation(rel__if__4__1__2__3__4);
lie->add_relation(rel___dollornil__0__);
lie->add_relation(rel__store__2__1);
lie->add_relation(rel___dollorhead__stratified25__4__1__2__3__4);
lie->add_relation(rel__if__4__2);
lie->add_relation(rel___dollorinter__body90__3__2__1);
lie->add_relation(rel__peek__ctx__3__2__1);
lie->add_relation(rel___dollorlst__2__1__2);
lie->add_relation(rel___dollorinter__body15__2__1__2);
lie->add_relation(rel___dollorhead__stratified16__4__);
lie->add_relation(rel___dollorinter__body28__4__1);
lie->add_relation(rel___dollorinter__body39__4__2);
lie->add_relation(rel___dollorinter__body12__2__1__2);
lie->add_relation(rel___dollorbir__sub6__3__1__2__3);
lie->add_relation(rel___dollorinter__body30__3__);
lie->add_relation(rel___dollorbir__sub5__4__1);
lie->add_relation(rel___dollorbir__sub2__2__1__2);
lie->add_relation(rel___dollorinter__body24__6__1__2__3__4__5__6);
lie->add_relation(rel___dollorinter__body32__4__2);
lie->add_relation(rel__store__2__2);
lie->add_relation(rel___dollorhead__stratified33__4__);
lie->add_relation(rel___dollorinter__body42__5__1);
lie->add_relation(rel___dollorinter__body87__3__2);
lie->add_relation(rel___dollorinter__head17__3__1__2__3);
lie->add_relation(rel___dollorinter__body59__2__2);
lie->add_relation(rel___dollorinter__body14__5__1__2__3__4__5);
lie->add_relation(rel___dollorinter__head11__3__1__2__3);
lie->add_relation(rel__prim2__3__1__2__3);
lie->add_relation(rel___dollorinter__body34__3__1__2__3);
lie->add_relation(rel__primval__3__3__2__1);
lie->add_relation(rel___dollorhead__stratified15__2__1__2);
lie->add_relation(rel___dollorinter__head15__5__1__2__3__4__5);
lie->add_relation(rel___dollorinter__body54__4__4);
lie->add_relation(rel__number__1__0);
lie->add_relation(rel___dollorinter__head30__5__3__2);
lie->add_relation(rel___dollorhead__stratified43__3__3__2);
lie->add_relation(rel__call__arg__list__3__3);
lie->add_relation(rel___dollorinter__head19__5__1__2__3__4__5);
lie->add_relation(rel___dollorinter__body66__2__1);
lie->add_relation(rel___dollorinter__body82__3__2);
lie->add_relation(rel___dollorinter__body51__3__1__3);
lie->add_relation(rel___dollorinter__head6__4__);
lie->add_relation(rel___dollorinter__head29__3__);
lie->add_relation(rel___dollorhead__stratified19__3__1__2__3);
lie->add_relation(rel__prim2__3__3__2__1);
lie->add_relation(rel__ifk__4__0);
lie->add_relation(rel___dollorinter__head__5__1__2);
lie->add_relation(rel___dollorinter__body23__3__3__2);
lie->add_relation(rel__argk__4__1__2__3__4);
lie->add_relation(rel___dollorhead__stratified38__4__4__3__1);
lie->add_relation(rel__setb__3__3);
lie->add_relation(rel___dollorinter__body61__4__1__2__3__4);
lie->add_relation(rel___dollorinter__body82__3__1__2__3);
lie->add_relation(rel__lambda__3__1__2__3);
lie->add_relation(rel___dollorinter__body26__2__1__2);
lie->add_relation(rel___dollorinter__head18__5__5__3__1);
lie->add_relation(rel___dollorbir__sub3__4__1__2__3__4);
lie->add_relation(rel__if__4__3);
lie->add_relation(rel___dollorinter__body50__3__1);
lie->add_relation(rel___dollorhead__stratified35__4__4__2);
lie->add_relation(rel___dollorinter__body23__3__1__2__3);
lie->add_relation(rel___dollorinter__body41__2__1);
lie->add_relation(rel___dollorinter__head5__4__1__2__3__4);
lie->add_relation(rel___dollorinter__body21__5__1__2__3__4__5);
lie->add_relation(rel___dollorinter__head20__3__1__2__3);
lie->add_relation(rel___dollorinter__body75__3__1__2__3);
lie->add_relation(rel__lambda__3__2);
lie->add_relation(rel___dollorbir__sub4__3__1);
lie->add_relation(rel___dollorinter__head3__4__4__2__1);
lie->add_relation(rel___dollorhead__stratified28__2__);
lie->add_relation(rel___dollorinter__body46__4__4);
lie->add_relation(rel___dollorinter__body69__2__1);
lie->add_relation(rel__flow__ae__2__1__2);
lie->add_relation(rel___dollorinter__head15__5__1__2);
lie->add_relation(rel___dollorinter__body86__4__4__1);
lie->add_relation(rel___dollorinter__body73__3__1__2__3);
lie->add_relation(rel___dollorhead__stratified21__3__1__2__3);
lie->add_relation(rel__here__5__1__2__3__4__5);
lie->add_relation(rel__kaddr__2__2__1);
lie->add_relation(rel___dollorinter__body65__3__);
lie->add_relation(rel___dollorinter__body69__2__1__2);
lie->add_relation(rel___dollorinter__body89__3__3__2);
lie->add_relation(rel___dollorinter__body63__2__1);
lie->add_relation(rel___dollorinter__body77__3__3__2);
lie->add_relation(rel__store__2__1__2);
lie->add_relation(rel___dollorinter__head4__3__);
lie->add_relation(rel___dollorinter__body67__3__1__2__3);
lie->add_relation(rel___dollorbir__sub2__2__1);
lie->add_relation(rel__argk__4__1);
lie->add_relation(rel___dollorinter__head13__6__5__2__1__3);
lie->add_relation(rel___dollorinter__body51__3__1__2__3);
lie->add_relation(rel___dollorinter__body36__3__1__2__3);
lie->add_relation(rel__kaddr__2__1__2);
lie->add_relation(rel___dollorinter__body89__3__1__2__3);
lie->add_relation(rel___dollorinter__body29__2__1__2);
lie->add_relation(rel___dollorinter__body68__2__1);
lie->add_relation(rel__E__3__1);
lie->add_relation(rel___dollorinter__body65__3__1__2__3);
lie->add_relation(rel___dollorinter__body53__2__1__2);
lie->add_relation(rel___dollorinter__body5__7__1__2__3__4__5__6__7);
lie->add_relation(rel___dollorinter__body56__3__2__3);
lie->add_relation(rel___dollorinter__body39__4__1__2__3__4);
lie->add_relation(rel__store__2__2__1);
lie->add_relation(rel___dollorinter__head25__4__1__2__3__4);
lie->add_relation(rel___dollorbir__sub__2__1__2);
lie->add_relation(rel___dollorinter__body9__3__1__2__3);
lie->add_relation(rel___dollorinter__body8__4__1__2__3__4);
lie->add_relation(rel___dollorhead__stratified28__2__1__2);
lie->add_relation(rel___dollorinter__body46__4__1__2__3__4);
lie->add_relation(rel__let__3__2);
lie->add_relation(rel___dollorinter__body19__3__3__1);
lie->add_relation(rel__var__2__1__2);
lie->add_relation(rel___dollorinter__body26__2__1);
lie->add_relation(rel___dollorinter__head26__3__1__2__3);
lie->add_relation(rel__let__list__3__1__2__3);
lie->add_relation(rel___dollorinter__head28__3__1__2__3);
lie->add_relation(rel__lambda__3__1);
lie->add_relation(rel___dollorhead__stratified43__3__1__2__3);
lie->add_relation(rel___dollorhead__stratified18__4__1__2__3__4);
lie->add_relation(rel___dollorinter__body58__2__2);
lie->add_relation(rel__flow__ea__2__1__2);
lie->add_relation(rel__copy__ctx__3__3__2__1);
lie->add_relation(rel___dollorinter__head1__7__6__2);
lie->add_relation(rel___dollorinter__body81__2__);
lie->add_relation(rel___dollorinter__body64__1__1);
lie->add_relation(rel___dollorinter__body58__2__1__2);
lie->add_relation(rel___dollorhead__stratified44__4__1__2__3__4);
lie->add_relation(rel___dollorlst__2__2__1);
lie->add_relation(rel___dollorinter__body45__3__1__2__3);
lie->add_relation(rel___dollorinter__body52__2__2);
lie->add_relation(rel___dollorhead__stratified__4__1__2__3__4);
lie->add_relation(rel___dollorinter__body30__3__1__2__3);
lie->add_relation(rel___dollorhead__stratified19__3__3__2);
lie->add_relation(rel___dollorinter__body48__3__3__1);
lie->add_relation(rel___dollorinter__body25__8__2);
lie->add_relation(rel___dollorinter__head9__4__1__2__3__4);
lie->add_relation(rel___dollorinter__head16__3__1__2__3);
lie->add_relation(rel___dollorinter__head18__5__1__2__3__4__5);
lie->add_relation(rel___dollorinter__body47__4__1__2__3__4);
lie->add_relation(rel__prim__call__3__1__2__3);
lie->add_relation(rel___dollorinter__body74__4__1__2__3__4);
lie->add_relation(rel___dollorinter__body50__3__1__2__3);
lie->add_relation(rel__peek__ctx__3__2__3__1);
lie->add_relation(rel___dollorinter__head25__4__4__3__1);
lie->add_relation(rel___dollorinter__body72__1__1);
lie->add_relation(rel___dollorinter__body62__2__1__2);
lie->add_relation(rel___dollorinter__body2__4__3);
lie->add_relation(rel__letk__4__4__3__2__1);
lie->add_relation(rel___dollorinter__head17__3__);
lie->add_relation(rel___dollorinter__body1__3__3__1);
lie->add_relation(rel___dollorhead__stratified3__2__1__2);
lie->add_relation(rel___dollorinter__head7__6__1__2__3__4__5__6);
lie->add_relation(rel___dollorhead__stratified2__7__1__2__3__4__5__6__7);
lie->add_relation(rel___dollorbir__sub1__4__1__2__3__4);
lie->add_relation(rel___dollorinter__head31__3__2__1);
lie->add_relation(rel___dollorinter__body34__3__);
lie->add_relation(rel__flow__ee__2__1__2);
lie->add_relation(rel___dollorinter__head10__3__3__2);
lie->add_relation(rel___dollorinter__body86__4__1__2__3__4);
lie->add_relation(rel___dollorinter__head3__4__1__2__3__4);
lie->add_relation(rel___dollorinter__body5__7__3__6);
lie->add_relation(rel___dollorbir__sub__2__2__1);
lie->add_relation(rel___dollorinter__head27__6__6__3__2__1);
lie->add_relation(rel___dollorinter__body7__4__3);
lie->add_relation(rel__primval__3__1__2__3);
lie->add_relation(rel___dollorinter__body19__3__1__2__3);
lie->add_relation(rel__if__4__4);
lie->add_relation(rel___dollorinter__head19__5__1__2);
lie->add_relation(rel__callcc__2__1);
lie->add_relation(rel__setb__3__1);
lie->add_relation(rel__top__exp__1__);
lie->add_relation(rel___dollorinter__head1__7__1__2__3__4__5__6__7);
lie->add_relation(rel___dollorinter__body8__4__3__2);
lie->add_relation(rel___dollorinter__body42__5__1__2__3__4__5);
lie->add_relation(rel___dollorinter__body75__3__2);
lie->add_relation(rel___dollorlst__2__0);
lie->add_relation(rel___dollorinter__head12__4__1__2__3__4);
lie->add_relation(rel___dollorhead__stratified45__2__);
lie->add_relation(rel___dollorinter__body54__4__1__2__3__4);
lie->add_relation(rel___dollorbir__sub5__4__1__2__3__4);
lie->add_relation(rel___dollorinter__body67__3__3);
lie->add_relation(rel___dollorinter__body61__4__4);
lie->add_relation(rel___dollorhead__stratified31__2__);
lie->add_relation(rel___dollorinter__body3__4__3__4__2);
lie->add_relation(rel__var__2__2__1);
lie->add_relation(rel___dollorinter__body20__7__1__2__3__4__5__6__7);
lie->add_relation(rel___dollorhead__stratified31__2__1__2);
lie->add_relation(rel__number__1__1);
lie->add_relation(rel___dollorinter__head7__6__6__4__2__1);
lie->add_relation(rel___dollorhead__stratified4__4__);
lie->add_relation(rel__free__2__2);
lie->add_relation(rel___dollorhead__stratified21__3__3__1);
lie->add_relation(rel___dollorinter__body__4__1__3);
lie->add_relation(rel___dollorinter__head11__3__);
lie->add_relation(rel___dollorinter__head28__3__2);
lie->add_relation(rel___dollorinter__body22__4__3__1);
lie->add_relation(rel___dollorinter__body55__5__1);
lie->add_relation(rel__peek__ctx__3__1__2__3);
lie->add_relation(rel___dollorinter__body17__2__1__2);
lie->add_relation(rel__callcc__2__2);
lie->add_relation(rel___dollorinter__body60__2__1__2);
lie->add_relation(rel__call__arg__list__3__2);
lie->add_relation(rel___dollorinter__body16__3__1__2__3);
lie->add_relation(rel___dollorinter__body37__2__1);
lie->add_relation(rel___dollorhead__stratified9__4__1__2__3__4);
lie->add_relation(rel___dollorinter__body88__2__2);
lie->add_relation(rel___dollorinter__body43__3__1__2__3);
lie->add_relation(rel___dollorinter__body90__3__1__2__3);
lie->add_relation(rel___dollorinter__body24__6__1__4);
lie->add_relation(rel__flow__ae__2__2__1);
lie->add_relation(rel___dollorinter__head8__8__2__7__6__1__4);
lie->add_relation(rel___dollorinter__body21__5__5__1__2);
lie->add_relation(rel___dollorinter__body35__4__1__2__3__4);
lie->add_relation(rel___dollorinter__body18__4__1__2__3__4);
lie->add_relation(rel___dollorinter__body63__2__1__2);
lie->add_relation(rel___dollorhead__stratified38__4__1__2__3__4);
lie->add_relation(rel__prim1__4__1__2__3__4);
lie->add_relation(rel___dollorinter__body2__4__1__2__3__4);
lie->add_relation(rel___dollorinter__body70__2__2);
lie->add_relation(rel__closure__2__2__1);
lie->add_relation(rel__let__3__1);
lie->add_relation(rel__A__2__2);
lie->add_relation(rel__A__2__1__2);
lie->add_relation(rel___dollorhead__stratified39__4__1__2__3__4);
lie->add_relation(rel___dollorinter__head26__3__);
lie->add_relation(rel__flow__ee__2__2__1);
lie->add_relation(rel___dollorinter__body57__3__2);
lie->add_relation(rel__E__3__2__1);
lie->add_relation(rel__vaddr__2__2__1);
lie->add_relation(rel__let__3__3);
lie->add_relation(rel__ifk__4__3__2__4__1);
lie->add_relation(rel___dollorinter__body40__4__2);
lie->add_relation(rel__prim2__3__0);
lie->add_relation(rel__bool__2__1__2);
lie->add_relation(rel___dollorinter__head32__4__1__2);
lie->add_relation(rel___dollorinter__body__4__1__2__3__4);
lie->add_relation(rel___dollorhead__stratified4__4__1__2__3__4);
lie->add_relation(rel___dollorinter__head6__4__1__2__3__4);
lie->add_relation(rel___dollorinter__body48__3__1__2__3);
lie->add_relation(rel___dollorinter__body22__4__1__2__3__4);
lie->add_relation(rel__kont__1__1);
lie->add_relation(rel___dollorinter__body33__1__1);
lie->add_relation(rel___dollorinter__head12__4__4__3__1);
lie->add_relation(rel___dollorinter__body71__2__1__2);
lie->add_relation(rel___dollorinter__body12__2__1);
lie->add_relation(rel___dollorhead__stratified23__4__1__2__3__4);
lie->add_relation(rel___dollorinter__body57__3__1__2__3);
lie->add_relation(rel___dollorinter__body13__3__2);
lie->add_relation(rel___dollorinter__body44__2__1__2);
lie->add_relation(rel___dollorinter__body41__2__1__2);
lie->add_relation(rel___dollorinter__body14__5__5);
lie->add_relation(rel__closure__2__0);
lie->add_relation(rel__A__2__1);
lie->add_relation(rel__let__list__3__3);
lie->add_relation(rel___dollorinter__head24__2__);
lie->add_relation(rel___dollorinter__body80__2__);
lie->add_relation(rel___dollorhead__stratified35__4__1__2__3__4);
lie->add_relation(rel___dollorinter__head__5__1__2__3__4__5);
lie->add_relation(rel__letk__4__0);
lie->add_relation(rel___dollorinter__body31__2__2);
lie->add_relation(rel___dollorinter__head22__4__1__2__3__4);
lie->add_relation(rel___dollorinter__head23__5__);
lie->add_relation(rel___dollorhead__stratified42__4__4__2);
lie->add_relation(rel__fn__4__4__3__2__1);
lie->add_relation(rel___dollorhead__stratified3__2__);
lie->add_relation(rel___dollorinter__body60__2__2);
lie->add_relation(rel__callcck__2__1__2);
lie->add_relation(rel___dollorinter__body84__3__1__2__3);
lie->add_relation(rel___dollorhead__stratified36__4__4__2);
lie->add_relation(rel___dollorinter__body77__3__1__2__3);
lie->add_relation(rel___dollorhead__stratified45__2__1__2);
lie->add_relation(rel__vaddr__2__1__2);
lie->add_relation(rel___dollorinter__body18__4__3__1);
lie->add_relation(rel___dollorinter__head30__5__1__2__3__4__5);
lie->add_relation(rel___dollorhead__stratified__4__);
lie->add_relation(rel___dollorinter__body70__2__1__2);
lie->add_relation(rel___dollorinter__body73__3__);
lie->add_relation(rel___dollorinter__body28__4__1__2__3__4);
lie->add_relation(rel___dollorinter__body66__2__1__2);
lie->add_relation(rel___dollorinter__body76__4__3__2);
lie->add_relation(rel___dollorinter__body84__3__1);
lie->add_relation(rel___dollorinter__head2__6__6__5__2__1);
lie->add_relation(rel___dollorhead__stratified9__4__);
lie->add_relation(rel__flow__aa__2__1__2);
lie->add_relation(rel___dollorinter__body16__3__1);
lie->add_relation(rel___dollorinter__body83__2__1);
lie->add_relation(rel__lambda__arg__list__3__1__2__3);
lie->add_relation(rel___dollorinter__body85__4__1);
lie->add_relation(rel__top__exp__1__1);
lie->add_relation(rel___dollorinter__body74__4__4__3);
lie->add_relation(rel___dollorinter__body79__4__3__4);
lie->add_relation(rel___dollorinter__body13__3__1__2__3);
lie->add_relation(rel__null__0__);
lie->add_relation(rel___dollorinter__head16__3__2);
lie->add_relation(rel___dollorinter__head9__4__3__2);
lie->add_relation(rel___dollorinter__body32__4__1__2__3__4);
lie->add_relation(rel___dollorhead__stratified15__2__);
lie->add_relation(rel__lambda__3__3);
lie->add_relation(rel___dollorinter__body27__3__1__2__3);
lie->add_relation(rel__num__2__1);
lie->add_relation(rel___dollorinter__body85__4__1__2__3__4);
lie->add_relation(rel___dollorinter__body59__2__1__2);
lie->add_relation(rel___dollorinter__head23__5__1__2__3__4__5);
lie->add_relation(rel___dollorinter__head2__6__1__2__3__4__5__6);
lie->add_relation(rel__call__3__2);
lie->add_relation(rel__callcc__2__1__2);
lie->add_relation(rel__call__3__1__2__3);
lie->add_relation(rel___dollorinter__head13__6__1__2__3__4__5__6);
lie->add_relation(rel___dollorinter__body3__4__1__2__3__4);
lie->add_relation(rel___dollorhead__stratified11__2__);
lie->add_relation(rel___dollorinter__head27__6__1__2__3__4__5__6);
lie->add_relation(rel__callcc__2__0);
lie->add_relation(rel___dollorinter__body78__3__3__1);
lie->add_relation(rel___dollorhead__stratified20__4__4__1);
lie->add_relation(rel___dollorhead__stratified39__4__);
lie->add_relation(rel__num__2__1__2);
lie->add_relation(rel___dollorbir__sub6__3__1);
lie->add_relation(rel__fn__4__1__2__3__4);
lie->add_relation(rel__prim__2__1__2);
lie->add_relation(rel___dollorinter__body37__2__1__2);
lie->add_relation(rel__boolv__1__0);
lie->add_relation(rel___dollorinter__head4__3__1__2__3);
lie->add_relation(rel___dollorinter__body71__2__2);
lie->add_relation(rel___dollorinter__body25__8__1__2__3__4__5__6__7__8);
lie->add_relation(rel___dollorinter__body43__3__1);
lie->add_relation(rel___dollorbir__sub3__4__3__1);
lie->add_relation(rel___dollorhead__stratified37__4__1__2__3__4);
lie->add_relation(rel___dollorinter__head14__5__3__1);
lie->add_relation(rel__copy__ctx__3__1);
lie->add_relation(rel___dollorinter__body88__2__1__2);
lie->add_relation(rel___dollorhead__stratified26__3__1__2__3);
lie->add_relation(rel___dollorinter__body62__2__2);
lie->add_relation(rel___dollorinter__head14__5__1__2__3__4__5);
lie->add_relation(rel___dollorhead__stratified33__4__1__2__3__4);
lie->add_relation(rel__call__3__3);
lie->add_relation(rel___dollorinter__body20__7__2__1__4);
lie->add_relation(rel___dollorinter__head24__2__1__2);
lie->add_relation(rel___dollorinter__body9__3__3__2);
lie->add_relation(rel___dollorinter__head32__4__1__2__3__4);
lie->add_relation(rel__setk__2__1__2);
lie->add_relation(rel___dollorinter__body83__2__1__2);
lie->add_relation(rel__mt__0__);
lie->add_relation(rel__num__1__1);
lie->add_relation(rel__prim1__4__3__2__1__4);
lie->add_relation(rel___dollorhead__stratified12__4__1__2__3__4);
lie->add_relation(rel___dollorinter__head20__3__1);
lie->add_relation(rel___dollorinter__body10__4__1__2__3__4);
lie->add_relation(rel___dollorlst__2__2);
lie->add_relation(rel___dollorinter__head5__4__);
lie->add_relation(rel___dollorinter__body4__7__1__2__3__4__5__6__7);
lie->add_relation(rel___dollorinter__body6__6__6__3);
lie->add_relation(rel___dollorinter__body36__3__2__3);
lie->add_relation(rel___dollorinter__body76__4__1__2__3__4);
lie->add_relation(rel__boolv__1__1);
lie->add_relation(rel__E__3__3__2__1);
lie->add_relation(rel__setk__2__0);
lie->add_relation(rel___dollorhead__stratified23__4__);
lie->add_relation(rel___dollorinter__head8__8__1__2__3__4__5__6__7__8);
lie->add_relation(rel__bool__2__1);
lie->add_relation(rel___dollorinter__body78__3__1__2__3);
lie->add_relation(rel__prim__call__3__3);
lie->add_relation(rel__let__3__1__2__3);
lie->add_relation(rel___dollorinter__body81__2__1__2);
lie->add_relation(rel__letk__4__1__2__3__4);
lie->add_relation(rel___dollorinter__body10__4__3);
lie->add_relation(rel___dollorinter__body15__2__1);
lie->add_relation(rel___dollorinter__body79__4__1__2__3__4);
lie->add_relation(rel___dollorinter__body1__3__1__2__3);
lie->add_relation(rel__fn__4__1);
lie->add_relation(rel___dollorinter__body27__3__1);
lie->add_relation(rel___dollorinter__body4__7__6__2__7);
lie->add_relation(rel___dollorbir__sub4__3__1__2__3);
lie->add_relation(rel___dollorinter__body35__4__4__2);
lie->add_relation(rel__here__5__5__4__3__2__1);
lie->add_relation(rel___dollorinter__body68__2__1__2);
lie->add_relation(rel___dollorinter__body87__3__1__2__3);
lie->add_relation(rel__copy__ctx__3__1__2__3);
lie->add_relation(rel___dollorinter__head31__3__1__2__3);
lie->add_relation(rel___dollorbir__sub1__4__1);
lie->add_relation(rel__call__arg__list__3__1__2__3);
lie->add_relation(rel___dollorinter__head29__3__1__2__3);
lie->add_relation(rel__free__2__1__2);
lie->add_relation(rel___dollorinter__body11__2__1);
lie->add_relation(rel__E__3__2);
lie->add_relation(rel___dollorhead__stratified2__7__1__2__4__7);
lie->add_relation(rel__let__list__3__1);
lie->add_relation(rel___dollorinter__body29__2__1);
lie->add_relation(rel___dollorinter__body56__3__1__2__3);
lie->add_relation(rel___dollorinter__body80__2__1__2);
lie->add_relation(rel___dollorinter__head10__3__1__2__3);
lie->add_relation(rel___dollorhead__stratified25__4__4__1);
lie->add_relation(rel__setk__1__1);
lie->add_relation(rel__setb__3__1__2__3);
lie->add_relation(rel__closure__2__1__2);
lie->add_scc(scc5451);
lie->add_scc(scc5452);
lie->add_scc(scc5453);
lie->add_scc(scc5454);
lie->add_scc(scc5455);
lie->add_scc(scc5456);
lie->add_scc(scc5457);
lie->add_scc(scc5458);
lie->add_scc(scc5459);
lie->add_scc(scc5460);
lie->add_scc(scc5461);
lie->add_scc(scc5462);
lie->add_scc(scc5463);
lie->add_scc(scc5464);
lie->add_scc(scc5465);
lie->add_scc(scc5466);
lie->add_scc(scc5467);
lie->add_scc(scc5468);
lie->add_scc(scc5469);
lie->add_scc(scc5470);
lie->add_scc(scc5471);
lie->add_scc(scc5472);
lie->add_scc(scc5473);
lie->add_scc(scc5474);
lie->add_scc(scc5475);
lie->add_scc(scc5476);
lie->add_scc(scc5477);
lie->add_scc(scc5478);
lie->add_scc(scc5479);
lie->add_scc(scc5480);
lie->add_scc(scc5481);
lie->add_scc(scc5482);
lie->add_scc(scc5483);
lie->add_scc(scc5484);
lie->add_scc(scc5485);
lie->add_scc(scc5486);
lie->add_scc(scc5487);
lie->add_scc(scc5488);
lie->add_scc(scc5489);
lie->add_scc(scc5490);
lie->add_scc(scc5491);
lie->add_scc(scc5492);
lie->add_scc(scc5493);
lie->add_scc(scc5494);
lie->add_scc(scc5495);
lie->add_scc(scc5496);
lie->add_scc(scc5497);
lie->add_scc(scc5498);
lie->add_scc(scc5499);
lie->add_scc(scc5500);
lie->add_scc(scc5501);
lie->add_scc(scc5502);
lie->add_scc(scc5503);
lie->add_scc(scc5504);
lie->add_scc(scc5505);
lie->add_scc(scc5506);
lie->add_scc(scc5507);
lie->add_scc(scc5508);
lie->add_scc(scc5509);
lie->add_scc(scc5510);
lie->add_scc(scc5511);
lie->add_scc(scc5512);
lie->add_scc(scc5513);
lie->add_scc(scc5514);
lie->add_scc(scc5515);
lie->add_scc(scc5516);
lie->add_scc(scc5517);
lie->add_scc(scc5518);
lie->add_scc(scc5519);
lie->add_scc(scc5520);
lie->add_scc(scc5521);
lie->add_scc(scc5522);
lie->add_scc(scc5523);
lie->add_scc(scc5524);
lie->add_scc(scc5525);
lie->add_scc(scc5526);
lie->add_scc(scc5527);
lie->add_scc(scc5528);
lie->add_scc(scc5529);
lie->add_scc(scc5530);
lie->add_scc(scc5531);
lie->add_scc(scc5532);
lie->add_scc(scc5533);
lie->add_scc(scc5534);
lie->add_scc(scc5535);
lie->add_scc(scc5536);
lie->add_scc(scc5537);
lie->add_scc(scc5538);
lie->add_scc(scc5539);
lie->add_scc(scc5540);
lie->add_scc(scc5541);
lie->add_scc(scc5542);
lie->add_scc(scc5543);
lie->add_scc(scc5544);
lie->add_scc(scc5545);
lie->add_scc(scc5546);
lie->add_scc(scc5547);
lie->add_scc(scc5548);
lie->add_scc(scc5549);
lie->add_scc(scc5550);
lie->add_scc(scc5551);
lie->add_scc(scc5552);
lie->add_scc(scc5553);
lie->add_scc(scc5554);
lie->add_scc(scc5555);
lie->add_scc(scc5556);
lie->add_scc(scc5557);
lie->add_scc(scc5558);
lie->add_scc(scc5559);
lie->add_scc(scc5560);
lie->add_scc(scc5561);
lie->add_scc(scc5562);
lie->add_scc(scc5563);
lie->add_scc(scc5564);
lie->add_scc(scc5565);
lie->add_scc(scc5566);
lie->add_scc(scc5567);
lie->add_scc(scc5568);
lie->add_scc(scc5569);
lie->add_scc(scc5570);
lie->add_scc(scc5571);
lie->add_scc(scc5572);
lie->add_scc(scc5573);
lie->add_scc(scc5574);
lie->add_scc(scc5575);
lie->add_scc(scc5576);
lie->add_scc(scc5577);
lie->add_scc(scc5578);
lie->add_scc(scc5579);
lie->add_scc(scc5580);
lie->add_scc(scc5581);
lie->add_scc(scc5582);
lie->add_scc(scc5583);
lie->add_scc(scc5584);
lie->add_scc(scc5585);
lie->add_scc(scc5586);
lie->add_scc(scc5587);
lie->add_scc(scc5588);
lie->add_scc(scc5589);
lie->add_scc(scc5590);
lie->add_scc(scc5591);
lie->add_scc(scc5592);
lie->add_scc(scc5593);
lie->add_scc(scc5594);
lie->add_scc(scc5595);
lie->add_scc(scc5596);
lie->add_scc(scc5597);
lie->add_scc(scc5598);
lie->add_scc(scc5599);
lie->add_scc(scc5600);
lie->add_scc(scc5601);
lie->add_scc(scc5602);
lie->add_scc(scc5603);
lie->add_scc(scc5604);
lie->add_scc(scc5605);
lie->add_scc(scc5606);
lie->add_scc(scc5607);
lie->add_scc(scc5608);
lie->add_scc(scc5609);
lie->add_scc(scc5610);
lie->add_scc(scc5611);
lie->add_scc(scc5612);
lie->add_scc(scc5613);
lie->add_scc(scc5614);
lie->add_scc(scc5615);
lie->add_scc(scc5616);
lie->add_scc(scc5617);
lie->add_scc(scc5618);
lie->add_scc(scc5619);
lie->add_scc(scc5620);
lie->add_scc(scc5621);
lie->add_scc(scc5622);
lie->add_scc(scc5623);
lie->add_scc(scc5624);
lie->add_scc(scc5625);
lie->add_scc(scc5626);
lie->add_scc(scc5627);
lie->add_scc(scc5628);
lie->add_scc(scc5629);
lie->add_scc(scc5630);
lie->add_scc(scc5631);
lie->add_scc(scc5632);
lie->add_scc(scc5633);
lie->add_scc(scc5634);
lie->add_scc(scc5635);
lie->add_scc(scc5636);
lie->add_scc(scc5637);
lie->add_scc(scc5638);
lie->add_scc(scc5639);
lie->add_scc(scc5640);
lie->add_scc(scc5641);
lie->add_scc(scc5642);
lie->add_scc(scc5643);
lie->add_scc(scc5644);
lie->add_scc(scc5645);
lie->add_scc(scc5646);
lie->add_scc(scc5647);
lie->add_scc(scc5648);
lie->add_scc(scc5649);
lie->add_scc(scc5650);
lie->add_scc(scc5651);
lie->add_scc(scc5652);
lie->add_scc(scc5653);
lie->add_scc(scc5654);
lie->add_scc(scc5655);
lie->add_scc(scc5656);
lie->add_scc(scc5657);
lie->add_scc(scc5658);
lie->add_scc(scc5659);
lie->add_scc(scc5660);
lie->add_scc(scc5661);
lie->add_scc(scc5662);
lie->add_scc(scc5663);
lie->add_scc(scc5664);
lie->add_scc(scc5665);
lie->add_scc(scc5666);
lie->add_scc(scc5667);
lie->add_scc(scc5668);
lie->add_scc(scc5669);
lie->add_scc(scc5670);
lie->add_scc(scc5671);
lie->add_scc(scc5672);
lie->add_scc(scc5673);
lie->add_scc(scc5674);
lie->add_scc(scc5675);
lie->add_scc(scc5676);
lie->add_scc(scc5677);
lie->add_scc(scc5678);
lie->add_scc(scc5679);
lie->add_scc(scc5680);
lie->add_scc(scc5681);
lie->add_scc(scc5682);
lie->add_scc(scc5683);
lie->add_scc(scc5684);
lie->add_scc(scc5685);
lie->add_scc(scc5686);
lie->add_scc(scc5687);
lie->add_scc(scc5688);
lie->add_scc(scc5689);
lie->add_scc(scc5690);
lie->add_scc(scc5691);
lie->add_scc(scc5692);
lie->add_scc(scc5693);
lie->add_scc(scc5694);
lie->add_scc(scc5695);
lie->add_scc(scc5696);
lie->add_scc(scc5697);
lie->add_scc(scc5698);
lie->add_scc(scc5699);
lie->add_scc(scc5700);
lie->add_scc(scc5701);
lie->add_scc(scc5702);
lie->add_scc(scc5703);
lie->add_scc(scc5704);
lie->add_scc(scc5705);
lie->add_scc(scc5706);
lie->add_scc(scc5707);
lie->add_scc(scc5708);
lie->add_scc(scc5709);
lie->add_scc(scc5710);
lie->add_scc(scc5711);
lie->add_scc(scc5712);
lie->add_scc(scc5713);
lie->add_scc(scc5714);
lie->add_scc(scc5715);
lie->add_scc(scc5716);
lie->add_scc(scc5717);
lie->add_scc(scc5718);
lie->add_scc(scc5719);
lie->add_scc(scc5720);
lie->add_scc(scc5721);
lie->add_scc(scc5722);
lie->add_scc(scc5723);
lie->add_scc(scc5724);
lie->add_scc(scc5725);
lie->add_scc(scc5726);
lie->add_scc(scc5727);
lie->add_scc(scc5728);
lie->add_scc(scc5729);
lie->add_scc(scc5730);
lie->add_scc(scc5731);
lie->add_scc(scc5732);
lie->add_scc(scc5733);
lie->add_scc(scc5734);
lie->add_scc(scc5735);
lie->add_scc(scc5736);
lie->add_scc(scc5737);
lie->add_scc_dependance(scc5451, scc5466);
lie->add_scc_dependance(scc5451, scc5463);
lie->add_scc_dependance(scc5452, scc5683);
lie->add_scc_dependance(scc5452, scc5677);
lie->add_scc_dependance(scc5452, scc5653);
lie->add_scc_dependance(scc5453, scc5599);
lie->add_scc_dependance(scc5453, scc5507);
lie->add_scc_dependance(scc5454, scc5466);
lie->add_scc_dependance(scc5454, scc5463);
lie->add_scc_dependance(scc5455, scc5670);
lie->add_scc_dependance(scc5456, scc5599);
lie->add_scc_dependance(scc5456, scc5507);
lie->add_scc_dependance(scc5457, scc5466);
lie->add_scc_dependance(scc5457, scc5463);
lie->add_scc_dependance(scc5458, scc5670);
lie->add_scc_dependance(scc5459, scc5657);
lie->add_scc_dependance(scc5460, scc5575);
lie->add_scc_dependance(scc5460, scc5570);
lie->add_scc_dependance(scc5461, scc5626);
lie->add_scc_dependance(scc5462, scc5670);
lie->add_scc_dependance(scc5463, scc5645);
lie->add_scc_dependance(scc5463, scc5570);
lie->add_scc_dependance(scc5464, scc5599);
lie->add_scc_dependance(scc5464, scc5507);
lie->add_scc_dependance(scc5465, scc5599);
lie->add_scc_dependance(scc5465, scc5507);
lie->add_scc_dependance(scc5466, scc5681);
lie->add_scc_dependance(scc5467, scc5681);
lie->add_scc_dependance(scc5469, scc5632);
lie->add_scc_dependance(scc5471, scc5466);
lie->add_scc_dependance(scc5471, scc5463);
lie->add_scc_dependance(scc5472, scc5609);
lie->add_scc_dependance(scc5472, scc5543);
lie->add_scc_dependance(scc5472, scc5519);
lie->add_scc_dependance(scc5473, scc5599);
lie->add_scc_dependance(scc5473, scc5507);
lie->add_scc_dependance(scc5474, scc5599);
lie->add_scc_dependance(scc5474, scc5507);
lie->add_scc_dependance(scc5475, scc5679);
lie->add_scc_dependance(scc5475, scc5669);
lie->add_scc_dependance(scc5475, scc5659);
lie->add_scc_dependance(scc5475, scc5498);
lie->add_scc_dependance(scc5477, scc5670);
lie->add_scc_dependance(scc5478, scc5670);
lie->add_scc_dependance(scc5479, scc5609);
lie->add_scc_dependance(scc5479, scc5543);
lie->add_scc_dependance(scc5479, scc5519);
lie->add_scc_dependance(scc5480, scc5599);
lie->add_scc_dependance(scc5480, scc5507);
lie->add_scc_dependance(scc5482, scc5676);
lie->add_scc_dependance(scc5482, scc5602);
lie->add_scc_dependance(scc5482, scc5582);
lie->add_scc_dependance(scc5484, scc5599);
lie->add_scc_dependance(scc5484, scc5507);
lie->add_scc_dependance(scc5485, scc5599);
lie->add_scc_dependance(scc5485, scc5507);
lie->add_scc_dependance(scc5486, scc5609);
lie->add_scc_dependance(scc5486, scc5543);
lie->add_scc_dependance(scc5486, scc5519);
lie->add_scc_dependance(scc5487, scc5599);
lie->add_scc_dependance(scc5487, scc5507);
lie->add_scc_dependance(scc5488, scc5609);
lie->add_scc_dependance(scc5488, scc5543);
lie->add_scc_dependance(scc5488, scc5519);
lie->add_scc_dependance(scc5489, scc5679);
lie->add_scc_dependance(scc5489, scc5669);
lie->add_scc_dependance(scc5489, scc5659);
lie->add_scc_dependance(scc5489, scc5498);
lie->add_scc_dependance(scc5491, scc5599);
lie->add_scc_dependance(scc5491, scc5507);
lie->add_scc_dependance(scc5492, scc5679);
lie->add_scc_dependance(scc5492, scc5669);
lie->add_scc_dependance(scc5492, scc5659);
lie->add_scc_dependance(scc5492, scc5498);
lie->add_scc_dependance(scc5493, scc5679);
lie->add_scc_dependance(scc5493, scc5669);
lie->add_scc_dependance(scc5493, scc5659);
lie->add_scc_dependance(scc5493, scc5498);
lie->add_scc_dependance(scc5494, scc5670);
lie->add_scc_dependance(scc5495, scc5679);
lie->add_scc_dependance(scc5495, scc5669);
lie->add_scc_dependance(scc5495, scc5659);
lie->add_scc_dependance(scc5495, scc5498);
lie->add_scc_dependance(scc5496, scc5657);
lie->add_scc_dependance(scc5497, scc5609);
lie->add_scc_dependance(scc5497, scc5543);
lie->add_scc_dependance(scc5497, scc5519);
lie->add_scc_dependance(scc5498, scc5681);
lie->add_scc_dependance(scc5499, scc5609);
lie->add_scc_dependance(scc5499, scc5543);
lie->add_scc_dependance(scc5499, scc5519);
lie->add_scc_dependance(scc5500, scc5460);
lie->add_scc_dependance(scc5501, scc5609);
lie->add_scc_dependance(scc5501, scc5543);
lie->add_scc_dependance(scc5501, scc5519);
lie->add_scc_dependance(scc5502, scc5670);
lie->add_scc_dependance(scc5503, scc5679);
lie->add_scc_dependance(scc5503, scc5669);
lie->add_scc_dependance(scc5503, scc5659);
lie->add_scc_dependance(scc5503, scc5498);
lie->add_scc_dependance(scc5505, scc5466);
lie->add_scc_dependance(scc5505, scc5463);
lie->add_scc_dependance(scc5506, scc5679);
lie->add_scc_dependance(scc5506, scc5669);
lie->add_scc_dependance(scc5506, scc5659);
lie->add_scc_dependance(scc5506, scc5498);
lie->add_scc_dependance(scc5507, scc5570);
lie->add_scc_dependance(scc5508, scc5466);
lie->add_scc_dependance(scc5508, scc5463);
lie->add_scc_dependance(scc5509, scc5679);
lie->add_scc_dependance(scc5509, scc5669);
lie->add_scc_dependance(scc5509, scc5659);
lie->add_scc_dependance(scc5509, scc5498);
lie->add_scc_dependance(scc5510, scc5536);
lie->add_scc_dependance(scc5511, scc5460);
lie->add_scc_dependance(scc5512, scc5609);
lie->add_scc_dependance(scc5512, scc5543);
lie->add_scc_dependance(scc5512, scc5519);
lie->add_scc_dependance(scc5513, scc5670);
lie->add_scc_dependance(scc5514, scc5609);
lie->add_scc_dependance(scc5514, scc5543);
lie->add_scc_dependance(scc5514, scc5519);
lie->add_scc_dependance(scc5515, scc5466);
lie->add_scc_dependance(scc5515, scc5463);
lie->add_scc_dependance(scc5516, scc5676);
lie->add_scc_dependance(scc5516, scc5602);
lie->add_scc_dependance(scc5516, scc5582);
lie->add_scc_dependance(scc5517, scc5609);
lie->add_scc_dependance(scc5517, scc5543);
lie->add_scc_dependance(scc5517, scc5519);
lie->add_scc_dependance(scc5518, scc5679);
lie->add_scc_dependance(scc5518, scc5669);
lie->add_scc_dependance(scc5518, scc5659);
lie->add_scc_dependance(scc5518, scc5498);
lie->add_scc_dependance(scc5519, scc5633);
lie->add_scc_dependance(scc5519, scc5617);
lie->add_scc_dependance(scc5520, scc5701);
lie->add_scc_dependance(scc5521, scc5679);
lie->add_scc_dependance(scc5521, scc5669);
lie->add_scc_dependance(scc5521, scc5659);
lie->add_scc_dependance(scc5521, scc5498);
lie->add_scc_dependance(scc5522, scc5670);
lie->add_scc_dependance(scc5523, scc5670);
lie->add_scc_dependance(scc5524, scc5609);
lie->add_scc_dependance(scc5524, scc5543);
lie->add_scc_dependance(scc5524, scc5519);
lie->add_scc_dependance(scc5525, scc5466);
lie->add_scc_dependance(scc5525, scc5463);
lie->add_scc_dependance(scc5526, scc5466);
lie->add_scc_dependance(scc5526, scc5463);
lie->add_scc_dependance(scc5527, scc5466);
lie->add_scc_dependance(scc5527, scc5463);
lie->add_scc_dependance(scc5529, scc5670);
lie->add_scc_dependance(scc5531, scc5670);
lie->add_scc_dependance(scc5532, scc5609);
lie->add_scc_dependance(scc5532, scc5543);
lie->add_scc_dependance(scc5532, scc5519);
lie->add_scc_dependance(scc5533, scc5679);
lie->add_scc_dependance(scc5533, scc5669);
lie->add_scc_dependance(scc5533, scc5659);
lie->add_scc_dependance(scc5533, scc5498);
lie->add_scc_dependance(scc5534, scc5681);
lie->add_scc_dependance(scc5535, scc5466);
lie->add_scc_dependance(scc5535, scc5463);
lie->add_scc_dependance(scc5536, scc5558);
lie->add_scc_dependance(scc5537, scc5670);
lie->add_scc_dependance(scc5538, scc5599);
lie->add_scc_dependance(scc5538, scc5507);
lie->add_scc_dependance(scc5539, scc5681);
lie->add_scc_dependance(scc5540, scc5466);
lie->add_scc_dependance(scc5540, scc5463);
lie->add_scc_dependance(scc5541, scc5670);
lie->add_scc_dependance(scc5542, scc5670);
lie->add_scc_dependance(scc5543, scc5654);
lie->add_scc_dependance(scc5544, scc5466);
lie->add_scc_dependance(scc5544, scc5463);
lie->add_scc_dependance(scc5546, scc5679);
lie->add_scc_dependance(scc5546, scc5669);
lie->add_scc_dependance(scc5546, scc5659);
lie->add_scc_dependance(scc5546, scc5498);
lie->add_scc_dependance(scc5548, scc5609);
lie->add_scc_dependance(scc5548, scc5543);
lie->add_scc_dependance(scc5548, scc5519);
lie->add_scc_dependance(scc5551, scc5679);
lie->add_scc_dependance(scc5551, scc5669);
lie->add_scc_dependance(scc5551, scc5659);
lie->add_scc_dependance(scc5551, scc5498);
lie->add_scc_dependance(scc5552, scc5681);
lie->add_scc_dependance(scc5553, scc5599);
lie->add_scc_dependance(scc5553, scc5507);
lie->add_scc_dependance(scc5555, scc5466);
lie->add_scc_dependance(scc5555, scc5463);
lie->add_scc_dependance(scc5556, scc5670);
lie->add_scc_dependance(scc5558, scc5520);
lie->add_scc_dependance(scc5559, scc5599);
lie->add_scc_dependance(scc5559, scc5507);
lie->add_scc_dependance(scc5560, scc5466);
lie->add_scc_dependance(scc5560, scc5463);
lie->add_scc_dependance(scc5561, scc5609);
lie->add_scc_dependance(scc5561, scc5543);
lie->add_scc_dependance(scc5561, scc5519);
lie->add_scc_dependance(scc5563, scc5679);
lie->add_scc_dependance(scc5563, scc5669);
lie->add_scc_dependance(scc5563, scc5659);
lie->add_scc_dependance(scc5563, scc5498);
lie->add_scc_dependance(scc5564, scc5679);
lie->add_scc_dependance(scc5564, scc5669);
lie->add_scc_dependance(scc5564, scc5659);
lie->add_scc_dependance(scc5564, scc5498);
lie->add_scc_dependance(scc5565, scc5466);
lie->add_scc_dependance(scc5565, scc5463);
lie->add_scc_dependance(scc5566, scc5460);
lie->add_scc_dependance(scc5567, scc5609);
lie->add_scc_dependance(scc5567, scc5543);
lie->add_scc_dependance(scc5567, scc5519);
lie->add_scc_dependance(scc5568, scc5609);
lie->add_scc_dependance(scc5568, scc5543);
lie->add_scc_dependance(scc5568, scc5519);
lie->add_scc_dependance(scc5569, scc5466);
lie->add_scc_dependance(scc5569, scc5463);
lie->add_scc_dependance(scc5570, scc5645);
lie->add_scc_dependance(scc5573, scc5670);
lie->add_scc_dependance(scc5575, scc5665);
lie->add_scc_dependance(scc5576, scc5599);
lie->add_scc_dependance(scc5576, scc5507);
lie->add_scc_dependance(scc5577, scc5670);
lie->add_scc_dependance(scc5578, scc5670);
lie->add_scc_dependance(scc5579, scc5599);
lie->add_scc_dependance(scc5579, scc5507);
lie->add_scc_dependance(scc5580, scc5466);
lie->add_scc_dependance(scc5580, scc5463);
lie->add_scc_dependance(scc5581, scc5466);
lie->add_scc_dependance(scc5581, scc5463);
lie->add_scc_dependance(scc5582, scc5681);
lie->add_scc_dependance(scc5583, scc5679);
lie->add_scc_dependance(scc5583, scc5669);
lie->add_scc_dependance(scc5583, scc5659);
lie->add_scc_dependance(scc5583, scc5498);
lie->add_scc_dependance(scc5584, scc5679);
lie->add_scc_dependance(scc5584, scc5669);
lie->add_scc_dependance(scc5584, scc5659);
lie->add_scc_dependance(scc5584, scc5498);
lie->add_scc_dependance(scc5585, scc5599);
lie->add_scc_dependance(scc5585, scc5507);
lie->add_scc_dependance(scc5586, scc5570);
lie->add_scc_dependance(scc5587, scc5679);
lie->add_scc_dependance(scc5587, scc5669);
lie->add_scc_dependance(scc5587, scc5659);
lie->add_scc_dependance(scc5587, scc5498);
lie->add_scc_dependance(scc5589, scc5679);
lie->add_scc_dependance(scc5589, scc5669);
lie->add_scc_dependance(scc5589, scc5659);
lie->add_scc_dependance(scc5589, scc5498);
lie->add_scc_dependance(scc5590, scc5609);
lie->add_scc_dependance(scc5590, scc5543);
lie->add_scc_dependance(scc5590, scc5519);
lie->add_scc_dependance(scc5591, scc5599);
lie->add_scc_dependance(scc5591, scc5507);
lie->add_scc_dependance(scc5592, scc5466);
lie->add_scc_dependance(scc5592, scc5463);
lie->add_scc_dependance(scc5594, scc5681);
lie->add_scc_dependance(scc5595, scc5466);
lie->add_scc_dependance(scc5595, scc5463);
lie->add_scc_dependance(scc5596, scc5599);
lie->add_scc_dependance(scc5596, scc5507);
lie->add_scc_dependance(scc5597, scc5466);
lie->add_scc_dependance(scc5597, scc5463);
lie->add_scc_dependance(scc5598, scc5609);
lie->add_scc_dependance(scc5598, scc5543);
lie->add_scc_dependance(scc5598, scc5519);
lie->add_scc_dependance(scc5599, scc5641);
lie->add_scc_dependance(scc5600, scc5469);
lie->add_scc_dependance(scc5602, scc5575);
lie->add_scc_dependance(scc5603, scc5670);
lie->add_scc_dependance(scc5604, scc5599);
lie->add_scc_dependance(scc5604, scc5507);
lie->add_scc_dependance(scc5606, scc5609);
lie->add_scc_dependance(scc5606, scc5543);
lie->add_scc_dependance(scc5606, scc5519);
lie->add_scc_dependance(scc5607, scc5670);
lie->add_scc_dependance(scc5608, scc5599);
lie->add_scc_dependance(scc5608, scc5507);
lie->add_scc_dependance(scc5609, scc5570);
lie->add_scc_dependance(scc5610, scc5679);
lie->add_scc_dependance(scc5610, scc5669);
lie->add_scc_dependance(scc5610, scc5659);
lie->add_scc_dependance(scc5610, scc5498);
lie->add_scc_dependance(scc5611, scc5681);
lie->add_scc_dependance(scc5612, scc5609);
lie->add_scc_dependance(scc5612, scc5543);
lie->add_scc_dependance(scc5612, scc5519);
lie->add_scc_dependance(scc5613, scc5609);
lie->add_scc_dependance(scc5613, scc5543);
lie->add_scc_dependance(scc5613, scc5519);
lie->add_scc_dependance(scc5614, scc5675);
lie->add_scc_dependance(scc5614, scc5673);
lie->add_scc_dependance(scc5614, scc5666);
lie->add_scc_dependance(scc5614, scc5461);
lie->add_scc_dependance(scc5616, scc5599);
lie->add_scc_dependance(scc5616, scc5507);
lie->add_scc_dependance(scc5617, scc5734);
lie->add_scc_dependance(scc5618, scc5670);
lie->add_scc_dependance(scc5620, scc5599);
lie->add_scc_dependance(scc5620, scc5507);
lie->add_scc_dependance(scc5621, scc5609);
lie->add_scc_dependance(scc5621, scc5543);
lie->add_scc_dependance(scc5621, scc5519);
lie->add_scc_dependance(scc5622, scc5609);
lie->add_scc_dependance(scc5622, scc5543);
lie->add_scc_dependance(scc5622, scc5519);
lie->add_scc_dependance(scc5623, scc5609);
lie->add_scc_dependance(scc5623, scc5543);
lie->add_scc_dependance(scc5623, scc5519);
lie->add_scc_dependance(scc5624, scc5676);
lie->add_scc_dependance(scc5624, scc5602);
lie->add_scc_dependance(scc5624, scc5582);
lie->add_scc_dependance(scc5625, scc5681);
lie->add_scc_dependance(scc5626, scc5652);
lie->add_scc_dependance(scc5627, scc5679);
lie->add_scc_dependance(scc5627, scc5669);
lie->add_scc_dependance(scc5627, scc5659);
lie->add_scc_dependance(scc5627, scc5498);
lie->add_scc_dependance(scc5628, scc5679);
lie->add_scc_dependance(scc5628, scc5669);
lie->add_scc_dependance(scc5628, scc5659);
lie->add_scc_dependance(scc5628, scc5498);
lie->add_scc_dependance(scc5629, scc5466);
lie->add_scc_dependance(scc5629, scc5463);
lie->add_scc_dependance(scc5630, scc5609);
lie->add_scc_dependance(scc5630, scc5543);
lie->add_scc_dependance(scc5630, scc5519);
lie->add_scc_dependance(scc5631, scc5599);
lie->add_scc_dependance(scc5631, scc5507);
lie->add_scc_dependance(scc5632, scc5638);
lie->add_scc_dependance(scc5632, scc5539);
lie->add_scc_dependance(scc5633, scc5510);
lie->add_scc_dependance(scc5634, scc5466);
lie->add_scc_dependance(scc5634, scc5463);
lie->add_scc_dependance(scc5635, scc5609);
lie->add_scc_dependance(scc5635, scc5543);
lie->add_scc_dependance(scc5635, scc5519);
lie->add_scc_dependance(scc5636, scc5466);
lie->add_scc_dependance(scc5636, scc5463);
lie->add_scc_dependance(scc5638, scc5681);
lie->add_scc_dependance(scc5639, scc5609);
lie->add_scc_dependance(scc5639, scc5543);
lie->add_scc_dependance(scc5639, scc5519);
lie->add_scc_dependance(scc5640, scc5570);
lie->add_scc_dependance(scc5641, scc5534);
lie->add_scc_dependance(scc5642, scc5679);
lie->add_scc_dependance(scc5642, scc5669);
lie->add_scc_dependance(scc5642, scc5659);
lie->add_scc_dependance(scc5642, scc5498);
lie->add_scc_dependance(scc5643, scc5679);
lie->add_scc_dependance(scc5643, scc5669);
lie->add_scc_dependance(scc5643, scc5659);
lie->add_scc_dependance(scc5643, scc5498);
lie->add_scc_dependance(scc5645, scc5552);
lie->add_scc_dependance(scc5646, scc5670);
lie->add_scc_dependance(scc5647, scc5460);
lie->add_scc_dependance(scc5648, scc5599);
lie->add_scc_dependance(scc5648, scc5507);
lie->add_scc_dependance(scc5649, scc5670);
lie->add_scc_dependance(scc5650, scc5466);
lie->add_scc_dependance(scc5650, scc5463);
lie->add_scc_dependance(scc5651, scc5609);
lie->add_scc_dependance(scc5651, scc5543);
lie->add_scc_dependance(scc5651, scc5519);
lie->add_scc_dependance(scc5652, scc5632);
lie->add_scc_dependance(scc5653, scc5641);
lie->add_scc_dependance(scc5653, scc5570);
lie->add_scc_dependance(scc5654, scc5681);
lie->add_scc_dependance(scc5655, scc5670);
lie->add_scc_dependance(scc5656, scc5599);
lie->add_scc_dependance(scc5656, scc5507);
lie->add_scc_dependance(scc5657, scc5570);
lie->add_scc_dependance(scc5657, scc5536);
lie->add_scc_dependance(scc5658, scc5609);
lie->add_scc_dependance(scc5658, scc5543);
lie->add_scc_dependance(scc5658, scc5519);
lie->add_scc_dependance(scc5659, scc5570);
lie->add_scc_dependance(scc5660, scc5681);
lie->add_scc_dependance(scc5661, scc5609);
lie->add_scc_dependance(scc5661, scc5543);
lie->add_scc_dependance(scc5661, scc5519);
lie->add_scc_dependance(scc5662, scc5609);
lie->add_scc_dependance(scc5662, scc5543);
lie->add_scc_dependance(scc5662, scc5519);
lie->add_scc_dependance(scc5663, scc5609);
lie->add_scc_dependance(scc5663, scc5543);
lie->add_scc_dependance(scc5663, scc5519);
lie->add_scc_dependance(scc5665, scc5681);
lie->add_scc_dependance(scc5666, scc5600);
lie->add_scc_dependance(scc5667, scc5466);
lie->add_scc_dependance(scc5667, scc5463);
lie->add_scc_dependance(scc5668, scc5466);
lie->add_scc_dependance(scc5668, scc5463);
lie->add_scc_dependance(scc5669, scc5570);
lie->add_scc_dependance(scc5670, scc5681);
lie->add_scc_dependance(scc5671, scc5679);
lie->add_scc_dependance(scc5671, scc5669);
lie->add_scc_dependance(scc5671, scc5659);
lie->add_scc_dependance(scc5671, scc5498);
lie->add_scc_dependance(scc5672, scc5466);
lie->add_scc_dependance(scc5672, scc5463);
lie->add_scc_dependance(scc5673, scc5625);
lie->add_scc_dependance(scc5673, scc5600);
lie->add_scc_dependance(scc5675, scc5626);
lie->add_scc_dependance(scc5676, scc5570);
lie->add_scc_dependance(scc5677, scc5681);
lie->add_scc_dependance(scc5678, scc5466);
lie->add_scc_dependance(scc5678, scc5463);
lie->add_scc_dependance(scc5679, scc5467);
lie->add_scc_dependance(scc5680, scc5570);
lie->add_scc_dependance(scc5681, scc5737);
lie->add_scc_dependance(scc5681, scc5731);
lie->add_scc_dependance(scc5681, scc5728);
lie->add_scc_dependance(scc5681, scc5707);
lie->add_scc_dependance(scc5681, scc5704);
lie->add_scc_dependance(scc5681, scc5696);
lie->add_scc_dependance(scc5681, scc5674);
lie->add_scc_dependance(scc5681, scc5664);
lie->add_scc_dependance(scc5681, scc5644);
lie->add_scc_dependance(scc5681, scc5637);
lie->add_scc_dependance(scc5681, scc5619);
lie->add_scc_dependance(scc5681, scc5615);
lie->add_scc_dependance(scc5681, scc5605);
lie->add_scc_dependance(scc5681, scc5601);
lie->add_scc_dependance(scc5681, scc5593);
lie->add_scc_dependance(scc5681, scc5588);
lie->add_scc_dependance(scc5681, scc5574);
lie->add_scc_dependance(scc5681, scc5572);
lie->add_scc_dependance(scc5681, scc5571);
lie->add_scc_dependance(scc5681, scc5562);
lie->add_scc_dependance(scc5681, scc5557);
lie->add_scc_dependance(scc5681, scc5550);
lie->add_scc_dependance(scc5681, scc5549);
lie->add_scc_dependance(scc5681, scc5547);
lie->add_scc_dependance(scc5681, scc5545);
lie->add_scc_dependance(scc5681, scc5530);
lie->add_scc_dependance(scc5681, scc5528);
lie->add_scc_dependance(scc5681, scc5490);
lie->add_scc_dependance(scc5681, scc5483);
lie->add_scc_dependance(scc5681, scc5476);
lie->add_scc_dependance(scc5681, scc5470);
lie->add_scc_dependance(scc5681, scc5468);
lie->add_scc_dependance(scc5682, scc5466);
lie->add_scc_dependance(scc5682, scc5463);
lie->add_scc_dependance(scc5683, scc5570);
lie->add_scc_dependance(scc5684, scc5670);
lie->add_scc_dependance(scc5685, scc5681);
lie->add_scc_dependance(scc5686, scc5609);
lie->add_scc_dependance(scc5686, scc5543);
lie->add_scc_dependance(scc5686, scc5519);
lie->add_scc_dependance(scc5687, scc5679);
lie->add_scc_dependance(scc5687, scc5669);
lie->add_scc_dependance(scc5687, scc5659);
lie->add_scc_dependance(scc5687, scc5498);
lie->add_scc_dependance(scc5688, scc5698);
lie->add_scc_dependance(scc5689, scc5466);
lie->add_scc_dependance(scc5689, scc5463);
lie->add_scc_dependance(scc5690, scc5466);
lie->add_scc_dependance(scc5690, scc5463);
lie->add_scc_dependance(scc5691, scc5599);
lie->add_scc_dependance(scc5691, scc5507);
lie->add_scc_dependance(scc5692, scc5670);
lie->add_scc_dependance(scc5693, scc5466);
lie->add_scc_dependance(scc5693, scc5463);
lie->add_scc_dependance(scc5694, scc5670);
lie->add_scc_dependance(scc5695, scc5466);
lie->add_scc_dependance(scc5695, scc5463);
lie->add_scc_dependance(scc5697, scc5609);
lie->add_scc_dependance(scc5697, scc5543);
lie->add_scc_dependance(scc5697, scc5519);
lie->add_scc_dependance(scc5698, scc5681);
lie->add_scc_dependance(scc5699, scc5679);
lie->add_scc_dependance(scc5699, scc5669);
lie->add_scc_dependance(scc5699, scc5659);
lie->add_scc_dependance(scc5699, scc5498);
lie->add_scc_dependance(scc5700, scc5466);
lie->add_scc_dependance(scc5700, scc5463);
lie->add_scc_dependance(scc5701, scc5681);
lie->add_scc_dependance(scc5702, scc5609);
lie->add_scc_dependance(scc5702, scc5543);
lie->add_scc_dependance(scc5702, scc5519);
lie->add_scc_dependance(scc5703, scc5599);
lie->add_scc_dependance(scc5703, scc5507);
lie->add_scc_dependance(scc5705, scc5681);
lie->add_scc_dependance(scc5706, scc5570);
lie->add_scc_dependance(scc5708, scc5679);
lie->add_scc_dependance(scc5708, scc5669);
lie->add_scc_dependance(scc5708, scc5659);
lie->add_scc_dependance(scc5708, scc5498);
lie->add_scc_dependance(scc5709, scc5670);
lie->add_scc_dependance(scc5710, scc5609);
lie->add_scc_dependance(scc5710, scc5543);
lie->add_scc_dependance(scc5710, scc5519);
lie->add_scc_dependance(scc5711, scc5609);
lie->add_scc_dependance(scc5711, scc5543);
lie->add_scc_dependance(scc5711, scc5519);
lie->add_scc_dependance(scc5712, scc5599);
lie->add_scc_dependance(scc5712, scc5507);
lie->add_scc_dependance(scc5713, scc5681);
lie->add_scc_dependance(scc5714, scc5657);
lie->add_scc_dependance(scc5715, scc5599);
lie->add_scc_dependance(scc5715, scc5507);
lie->add_scc_dependance(scc5716, scc5670);
lie->add_scc_dependance(scc5717, scc5670);
lie->add_scc_dependance(scc5718, scc5609);
lie->add_scc_dependance(scc5718, scc5543);
lie->add_scc_dependance(scc5718, scc5519);
lie->add_scc_dependance(scc5719, scc5466);
lie->add_scc_dependance(scc5719, scc5463);
lie->add_scc_dependance(scc5720, scc5679);
lie->add_scc_dependance(scc5720, scc5669);
lie->add_scc_dependance(scc5720, scc5659);
lie->add_scc_dependance(scc5720, scc5498);
lie->add_scc_dependance(scc5721, scc5676);
lie->add_scc_dependance(scc5721, scc5602);
lie->add_scc_dependance(scc5721, scc5582);
lie->add_scc_dependance(scc5722, scc5679);
lie->add_scc_dependance(scc5722, scc5669);
lie->add_scc_dependance(scc5722, scc5659);
lie->add_scc_dependance(scc5722, scc5498);
lie->add_scc_dependance(scc5723, scc5599);
lie->add_scc_dependance(scc5723, scc5507);
lie->add_scc_dependance(scc5724, scc5679);
lie->add_scc_dependance(scc5724, scc5669);
lie->add_scc_dependance(scc5724, scc5659);
lie->add_scc_dependance(scc5724, scc5498);
lie->add_scc_dependance(scc5725, scc5599);
lie->add_scc_dependance(scc5725, scc5507);
lie->add_scc_dependance(scc5726, scc5679);
lie->add_scc_dependance(scc5726, scc5669);
lie->add_scc_dependance(scc5726, scc5659);
lie->add_scc_dependance(scc5726, scc5498);
lie->add_scc_dependance(scc5727, scc5570);
lie->add_scc_dependance(scc5729, scc5609);
lie->add_scc_dependance(scc5729, scc5543);
lie->add_scc_dependance(scc5729, scc5519);
lie->add_scc_dependance(scc5730, scc5609);
lie->add_scc_dependance(scc5730, scc5543);
lie->add_scc_dependance(scc5730, scc5519);
lie->add_scc_dependance(scc5732, scc5466);
lie->add_scc_dependance(scc5732, scc5463);
lie->add_scc_dependance(scc5733, scc5679);
lie->add_scc_dependance(scc5733, scc5669);
lie->add_scc_dependance(scc5733, scc5659);
lie->add_scc_dependance(scc5733, scc5498);
lie->add_scc_dependance(scc5734, scc5520);
lie->add_scc_dependance(scc5735, scc5670);
lie->add_scc_dependance(scc5736, scc5679);
lie->add_scc_dependance(scc5736, scc5669);
lie->add_scc_dependance(scc5736, scc5659);
lie->add_scc_dependance(scc5736, scc5498);



  // Enable IO
 lie->enable_share_io();
 lie->set_output_dir("./benchmark/"); // Write to this directory
 lie->set_comm(mcomm);
 lie->set_batch_size(1);
 lie->execute();
 lie->print_all_relation_size(); // Continuously print relation sizes

 delete lie;
 mcomm.destroy();
 return 0;
}
