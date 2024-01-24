
#pragma once

#include "parallel_RA_inc.h"

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


std::vector<std::array<u64, 2>> builtin_div_rem(const u64 *const data);

#define BUILTIN_IMP_FUNC_DECL(name) \
    state_t name(const u64 *data, state_t init_state, builtin_impl_callback_t);

BUILTIN_IMP_FUNC_DECL(builtin_less);
BUILTIN_IMP_FUNC_DECL(builtin_greater);
BUILTIN_IMP_FUNC_DECL(builtin_le);
BUILTIN_IMP_FUNC_DECL(builtin_ge);

BUILTIN_IMP_FUNC_DECL(builtin_add)
BUILTIN_IMP_FUNC_DECL(builtin_subtract)
BUILTIN_IMP_FUNC_DECL(builtin_multiply)
BUILTIN_IMP_FUNC_DECL(builtin_divide)

BUILTIN_IMP_FUNC_DECL(builtin_arg2_minus_arg1)

BUILTIN_IMP_FUNC_DECL(builtin_add1)
BUILTIN_IMP_FUNC_DECL(builtin_add1_2)
BUILTIN_IMP_FUNC_DECL(builtin_sub1)
BUILTIN_IMP_FUNC_DECL(builtin_sub1_2)

std::vector<std::array<u64, 1>> builtin_range(const u64 *const data);
state_t callback_builtin_range(const u64 *data, state_t init_state, builtin_impl_callback_t callback);

BUILTIN_IMP_FUNC_DECL(builtin_eq)
BUILTIN_IMP_FUNC_DECL(builtin_neq)

state_t builtin_eq_1(const u64 *data, state_t init_state, builtin_impl_callback_t callback);

BUILTIN_IMP_FUNC_DECL(builtin_number_huh)
BUILTIN_IMP_FUNC_DECL(builtin_not_number_huh)

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
//  template<bool f (u64, u64)>
//  bool builtin_binary_number_pred(const u64* data){
//    if (is_number(data[0]) && is_number(data[1])){
//      return f(datum_to_number(data[0]), datum_to_number(data[1]));
//    } else {
//      return false;
//    }
//  }
//  bool _less(u64 x, u64 y) { return x < y;}
//  auto builtin_less2 = builtin_binary_number_pred<_less>;

// state_t builtin_nop(const u64* data, state_t init_state, state_t (*callback) (state_t state));

// inline builtin_func_t
// get_builtin_function_by_name (std::string fname) {
//     if (fname == "builtin_div_rem") {
//         return &builtin_div_rem;
//     }
// }

builtin_impl_t builtin_map(std::string& func_name);


// //////////////////// AGGREGATORS Alternative design ////////////////////

// TODO: add number type check
//////////////////////////////  count /////////////////////////////////////

local_agg_res_t agg_count_local(std::pair<shmap_relation::iterator, shmap_relation::iterator> joined_range);
local_agg_res_t agg_count_reduce(local_agg_res_t x, local_agg_res_t y);

//////////////////////////////  sum /////////////////////////////////////

local_agg_res_t agg_sum_local(std::pair<shmap_relation::iterator, shmap_relation::iterator> joined_range);

local_agg_res_t agg_sum_reduce(local_agg_res_t x, local_agg_res_t y);

//////////////////////////////  maximum  /////////////////////////////////////

local_agg_res_t agg_maximum_local(std::pair<shmap_relation::iterator, shmap_relation::iterator> joined_range);

local_agg_res_t agg_maximum_reduce(local_agg_res_t x, local_agg_res_t y);

//////////////////////////////  minimum  /////////////////////////////////////

local_agg_res_t agg_minimum_local(std::pair<shmap_relation::iterator, shmap_relation::iterator> joined_range);

local_agg_res_t agg_minimum_reduce(local_agg_res_t x, local_agg_res_t y);

// agg_func_t agg_map(std::string& agg_name);
reduce_agg_func_t agg_reduce_map(std::string& agg_name);
local_agg_func_t agg_local_map(std::string& agg_name);
// global_agg_func_t agg_global_map(std::string& agg_name);
