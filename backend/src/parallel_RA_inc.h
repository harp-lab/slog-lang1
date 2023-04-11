/*
 *
 * Parallel Relational Algebra
 * Copyright (c) Sidharth Kumar, Thomas Gilray, Kristopher Micinski, see License.md
 *
 */

#pragma once

// #include <vector>
#include "ast.h"
#include "compat.h"
#include "ds.h"
// #include "shmap/shmap.h"
#include "shmap/shmap_goog.h"
#include <functional>
#include <memory>

// #define DEBUG_OUTPUT 1
#define MAX_LOOP_COUNT 120000

#include "hash/hash.h"
#include "log/logger.h"

const u64 tag_mask = 0xffffc00000000000;
const u64 tag_position = 46;
const u64 int_tag = 0;
const u64 str_tag = 2;
const u64 sign_flip_const = 0x0000200000000000;
const u64 signed_num_mask = 0xFFFFE00000000000;

inline bool is_number(u64 datum) {
    // cout << "is_number(" << datum << "): " << (datum >> tag_position == int_tag) << "\n";
    return datum >> tag_position == int_tag;
}

inline i64 datum_to_number(u64 datum) {
    i64 signed_val = (datum & ~tag_mask) << (64 - tag_position) >> (64 - tag_position);
    if (signed_val >= sign_flip_const) {
        signed_val = sign_flip_const - signed_val;
    }
    return signed_val;
    // return (i64) (datum & ~tag_mask) << (64 - tag_position) >> (64 - tag_position);
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
    u64 str_hash = string_hash(str);
    return (str_hash & ~tag_mask) | (str_tag << tag_position);
}
const auto s2d = string_to_datum;

using state_t = std::tuple<const u64 *, u64 *, std::shared_ptr<slogc_ra_external_function>>;
using builtin_impl_callback_t = std::function<state_t(std::vector<u64> &res, state_t state)>;
using builtin_impl_t = std::function<state_t(const u64 *data, state_t init_state, builtin_impl_callback_t callback)>;

#include "IO/parallel_io.h"
#include "buffer/vector_buffer.h"
#include "comm/all_to_allv_comm.h"
#include "comm/comm.h"
#include "relation/shmap_relation.h"

enum class SpecialAggregator {
    none,
    sum,
    count,
    maximum,
    minimum,
    recursive
};

// TODO: remove unused argument
//       rename global to reduce
using local_agg_res_t = u64;
// typedef local_agg_res_t *local_agg_func_t (shmap_relation& agg_rel, std::vector<u64>& data);
using local_agg_func_t = std::function<local_agg_res_t(std::pair<shmap_relation::iterator, shmap_relation::iterator> joined_range)>;
using reduce_agg_func_t = std::function<local_agg_res_t(local_agg_res_t, local_agg_res_t)>;
using global_agg_func_t = std::function<u64(local_agg_res_t a, local_agg_res_t b)>;
class agg_func_t {
public:
    local_agg_func_t local_func;
    reduce_agg_func_t reduce_func;
    global_agg_func_t global_func;
    agg_func_t(local_agg_func_t local_func, reduce_agg_func_t reduce_func,
               global_agg_func_t global_func) :
        local_func(local_func), reduce_func(reduce_func), global_func(global_func){}
};
// typedef local_agg_res_t *reduce_agg_func_t (local_agg_res_t x, local_agg_res_t y);
// typedef int *global_agg_func_t (std::vector<u64>& data, local_agg_res_t agg_data, int agg_data_count, std::vector<u64>& output);

#include "builtin.h"

#include "relation/balanced_hash_relation.h"
#include "RA/parallel_RA.h"
#include "RA/fact.h"
#include "RA/parallel_join.h"
#include "RA/parallel_copy.h"
#include "RA/parallel_copy_filter.h"
#include "RA/parallel_copy_generate.h"
#include "RA/parallel_acopy.h"
#include "RA/parallel_agg.h"
#include "comm/intra_bucket_comm.h"
#include "RAM/RA_tasks.h"
#include "lie/lie.h"
//#include "lie/lie_multi_task.h"
#undef LOGGING
