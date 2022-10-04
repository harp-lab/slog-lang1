/*
 *
 * Parallel Relational Algebra
 * Copyright (c) Sidharth Kumar, Thomas Gilray, Kristopher Micinski, see License.md
 *
 */


#pragma once

// #include <vector>
#include "ds.h"
#include "compat.h"
// #include "shmap/shmap.h"
#include "shmap/shmap_goog.h"
#include <functional>

//#define DEBUG_OUTPUT 1
#define MAX_LOOP_COUNT 120000

#include "log/logger.h"
#include "hash/hash.h"
#include "comm/comm.h"
#include "buffer/vector_buffer.h"
#include "IO/parallel_io.h"
#include "comm/all_to_allv_comm.h"
#include "comm/all_to_all_comm.h"
#include "relation/shmap_relation.h"

enum class SpecialAggregator {
  none,
  sum,
  count,
  maximum,
  minimum,
  recusive
};

// TODO: remove unused argument
//       rename global to reduce
using local_agg_res_t = u64;
// typedef local_agg_res_t *local_agg_func_t (shmap_relation& agg_rel, std::vector<u64>& data);
using local_agg_func_t = std::function<local_agg_res_t(std::pair<shmap_relation::iterator, shmap_relation::iterator> joined_range)>;
using reduce_agg_func_t = std::function<local_agg_res_t(local_agg_res_t, local_agg_res_t)>;
using global_agg_func_t = std::function<u64(local_agg_res_t a, local_agg_res_t b)>;
// typedef local_agg_res_t *reduce_agg_func_t (local_agg_res_t x, local_agg_res_t y);
// typedef int *global_agg_func_t (std::vector<u64>& data, local_agg_res_t agg_data, int agg_data_count, std::vector<u64>& output); 

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
