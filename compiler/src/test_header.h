

#pragma once

#define u64  uint64_t

using local_agg_res_t = u64;

// struct _BTree {
//   virtual bool has_key(const u64* key) = 0;
// };

typedef local_agg_res_t *local_agg_func_t (shmap_relation* agg_rel, const u64* data);

typedef local_agg_res_t *reduce_agg_func_t (local_agg_res_t x, local_agg_res_t y);

typedef int *global_agg_func_t (u64* data, local_agg_res_t agg_data, int agg_data_count, u64* output);
