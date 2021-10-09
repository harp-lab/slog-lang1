/*
 *
 * Parallel Relational Algebra
 * Copyright (c) Sidharth Kumar, Thomas Gilray, Kristopher Micinski, see License.md
 *
 */


#pragma once

#include "ds.h"
#include "compat.h"
//#include "shmap/shmap.h"
#include "shmap/shmap_goog.h"


#define DEBUG_OUTPUT 1

#define MAX_LOOP_COUNT 40000


#include "log/logger.h"
#include "hash/hash.h"
#include "comm/comm.h"
#include "buffer/vector_buffer.h"
#include "IO/parallel_io.h"
#include "comm/all_to_allv_comm.h"
#include "comm/all_to_all_comm.h"
#include "relation/google_btree_relation.h"
#include "relation/shmap_relation.h"



#include "relation/balanced_hash_relation.h"
#include "RA/parallel_RA.h"
#include "RA/fact.h"
#include "RA/parallel_join.h"
#include "RA/parallel_copy.h"
#include "RA/parallel_copy_filter.h"
#include "RA/parallel_copy_generate.h"
#include "RA/parallel_acopy.h"
#include "comm/intra_bucket_comm.h"
#include "RAM/RA_tasks.h"
#include "lie/lie.h"
//#include "lie/lie_multi_task.h"



#undef LOGGING
