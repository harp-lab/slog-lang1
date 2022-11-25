#pragma once

#include <stdint.h>
#include <sys/stat.h>
#include <errno.h>
#include <limits.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <fcntl.h>
#include <unistd.h>
#include <iostream>
#include <fstream>
#include <mpi.h>
#include <vector>
#include <unordered_set>
#include <set>
#include <map>
#include <queue>
#include <unordered_map>
#include <tuple>
#include "btree/btree_map.h"
#include "btree/btree_set.h"
#include <filesystem>
#include <optional>
#include <bit>


#ifdef __GNUC__

typedef int8_t s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int64_t s64;
typedef char c8;
typedef wchar_t c16;
#else
#error No compat declarations for this compiler
#endif
