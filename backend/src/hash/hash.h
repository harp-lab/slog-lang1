/*
 * Hash function (used by both the backend and the frontend)
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */


#pragma once


/// Based on the FNV-1a hash function

#include <cstdint>
#include <string>
#include <vector>
// #include <endian.h>
#define MURMUR_SEED 7917

///FNV-1a
inline uint64_t fnv1a(const uint64_t* start_ptr, uint64_t prefix_len)
{
    const uint64_t base = 14695981039346656037ULL;
    const uint64_t prime = 1099511628211ULL;

    uint64_t hash = base;
    for (uint64_t i = 0; i < prefix_len; ++i)
    {
        uint64_t chunk = start_ptr[i];
        hash ^= chunk & 255ULL;
        hash *= prime;
        for (char j = 0; j < 7; ++j)
        {
            chunk = chunk >> 8;
            hash ^= chunk & 255ULL;
            if ((chunk & 255ULL) == 0)
              continue;
            hash *= prime;
        }
    }
    return hash;
}

inline uint64_t nonhash1(const uint64_t* start_ptr, uint64_t prefix_len)
{
    // range base split on first column,
    return start_ptr[0]; 
}


// murmurhash
#if defined(_MSC_VER)

#define BIG_CONSTANT(x) (x)

// Other compilers

#else	// defined(_MSC_VER)

#define BIG_CONSTANT(x) (x##LLU)

#endif // !defined(_MSC_VER)

static inline uint64_t getblock ( const uint64_t * p )
{
#if defined(__BYTE_ORDER__) && (__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__)
  return *p;
#else
  const uint8_t *c = (const uint8_t *)p;
  return (uint64_t)c[0] |
	 (uint64_t)c[1] <<  8 |
	 (uint64_t)c[2] << 16 |
	 (uint64_t)c[3] << 24 |
	 (uint64_t)c[4] << 32 |
	 (uint64_t)c[5] << 40 |
	 (uint64_t)c[6] << 48 |
	 (uint64_t)c[7] << 56;
#endif
}

inline uint64_t MurmurHash64A ( const void * key, int len, uint64_t seed )
{
  const uint64_t m = BIG_CONSTANT(0xc6a4a7935bd1e995);
  const int r = 47;

  uint64_t h = seed ^ (len * m);

  const uint64_t * data = (const uint64_t *)key;
  const uint64_t * end = data + (len/8);

  while(data != end)
  {
    uint64_t k = getblock(data++);

    k *= m; 
    k ^= k >> r; 
    k *= m; 
    
    h ^= k;
    h *= m; 
  }

  const unsigned char * data2 = (const unsigned char*)data;

  switch(len & 7)
  {
  case 7: h ^= uint64_t(data2[6]) << 48;
  case 6: h ^= uint64_t(data2[5]) << 40;
  case 5: h ^= uint64_t(data2[4]) << 32;
  case 4: h ^= uint64_t(data2[3]) << 24;
  case 3: h ^= uint64_t(data2[2]) << 16;
  case 2: h ^= uint64_t(data2[1]) << 8;
  case 1: h ^= uint64_t(data2[0]);
          h *= m;
  };
 
  h ^= h >> r;
  h *= m;
  h ^= h >> r;

  return h;
} 

inline uint64_t hash64shift(const uint64_t* keys)
{
  uint64_t key = keys[0];
  key = (~key) + (key << 21); // key = (key << 21) - key - 1;
  key = key ^ (key >> 24);
  key = (key + (key << 3)) + (key << 8); // key * 265
  key = key ^ (key >> 14);
  key = (key + (key << 2)) + (key << 4); // key * 21
  key = key ^ (key >> 28);
  key = key + (key << 31);
  return key;
}


uint64_t tuple_hash(const uint64_t* start_ptr, uint64_t prefix_len);
std::vector<uint64_t> tuple_hash_test_all(const uint64_t* start_ptr, uint64_t prefix_len);

// change this to compile time?
inline uint32_t string_hash(const std::string& str) {
    const uint32_t base = 2166136261u;
    const uint32_t prime = 16777619u;

    uint32_t hash = base;
    for (char c: str)
    {
        if ((int)c == 0) continue;
        hash ^= (int)c;
        hash *= prime;
    }
    return hash;
}

