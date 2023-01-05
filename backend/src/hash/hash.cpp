#include "hash.h"

#include "fasthash.h"
#include "spooky-c.h"
#include "xxhash.h"
#include <vector>

uint64_t tuple_hash(const uint64_t* start_ptr, uint64_t prefix_len)
{
    return fnv1a(start_ptr, prefix_len);
    // return MurmurHash64A(start_ptr, prefix_len*8, MURMUR_SEED);
    // return spooky_hash64(start_ptr, prefix_len*8, MURMUR_SEED);
    // return fasthash64(start_ptr, prefix_len*8, 10);
    // return XXH64(start_ptr, prefix_len*8, 10);
}

std::vector<uint64_t> tuple_hash_test_all(const uint64_t* start_ptr, uint64_t prefix_len) {
    std::vector<uint64_t> all_hash_v;
    all_hash_v.push_back(start_ptr[0]);
    all_hash_v.push_back(fnv1a(start_ptr, prefix_len));
    all_hash_v.push_back(hash64shift(start_ptr));
    all_hash_v.push_back(spooky_hash64(start_ptr, prefix_len*8, 1));
    all_hash_v.push_back(fasthash64(start_ptr, prefix_len*8, 1));
    // all_hash_v.push_back(XXH64(start_ptr, prefix_len*8, 1));
    all_hash_v.push_back(XXH32(start_ptr, prefix_len*4, 1));
    return all_hash_v;
}

