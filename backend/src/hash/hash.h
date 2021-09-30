/*
 * Hash function (used by both the backend and the frontend)
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */


#pragma once


/// Based on the FNV-1a hash function
inline u64 tuple_hash(const u64* start_ptr, u64 prefix_len)
{
    const u64 base = 14695981039346656037ULL;
    const u64 prime = 1099511628211ULL;

    u64 hash = base;
    for (u64 i = 0; i < prefix_len; ++i)
    {
        u64 chunk = start_ptr[i];
        hash ^= chunk & 255ULL;
        hash *= prime;
        for (char j = 0; j < 7; ++j)
        {
            chunk = chunk >> 8;
            hash ^= chunk & 255ULL;
            hash *= prime;
        }
    }
    return hash;
}
