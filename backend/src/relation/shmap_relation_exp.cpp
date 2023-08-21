/**
 * @file btree_relation.cpp
 * @author Yihao Sun (ysun67@syr.edu)
 * @brief the implmentation of slog relation using souffle's btree,
 *        reload all fucntion in original shmap, just to keep other part code working...
 * @version 0.1
 * @date 2021-12-15
 * 
 * @copyright Yihao Sun Copyright (c) 2021
 * 
 */

#include "../parallel_RA_inc.h"
#include "shmap_relation.h"
#include <cstddef>
#include <iostream>



shmap_relation::shmap_relation(int arity, bool id_flag)
{
    this->arity = arity;
    // ind = new t_ind(t_comparator(id_flag));
    this->id_flag = id_flag;
}

bool shmap_relation::insert_tuple_from_array(u64 *t, int width)
{
    t_tuple tp(t, t+width);

    return insert(tp);
}

std::pair<shmap_relation::iterator, shmap_relation::iterator>
shmap_relation::prefix_range(std::vector<u64> &prefix)
{
    if (prefix.size() >= arity+1)
        return std::make_pair(end(), end());
    t_tuple upper_bound(arity+1, std::numeric_limits<u64>::max());
    t_tuple lower_bound(arity+1, std::numeric_limits<u64>::min());
    for(size_t i = 0; i < prefix.size(); i++)
    {
        upper_bound[i] = prefix[i];
        lower_bound[i] = prefix[i];
    }
    return lowerUpperRange(lower_bound, upper_bound);
}


int shmap_relation::count()
{
    return size();
}

void shmap_relation::remove_tuple()
{
    this->purge();
}

bool shmap_relation::find_tuple_from_array(u64 *t, int width)
{
    // t_tuple upper_bound(arity+1, std::numeric_limits<u64>::max());
    // t_tuple lower_bound(arity+1, std::numeric_limits<u64>::min());
    // for(size_t i = 0; i < width; i++)
    // {
    //     upper_bound[i] = t[i];
    //     lower_bound[i] = t[i];
    // }
    t_tuple tp(t, t+width);
    auto joined_range = prefix_range(tp);
    if (joined_range.first == ind.end()) {
        return false;
    }

    return true;
}

// NOTE: prefix in this function is useless and also actually never use in other code
void shmap_relation::as_vector_buffer_recursive(vector_buffer *vb, std::vector<u64> prefix)
{
    if (size() == 0)
    {
        return;
    }
    for (const auto &cur_path : ind)
    {
        u64 path[cur_path.size()];
        for (u32 i = 0; i < cur_path.size(); i++)
        {
            path[i] = cur_path[i];
        }
        vb->vector_buffer_append((const unsigned char*)path, sizeof(u64)*cur_path.size());
    }
}
