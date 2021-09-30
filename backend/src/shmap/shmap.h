/*
 * The Slog Hybrid Map a templated "u64 -> V" map
 * non-thread-safe, insertion only
 *
 * see test.cpp for usage
 *
 * Copyright (C) Thomas Gilray, et al, 2020.
 */

//#include <iostream> //for testing
//#include <bitset> //for testing
#pragma once
//#include "../parallel_RA_inc.h" //for u16, etc
#include "../compat.h" //for u16, etc

#include "strie/strie.h"
#include "sbtree/sbtree.h"

template<class V>
class shmap
{
private:
  strie<sbtree_root<u32,V>> root_ptr;
  
public:
  inline shmap()
  {
  }

  inline ~shmap()
  {
  }

  inline V* find(u64 key)
  {
    const u32 top = (u32)(key >> 32);
    const u32 bot = (u32)key;
    sbtree_root<u32,V>* subtree = root_ptr.find(top);
    if (subtree == 0) return 0;
    return subtree->find(bot);
  }

  inline void insert(u64 key, const V& val)
  {
    const u32 top = (u32)(key >> 32);
    const u32 bot = (u32)key;
    sbtree_root<u32,V>* subtree = root_ptr.find_or_insert(top);
    subtree->insert(bot, val);
  }

  inline void clear()
  {
    for (auto it = root_ptr.begin(); it; it.next())
    {
      sbtree_root<u32,V>* subtree = it.val();
      if (subtree) subtree->clear();
    }
    root_ptr.clear();
  }

  class iter
  {
  private:
    typename strie<sbtree_root<u32,V>>::iter iter0;
    typename sbtree_root<u32,V>::iter iter1;
  
  public:
    inline iter(const typename strie<sbtree_root<u32,V>>::iter& top_iter)
      : iter0(top_iter)
    {
      if (iter0)
        iter1 = iter0.val()->begin();
    }

    inline ~iter()
    {
    }

    inline u64 key() 
    {
      return ((u64)iter0.key() << 32) | iter1.key(); 
    }

    inline V val() 
    {
      return iter1.val();
    }

    inline operator bool() const
    { 
      return (bool)iter1;
    }
  
    inline void next()
    {
      iter1.next();
      if (!iter1)
      {
        iter0.next();
        if (!iter0)
          return;
        iter1 = iter0.val()->begin();
      }
    }
  };

  inline iter begin()
  {
    return iter(root_ptr.begin());
  }
};




