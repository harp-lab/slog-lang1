/*
 * A wrapper for the Google B-tree that makes it easy to test our map against google's.
 */

//#include <iostream> //for testing
//#include <bitset> //for testing
#pragma once
#include "../compat.h" //for u16, etc

template<class V>
class shmap
{
private:
  btree::btree_map<u64, V> root_ptr;
  
public:
  inline shmap()
  {
  }

  inline ~shmap()
  {
  }

  inline V* find(u64 key)
  {
    auto it = root_ptr.find(key);
    if (it != root_ptr.end())
      return &it->second;
    return 0;
  }

  inline void insert(u64 key, const V& val)
  {
    root_ptr.insert(std::pair<u64,V>(key, val));
  }

  inline void clear()
  {
    root_ptr.erase(root_ptr.begin(), root_ptr.end());
  }

  class iter
  {
  private:
    typename btree::btree_map<u64,V>* self;
    typename btree::btree_map<u64,V>::iterator it;
  
  public:
    inline iter(typename btree::btree_map<u64,V>* _slf, const typename btree::btree_map<u64,V>::iterator& _it)
      : self(_slf), it(_it)
    {
    }

    inline ~iter()
    {
    }

    inline u64 key() 
    {
      return it->first;
    }

    inline V val() 
    {
      return it->second;
    }

    inline operator bool() const
    { 
      return (it != self->end());
    }
  
    inline void next()
    {
      ++it;
    }
  };

  iter begin()
  {
    return iter(&root_ptr, root_ptr.begin());
  }
};




