/*
 * The Slog compressed trie datastructures, and hybrid trie+btree data structures
 * non-thread-safe, insertion only
 *
 * see test.cpp for usage
 *
 * Copyright (C) Thomas Gilray, et al, 2020.
 */

//#include <iostream> //for testing
//#include <bitset> //for testing
#pragma once
//#include "../../parallel_RA_inc.h" //for u16, etc
#include "../../compat.h" //for u16, etc

template<u16 W, class V>
class strie_node
{
private:
  struct singleton
  {
    u32 ki;
    V data;
  };
  
  struct fullnode 
  {
    u32 bm[(1U << W)/32];
    V data[1];
    fullnode()
      : bm{0}, data() {}
    
    inline bool exists(const u32 i)
    {
      return bm[i/32] & (1U << (i%32));
    }

    inline void turnon(const u32 i)
    {
      bm[i/32] |= (1U << (i%32));
    }

    inline u32 indexof(const u32 i)
    {
      u32 total = 0;
      for (u32 j = 0; j < i/32; ++j)
        total += __builtin_popcount(bm[j]);
      return total + __builtin_popcount((bm[i/32] << 1) << (31-i));
    }

    inline u64 indexof_and_size(const u32 i)
    {
      u32 postsum = 0;
      u32 presum = 0;
      for (u32 j = 0; j < (1U << W)/32; ++j)
      {
        if (j < i/32)
          presum += __builtin_popcount(bm[j]);
        else
          postsum += __builtin_popcount(bm[j]);
      }
      
      return ((u64)(presum + __builtin_popcount((bm[i/32] << 1) << (31-i))) << 32)
               | (u64)(presum+postsum);
    }
  };
  
  union Ptr
  {
  public:
    fullnode* node;
    singleton* single;
    u64 bm;
  };
  
  Ptr ptr; 
  
public:
  inline strie_node()
  {
    ptr.bm=0;
  }

  inline ~strie_node()
  {
  }

  operator bool()
  {
    return ptr.bm != 0;
  }

  strie_node<W,V>& operator =(const strie_node<W,V>& other)
  {
    ptr.bm = other.ptr.bm;
    return *this;
  }

  inline V* find_or_insert(const u32 ki)
  {
    if (ptr.bm == 0)
      { /* empty */
      ptr.single = new singleton();
      ptr.single->ki = ki;
      auto sng = ptr.single;
      ptr.bm |= 2;
      sng->data = V();
      return &sng->data;
    }
    else if (ptr.bm & 2)
    { /* singleton */
      ptr.bm = ptr.bm & 0xfffffffffffffffd;
      singleton* sng = ptr.single;
      if (sng->ki == ki)
      {
        ptr.bm |= 2;
        return &sng->data;
      }
      
      fullnode* node = (fullnode*)calloc(1,sizeof(fullnode) + 7*sizeof(V));
      
      new (node) fullnode();
      node->turnon(ki);
      node->turnon(sng->ki);
      V* ret = 0;
      if (ki < sng->ki)
      {
        node->data[0] = V();
        node->data[1] = sng->data;
        ret = &node->data[0];
      }
      else
      {
        node->data[0] = sng->data;
        node->data[1] = V();
        ret = &node->data[1];
      }
      delete sng;
      ptr.node = node;
      return ret;
    }
    else
    { /* full node */
      const u64 ind_size = ptr.node->indexof_and_size(ki);
      u32 count = (u32)ind_size;
      const u32 ind = (u32)(ind_size >> 32);
      if (ptr.node->exists(ki))
        return &ptr.node->data[ind];
      else
      {
        for (u32 j = count; j > ind; --j)
          ptr.node->data[j] = ptr.node->data[j-1];
        ptr.node->data[ind] = V();
        ptr.node->turnon(ki);
        
        if (count > 6 && __builtin_popcount(count+1)==1)
        {
          ++count;
          
          /* reallocate */
          fullnode* bignode = (fullnode*)calloc(1,sizeof(fullnode) + (count<<1)*sizeof(V) - sizeof(V));
          memcpy((void*)bignode, (void*)ptr.node, sizeof(fullnode) - sizeof(V) + count*sizeof(V));
          free(ptr.node);
          ptr.node = bignode;
        }
        
        return &ptr.node->data[ind];
      }
    }
  }

  inline V* find(const u32 ki)
  {
    if (ptr.bm == 0)
      return 0;
    if (ptr.bm & 2)
    {
      ptr.bm = ptr.bm & 0xfffffffffffffffd;
      singleton* sng = ptr.single;
      ptr.bm |= 2;
      if (sng->ki == ki)
        return &sng->data;
      return 0;
    }
    else
    {
      if (ptr.node->exists(ki))
        return &ptr.node->data[ptr.node->indexof(ki)];
      else
        return 0;
    }
  }

  inline void insert(const u32 ki, V v)
  {
    V* vptr = find_or_insert(ki);
    *vptr = v; 
  }

  class iter
  {
  private:
    Ptr ptr;
    u16 len;
    u16 ind;
    u32 ki;
  
  public:
    inline iter()
      : ptr(), len(0), ind(0), ki()
    {
      ptr.bm = 0;
    }
    
    inline iter(u64 _bm, u16 _len)
      : ptr(), len(_len), ind(0), ki(0)
    {
      ptr.bm = _bm;
      if (ptr.bm & 2)
      {
        ptr.bm &= 0xfffffffffffffffd;
        ki = ptr.single->ki;
        ptr.bm |= 2;
      }
      else if (ptr.bm)
      {
        while (ki < (1U << W) && !ptr.node->exists(ki)) ++ki;
        if (ki == (1U << W))
          std::cout << "Error: empty bignode found!" << std::endl;
      }
    }

    inline ~iter()
    {
    }

    inline u32 key() 
    {
      return ki;
    }

    inline V* val() 
    {
      if (ptr.bm & 2)
      {
        ptr.bm &= 0xfffffffffffffffd;
        V* val = &ptr.single->data;
        ptr.bm |= 2;
        return val;
      }
      else
        return &ptr.node->data[ind];
    }

    inline operator bool() const
    { 
      return ptr.bm != 0;
    }

    inline iter& operator=(const iter& other)
    {
      ptr.bm = other.ptr.bm;
      len = other.len;
      ind = other.ind;
      ki = other.ki;
      return *this;
    }
  
    inline void next()
    {
      if (ptr.bm & 2)
        ptr.bm = 0;
      else
      {
        while (++ki < (1U << W) && !ptr.node->exists(ki));
        ++ind;
        if (ind == len) ptr.bm = 0;
      }       
    }
  };

  inline iter begin()
  {
    if (ptr.bm & 2)
      return iter(ptr.bm, 1);
    else if (ptr.bm)
    {
      const u64 ind_size = ptr.node->indexof_and_size(0);
      const u32 count = (u32)ind_size;
      return iter(ptr.bm, count);
    }
    else
      return iter();
  }

  void clear()
  {
    if (ptr.bm & 2)
    {
      ptr.bm &= 0xfffffffffffffffd;
      delete ptr.single;
    }
    else if (ptr.node)
    {
      free(ptr.node);
    }
    ptr.bm = 0;
  }
};

template<class V>
class strie
{
private:
  strie_node<10,strie_node<11,strie_node<11,V>>> root_ptr;
  
public:
  inline strie()
  {
  }

  inline ~strie()
  {
  }

  inline V* find_or_insert(const u32 k)
  {
    strie_node<11,strie_node<11,V>>* lv2 = root_ptr.find_or_insert(k >> 22);
    strie_node<11,V>* lv3 = lv2->find_or_insert((k << 10) >> 21);
    return lv3->find_or_insert((k << 21) >> 21);
  }
  
  inline V* find(const u32 k)
  {
    auto lv2 = root_ptr.find(k >> 22);
    if (!lv2) return 0;
    auto lv3 = lv2->find((k << 10) >> 21);
    if (!lv3) return 0;
    return lv3->find((k << 21) >> 21);
  }

  inline void insert(const u32 k, const V v)
  {
    V* vptr = find_or_insert(k);
    *vptr = v;
  }

  void clear()
  {
    for (auto lv2it = root_ptr.begin(); lv2it; lv2it.next())
    {
      strie_node<11,strie_node<11,V>>* lv2 = lv2it.val();
      if (lv2)
      {
        for (auto lv3it = lv2->begin(); lv3it; lv3it.next())
        {
          strie_node<11,V>* lv3 = lv3it.val();
          if (lv3) lv3->clear();
        }
        lv2->clear();
      }
    }
    root_ptr.clear();
  }

  class iter
  {
  private:
    typename strie_node<10,strie_node<11,strie_node<11,V>>>::iter iter0;
    typename strie_node<11,strie_node<11,V>>::iter iter1;
    typename strie_node<11,V>::iter iter2;
  
  public:
    inline iter(const typename strie_node<10,strie_node<11,strie_node<11,V>>>::iter& top_iter)
    {
      iter0 = top_iter;
      if (iter0)
      {
        iter1 = iter0.val()->begin();
        if (iter1)
          iter2 = iter1.val()->begin();
      }
    }

    inline ~iter()
    {
    }

    inline u32 key() 
    {
      return (iter0.key() << 22)
        | (iter1.key() << 11)
        | iter2.key(); 
    }

    inline V* val() 
    {
      return iter2.val();
    }

    inline operator bool() const
    { 
      return (bool)iter2;
    }
  
    inline void next()
    {
      iter2.next();
      if (!iter2)
      {
        iter1.next();
        if (!iter1)
        {
          iter0.next();
          if (!iter0)
            return;
          iter1 = iter0.val()->begin();
        }
        iter2 = iter1.val()->begin();
      }
    }
  };

  iter begin()
  {
    return iter(root_ptr.begin());
  }
};





