
#include <iostream>
#include "sbtree.h"

#include <cstdlib>

int main()
{
  // as a set of u32 values
  for (u32 j = 0; j < 2; ++j)
  {
    sbtree_root<u32, void> t;
    t.insert(0); 
    for (u32 i = 0; i < 250000; ++i)
    {
      u32 x = (rand()%2999999999) + 999999999;
      t.insert(x);
    }
    for (u32 i = 300000; i>0; i--)
      t.insert(i);
    for (u32 i = 20000; i>0; i--)
      t.insert(i);
    for (u32 i = 1000; i < 100000; ++i)
      if (!t.find(i))
      {
        std::cout << "error! not found" << std::endl;
        exit(1);
      }
    for (u32 i = 0xffffffff; i > 0xfeffffff; ++i)
      if (t.find(i))
      {
        std::cout << "error! found" << std::endl;
        exit(1);
      }
  
    u64 count = 0;
    u16 last = 0;
    for (auto it = t.begin(); it; it.next())
    {
      if (it.key() >= last)
        last = it.key(); // it.val() gets each value when non-void
      else
      {
        std::cout << "error: out of proper order!\n" << "  " << last << "<-->" << it.key() << std::endl;
        *((int*)((void*)0)) = 1;
      }
      ++count;
    }
    t.clear();
  }

  // as a map from u32->void*
  for (u32 j = 0; j < 2; ++j)
  {
    sbtree_root<u32, void*> t;
    t.insert(0,0); 
    for (u32 i = 0; i < 25000; ++i)
    {
      u32 x = (rand()%2999999999) + 999999999;
      t.insert(x, (void*)((u64)x));
    }
    for (u32 i = 20000; i>0; i--)
    {
      t.insert(i, (void*)((u64)i));
      if (i%50==0)
      for (auto it = t.begin(); it; it.next())
        if (it.key() != (u32)((u64)it.val()))
        {
          std::cout << "broke at i=" << i << std::endl;
          exit(1);
        }
    }
    for (u32 i = 300000; i>0; i--)
      t.insert(i, (void*)((u64)i));
    for (u32 i = 1000; i < 10000; ++i)
      if ((!t.find(i)) || *t.find(i) != (void*)((u64)i))
      {
        std::cout << (u64)t.find(i) << std::endl;
        std::cout << i << std::endl;
        std::cout << "error! not found or wrong" << std::endl;
        exit(1);
      }
    for (u32 i = 0xffffffff; i > 0xfeffffff; ++i)
      if (t.find(i))
      {
        std::cout << "error! found" << std::endl;
        exit(1);
      }
  
    u64 count = 0;
    u16 last = 0;
    for (auto it = t.begin(); it; it.next())
    {
      if (it.key() >= last && it.key() == ((u64)it.val()))
        last = it.key(); // it.val() gets each value when non-void
      else
      {
        std::cout << "error: out of proper order or wrong value!\n" << "  " << last << "<-->" << it.key() << std::endl;
        *((int*)((void*)0)) = 1;
      }
      ++count;
    }
    t.clear();
  }  
  return 0;
}
