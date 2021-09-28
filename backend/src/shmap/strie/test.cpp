
#include <iostream>
#include "strie.h"
#include <cstdlib>

int main()
{/*
  for (u32 h = 0; h < 500; ++h){
  strie_node<11,void*> nd;
  for (u32 m = 0; m < 18; ++m)
    for (u32 q = 2048/(m+1); q > 0; --q)
  {
    nd.insert(q,(void*)(u64)q);
    if (q %24 == 0)
  for (auto it = nd.begin(); it; it.next())
    if (it.val() && it.key() != (u32)(u64)*it.val())
      { std::cout << "bad key or none during iter: " << it.key() << std::endl; exit(1);}
  }
  for (u32 m = 0; m < 10; ++m)
  for (auto it = nd.begin(); it; it.next())
    if (it.val() && it.key() != (u32)(u64)*it.val())
      { std::cout << "bad key or none during iter: " << it.key() << std::endl; exit(1);}
  for (u32 q = 0; q < 2048; ++q)
    if (nd.find(q) && *nd.find(q) != (void*)(u64)q)
      { std::cout << "!! q=" << q << "  find()=" << (u64)*nd.find(q) << std::endl; exit(1);}
  nd.clear();}
  exit(0);*/
  
  for (u32 j = 0; j < 2; ++j)
  {
    strie<void*> t;
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
        if (it.key() != (u32)((u64)*it.val()))
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
    u32 last = 0;
    for (auto it = t.begin(); it; it.next())
    {
      if (it.key() >= last && it.key() == ((u64)*it.val()))
        last = it.key(); // it.val() gets each value when non-void
      else
      {
        std::cout << "error: out of proper order...:" << "  " << last << "<-->" << it.key() << std::endl;
        std::cout << "error: ...or wrong value...: " << "  " << it.key() << "<-->" << (u64)*it.val() << std::endl;
        exit(1);
      }
      ++count;
    }
    std::cout << "Clearing map! " << std::endl;
    t.clear();
    std::cout << "Done with test iter " << j << std::endl;
  }
  
  return 0;
}
