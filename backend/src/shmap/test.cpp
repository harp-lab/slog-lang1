
#include <iostream>

// Uncomment the first line to use our map, the second to use google's
//#include "shmap.h"
#include "shmap_goog.h"


#include <cstdlib>
#include "stdlib.h"
#include "stdio.h"
#include "string.h"

int parseLine(char* line){
  // This assumes that a digit will be found and the line ends in " Kb".
  int i = strlen(line);
  const char* p = line;
  while (*p <'0' || *p > '9') p++;
  line[i-3] = '\0';
  i = atoi(p);
  return i;
}

int getValue(){ //Note: this value is in KB!
  FILE* file = fopen("/proc/self/status", "r");
  int result = -1;
  char line[128];

  while (fgets(line, 128, file) != NULL){
    if (strncmp(line, "VmSize:", 7) == 0){
      result = parseLine(line);
      break;
    }
  }
  fclose(file);
  return result;
}

int main()
{
  for (u32 j = 0; j < 1; ++j)
  {
    shmap<void*> t;
    t.insert(0,0); 
    for (u64 i = 0; i < 299990; ++i)
    {
      u64 x = ((u64)(rand()%32) << 32) | ((u64)(rand()%256) << 44) | ((rand()%2999999999) + 999999999);
      t.insert(x, (void*)x);
      if (t.find(x) == 0 || *t.find(x) != (void*)x)
      {
          std::cout << "broke at x=" << x << std::endl;
          exit(1);
      };
    }
    for (u64 i = 999000; i>0; i--)
    {
      t.insert(i, (void*)((u64)i));
      if (i%500==0)
      for (auto it = t.begin(); it; it.next())
        if (it.key() != (u64)it.val())
        {
          std::cout << "broke at i=" << i << std::endl;
          exit(1);
        }
    }
    for (u64 i = 9999; i>0; i--)
      t.insert((i<<32), (void*)(i<<32));
    for (u64 i = 600000; i>0; i--)
      t.insert(i, (void*)i);
    for (u64 i = 1000; i < 10000; ++i)
      if ((!t.find(i)) || *t.find(i) != (void*)((u64)i))
      {
        std::cout << (u64)t.find(i) << std::endl;
        std::cout << i << std::endl;
        std::cout << "error! not found or wrong" << std::endl;
        exit(1);
      }
    for (u64 i = 0xffffffff; i > 0xfeffffff; --i)
      if (t.find(i))
      {
        std::cout << "error! found" << std::endl;
        exit(1);
      }
    
    u64 count = 0;
    u64 last = 0;
    for (auto it = t.begin(); it; it.next())
    {
      if (it.key() >= last && it.key() == (u64)it.val())
        last = it.key(); // it.val() gets each value when non-void
      else
      {
        std::cout << "error: out of proper order...:" << "  " << last << "<-->" << it.key() << std::endl;
        std::cout << "error: ...or wrong value...: " << "  " << it.key() << "<-->" << (u64)it.val() << std::endl;
        exit(1);
      }
      ++count;
    }
    std::cout << "Total Mem Usage: " << getValue()/1000 << std::endl;
    std::cout << "Clearing map! " << std::endl;
    t.clear();
    std::cout << "Done with test iter " << j << std::endl;
  }
  
  return 0;
}
