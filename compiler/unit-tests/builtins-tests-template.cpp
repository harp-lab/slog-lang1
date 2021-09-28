
// LINK compile-test-file-generator.rkt
// LINK ./output/builtins-tests-generated.cpp
#include <vector>
#include <string>
#include <unordered_map>
#include <functional>
#include <cassert>
#include <iostream>
#include <array>

#include "../../src/builtins.cpp"

int main(){
  {
    // (srule
    //     ((rel-select bar 3 (1 2 3)) x y y+x w 42)
    //     ((rel-version foo 3 (2 1) total) y x w _2)
    //     ((rel-version + 3 (2 1) total) y x _1 y+x))
    auto lam = ~a;
    u64 y = 5, x = 4, w = 100;
    auto input = vector<u64> {n2d(y), n2d(x), n2d(w)};
    u64 out[1000];
    auto res = lam(input.data(), out);
    // cout << "out[0], out[1], out[2]: " << out[0] << ", " << out[1] << ", " << out[2] << "\n";
    // cout << "res: " << res << "\n";
    assert(out[0] == n2d(x) && out[1] == n2d(y) && 
           out[2] == n2d(y+x) && out[3] == n2d(w) && 
           out[4] == n2d(42));
    assert(res == 1);
  }
  
  {
    // (srule
    //   ((rel-select bar 2 (1 2)) w ans)
    //   ((rel-version foo 3 (1 2) total) x y _2 w)
    //   ((rel-version range 3 (1 2) total) x y _1 ans))
    auto lam = ~a;
    u64 x = n2d(11), y = n2d(14), _2 = 10000, w = n2d(5);
    auto input = vector<u64> { x, y, _2, w};
    u64 out[1000];
    auto res = lam(input.data(), out);
    assert(res == 3);
    int base = 0;
    assert(out[base+ 0] == n2d(5) && out[base+ 1] == n2d(11));
    base = 2;
    assert(out[base+ 0] == n2d(5) && out[base+ 1] == n2d(12));
    
  }

  {
    /* (srule
        ((rel-select bar 5 (1 2)) y x 42 rem div)
        ((rel-version foo 5 (1 2 3) total) x y rem _2 w1 w2)
        ((rel-version div-rem 4 (1 2 4) total) x y rem _1 div)) */
    auto lam = ~a;
    u64 x = n2d(20), y = n2d(7), rem = n2d(6),
         w1 = n2d(101), w2 = n2d(102), _2 = 1000000, div = n2d(2);
    auto input = vector<u64> { x, y, rem, _2, w1, w2};
    u64 out[1000];
    auto res = lam(input.data(), out);
    assert(res == 1);
    assert(out[0] == y && out[1] == x && out[2] == n2d(42) &&
           out[3] == rem && out[4] == div);
  }

  {
    /* (srule
          ((rel-select bar 3 (1 2)) y z 42)
          ((rel-version foo 3 (1 2 3) total) x y z)
          ((rel-version =/= 2 (1 2) total) x y)) */
    auto lam = ~a;
    {
      u64 x = n2d(20), y = n2d(7), z = n2d(6);         
      auto input = vector<u64> { x, y, z};
      u64 out[1000];
      auto res = lam(input.data(), out);
      assert(res == 1);
      assert(out[0] == y && out[1] == z && out[2] == n2d(42));
    }
    {
      u64 x = n2d(20), y = n2d(20), z = n2d(6);         
      auto input = vector<u64> { x, y, z};
      u64 out[1000];
      auto res = lam(input.data(), out);
      assert(res == 0);
    }
  }
  return 0;
}