
// LINK compile-test-file-generator.rkt
// LINK ./output/builtins-tests-generated.cpp
#include <vector>
#include <string>
#include <unordered_map>
#include <functional>
#include <cassert>
#include <iostream>
#include <array>

#include "./test_header.h"
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

  {
    //[rule5 '(srule
    //         ((rel-select bar 4 (1 2 3 4) db) z w x y)
    //         ((rel-version foo 4 (1 3) total) x y _2 w z)
    //         ((rel-version > 2 (2 1) comp) x y _1))]
    //[lam5 (generate-cpp-lambda-for-rule-with-builtin-impl rule2 '(1 2) "builtin_greater")]

    auto lam = ~a;
    {
      u64 x = n2d(101), y = n2d(102), w= n2d(1000), z = n2d(1001);
      auto input = vector<u64> {x, y, 0, w, z};
      u64 out[1000];
      auto res = lam(input.data(), out);
      assert(res == 1);
      // cout << " out[0]: " << out[0] << ", out[1]: " << out[1] << ", out[2]: " << out[2] << " out[3]: " << out[3] << "\n";
      assert(out[0] == z && out[1] == w && out[2] == x && out[3] == y);
    }
    {
      u64 x = n2d(101), y = n2d(100), w= n2d(1000), z = n2d(1001);
      auto input = vector<u64> {x, y, 0, w, z};
      u64 out[1000];
      auto res = lam(input.data(), out);
      assert(res == 0);
    }
  }

  {
    // [rule6 '(srule
    //         ((rel-select bar 4 (1 2 3 4) db) z w x y)
    //         ((rel-version foo 4 (1 3) total) x y _2 w z)
    //         ((rel-version > 2 (2 1) comp) 101 102 _1))]
    // [lam6 (generate-cpp-lambda-for-rule-with-builtin-impl rule6 '(1 2) "builtin_greater")]
    auto lam = ~a;
    {
      u64 x = n2d(101), y = n2d(102), w= n2d(1000), z = n2d(1001);
      auto input = vector<u64> {x, y, 0, w, z};
      u64 out[1000];
      auto res = lam(input.data(), out);
      assert(res == 1);
      // cout << " out[0]: " << out[0] << ", out[1]: " << out[1] << ", out[2]: " << out[2] << " out[3]: " << out[3] << "\n";
      assert(out[0] == z && out[1] == w && out[2] == x && out[3] == y);
    }
  }

  {
    // [rule7 '(srule
    //         ((rel-select bar 4 (1 2 3 4) db) x ans y w ans z)
    //         ((rel-version foo 4 (1 3) total) x y _2 w z)
    //         ((rel-version = 2 (1) comp) 42 ans _1))]
    // [lam7 (generate-cpp-lambda-for-rule-with-builtin-impl rule7 '(1 2) "builtin_eq_1")]
    auto lam = ~a;
    {
      u64 x = n2d(101), y = n2d(102), w= n2d(1000), z = n2d(1001);
      u64 ans = n2d(42);
      auto input = vector<u64> {x, y, 111111, w, z};
      u64 out[1000];
      auto res = lam(input.data(), out);
      assert(res == 1);
      // for (int i =0; i<6; i++) cout << "out["<<i<<"]: " << out[i] << ", "; cout << "\n";
      assert(out[0] == x && out[1] == ans && out[2] == y && out[3] == w && out[4] == ans && out[5] == z);
    }
  }

  {
    // [rule8 '(srule
    //         ((rel-select bar 4 (1 2 3 4) db) 1001 x 1002 y 1003 z)
    //         ((rel-version foo 3 (1) total) x _2 y z)
    //         ((rel-version < 2 (2 1) comp) x 100 _1))]
    // [lam8 (generate-cpp-lambda-for-rule-with-builtin-impl rule8 '(1 2) "builtin_less")]
    auto lam = ~a;
    {
      u64 x = n2d(200), y = n2d(211), z= n2d(212);
      auto input = vector<u64> {x, 400000, y, z};
      u64 out[1000];
      auto res = lam(input.data(), out);
      assert(res == 1);
      // for (int i =0; i<6; i++) cout << "out["<<i<<"]: " << out[i] << ", "; cout << "\n";
      assert(out[0] == n2d(1001) && out[1] == x && out[2] == n2d(1002) && out[3] == y && out[4] == n2d(1003) && out[5] == z);
    }
  }

  {
    // [rule9 '(srule
    //      ((rel-select $inter-body4 3 (1 2 3) db) $id1 $id2 e)
    //      ((rel-version $inter-body3 3 (1) total) $id1 $_14 $id2 e)
    //      ((rel-version = 2 (1 2) comp) $id1 $id1 $_8))]
    // [lam9 (generate-cpp-lambda-for-rule-with-builtin-impl rule9 '(1 2) "builtin_eq")]
    auto lam = ~a;
    {
      u64 id1 = n2d(42), id2 = n2d(123), e = n2d(1000);
      auto input = vector<u64> {id1, 9999, id2, e};
      u64 out[1000];
      auto res = lam(input.data(), out);
      // cout << "res: " << res << "\n";
      assert( res == 1);
      // for (int i =0; i<3; i++) cout << "out["<<i<<"]: " << out[i] << ", "; cout << "\n";
      assert(out[0] = id1 && out[1] == id2 && out[2] == e);
    }
  }

  {
    // (srule
    //      ((rel-select $inter-body4 3 (1 2 3) db) a b c)
    //      ((rel-version foo 3 (1 2 3) total) a b c)
    //      ((rel-version range 3 (3 2 1) comp) c b a $_))
    auto lam = ~a;
    {
      u64 a = n2d(1), b = n2d(11), c = n2d(5);
      auto input = vector<u64> {a, b, c, 10000};
      u64 out[1000];
      auto res = lam(input.data(), out);
      // cout << "res : " << res << "\n";
      assert( res == 1);
    }
  }
  return 0;
}