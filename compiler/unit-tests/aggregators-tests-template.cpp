#include <vector>
#include <string>
#include <unordered_map>
#include <functional>
#include <cassert>
#include <iostream>
#include <array>

#include "../../src/builtins.cpp"

struct TestBTree : public _BTree {
  vector<u64> row;
  TestBTree(vector<u64> row) {
    this->row = row;
  }
  bool has_key(const u64 *key) override{
    bool res = true;
    for (int i = 0; i < row.size(); i++)
      if (row[i] != key[i]) res = false;
    return res;
  }
};

int main(){
  {
    // (srule
    //     ((rel-select bar 2 (1 2) db) x y)
    //     ((rel-version foo 2 (1 2) total) x y $_id)
    //     ((rel-version ~ 2 (1 2) (agg (rel-version aggregated 2 (1 2) total))) x 100 $_1))
    auto local_lam = ~a;
    auto global_lam = ~a;
    {
        u64 x = n2d(1), y = n2d(4);
        auto input = vector<u64> {n2d(x), n2d(y)};
        _BTree* aggregated_btree = new TestBTree(vector<u64> {n2d(1), n2d(100)});
        auto local_res = local_lam(aggregated_btree, input.data());
        assert(local_res == (u64) true);
        u64 out[1000];
        assert(global_lam(input.data(), local_res, 1, out) == 0);
    }
  }
  
  {
    // (srule
    //       ((rel-select bar 2 (1 2) db) 1000 y x 10000 y)
    //       ((rel-version foo 2 (1 2) total) x y $_id)
    //       ((rel-version ~ 2 (1 2) (agg (rel-version aggregated 2 (1 2) total))) x 100 $_1))
    auto local_lam = ~a;
    auto global_lam = ~a;
    {
      u64 x = n2d(1), y = n2d(4);
      auto input = vector<u64> {n2d(x), n2d(y)};
      {
        _BTree* aggregated_btree = new TestBTree(vector<u64> {n2d(1), n2d(100)});
        auto local_res = local_lam(aggregated_btree, input.data());
        assert(local_res == (u64) true);
        u64 out[1000];
        assert(global_lam(input.data(), local_res, 1, out) == 0);
      }
      {
        _BTree* aggregated_btree = new TestBTree(vector<u64> {n2d(1), n2d(99)});
        auto local_res = local_lam(aggregated_btree, input.data());
        assert(local_res == (u64) false);
        u64 out[1000];
        assert(global_lam(input.data(), local_res, 1, out) == 1);
        assert(out[0] == n2d(1000) && out[1] == y && out[2] == x && out[3] == n2d(10000) && out[4] == y );
      }
    }
  }
  return 0;
}