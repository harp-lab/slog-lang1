
// LINK compile-test-file-generator.rkt
// LINK ./output/builtins-tests2-generated.cpp
#include <vector>
#include <string>
#include <unordered_map>
#include <iostream>
#include <cassert>
#include <array>
#include <chrono>


#include "../../src/builtins.cpp"

#define u64 uint64_t

using namespace std;

vector<vector<u64>> test_bi_func(const u64* const data){
    auto res = data[0] + data[1];
    return vector<vector<u64>> { vector<u64> {res}};
}

//generated factorial func:
[FACTORIAL_FUNC]

[COMPUTATIONAL_RELATION_FUNC]

[FUNC3]

[FUNC4]

[FUNC5]

[IN_RANGE]

// for comparison
u64 hand_written_factorial(u64 x) {
  if (x == 0) return 1;
  return x * hand_written_factorial(x-1);
}
int main(){

  assert(d2n(n2d(-1)) == -1);
  assert(d2n(n2d(42)) == 42);

  {
    // [extended-func (extend-cpp-builtin-to-new-args 3 '(1 2) '(1 2 3) "test_bi_func")]
    auto extended_func = [BI_EXTENDED_LAM];
    
    auto test_case = vector<u64> {1, 2, 3};
    auto expected = vector<array<u64,0>> { array<u64,0> {}};
    auto res = extended_func(test_case.data());

    assert(res == expected);
  }

  {
    // [extended-func2 (extend-cpp-builtin-to-new-args 4 '(1 2) '(4 2 1) "builtin_div_rem")]
    auto extended_func2 = [BI_EXTENDED_LAM2];

    auto test_case = vector<u64>{ number_to_datum(3), number_to_datum(10), number_to_datum(23)};
    auto expected = vector<array<u64, 1>> {{number_to_datum(2)}};
    auto res = extended_func2(test_case.data());

    assert(res == expected);
  }

  {
    // (generate-cpp-lambda-for-computational-join
    //   '((rel-version > 2 (1 2) total) x 0 _) "builtin_greater"
    //   '((rel-version + 3 (1 2) total) x 10 _ y) "builtin_add"
    //   '(x) '(y))
    
    auto join_func = [JOIN_LAM];
    
    auto test_cases = vector<pair<u64, vector<u64>>> {{n2d(4), {n2d(14)}},
                                                      {n2d(0), {}},
                                                      {n2d(-1), {}}};
    for(auto [inp, expected] : test_cases){
      u64 data[] = {inp};
      vector<u64> vec;
      join_func(data, &vec, [](u64 y, vector<u64>* vec){vec->push_back(y); return vec; });
      assert(vec == expected);
    }
  }

  {
    // (generate-cpp-lambda-for-computational-copy
    //   '((rel-version < 2 (1 2) total) 0 x _) "builtin_less"
    //   '((rel-version sign 2 (1) total) x _ 1))

    auto copy_func = [COPY_LAM];

    u64 data[] = {n2d(42)};
    vector<u64> vec;
    copy_func(data, &vec, [](u64 res, vector<u64>* state){state->push_back(res); return state;});
    vector<u64> expected = {n2d(1)};
    // cout << "vec.size(): " << vec.size() << "\n";
    // cout << "vec[0]: " << d2n(vec[0]) << "\n";
    assert(vec == expected);
  }
  
  {
    // (generate-cpp-lambda-for-computational-copy
    //   '((rel-version range 3 (1 2) total) x y _ 10) "callback_builtin_range"
    //   '((rel-version my-crel 2 (1 2) total) x y _))

    auto copy_func2 = [COPY_LAM2];

    u64 data[] = {n2d(8), n2d(11)};
    int state = 0;
    copy_func2(data, &state, [](int* state){(*state)++; return state;});
    // cout << "state: " << state << "\n";
    assert(state == 1);
  }

  {
    // res == b^2 - 4 a c
    // (crule ((rel-version delta 4 (1 2 3) total) a b c _ res)

    //   ((rel-version - 3 (1 2) total) b2 a*4*c _ res)
    //   ((rel-version * 3 (1 2) total) a*4 c _ a*4*c)
    //   ((rel-version * 3 (1 2) total) 4 a _ a*4)
    //   ((rel-version * 3 (1 2) total) b b _ b2))

    auto test_cases = vector<pair<vector<u64>, vector<u64>>> {
      {{n2d(2), n2d(5), n2d(3)}, {n2d(1)}},
      {{n2d(2), n2d(4), n2d(2)}, {n2d(0)}},
    };

    for(auto [inp, expected] : test_cases){
      vector<u64> vec;
      delta_func<vector<u64>*>(inp.data(), &vec, [](u64 res, vector<u64>* state){state->push_back(res); return state;});
      assert(vec == expected);
    }
  }

  {
    u64 data[] = {n2d(5)};
    vector<u64> vec;
    cpp_factorial<vector<u64>*>(data, &vec, [](u64 res, vector<u64>* state){state->push_back(res); return state;});
    vector<u64> expected = {n2d(120)};
    // cout << "vec.size(): " << vec.size() << "\n";
    // cout << "fac output: ";
    // for (u64 i : vec) cout << i << ", ";
    // cout << "\n";
    assert(vec == expected);

  }

  auto push_res_to_vec = [](u64 res, vector<u64>* state) -> vector<u64>* {state->push_back(res); return state;};
  auto inc_counter = [](u64* state) -> u64* {(*state) ++; return state;};
  {
    // (crule ((rel-select comp_rel3 3 (1 2) comp) 42 inp _ res)
    //       ((rel-select + 3 (1 2) comp) inp 1 _ inp+1)
    //       ((rel-select > 2 (1 2) comp) inp+1 inp _)
    //       ((rel-select < 2 (2 1) comp) inp+1 inp _)
    //       ((rel-select * 3 (1 2) comp) inp+1 2 _ res))
    {
      u64 data[] = {n2d(42), n2d(2)};
      vector<u64> vec;
      comp_rel3<vector<u64>*>(data, &vec, push_res_to_vec);
      // cout << "vec: "; for (auto x : vec) cout << x << ", "; cout << "\n";
      vector<u64> expected = {n2d(6)};
      assert(vec == expected);
    }

    {
      u64 data[] = {n2d(4200), n2d(2)};
      vector<u64> vec;
      comp_rel3<vector<u64>*>(data, &vec, push_res_to_vec);
      vector<u64> expected = {};
      assert(vec == expected);
    }
  }

  {
    // (crule ((rel-select comp_rel4 3 (1 2) comp) x y _ 100)
    //        ((rel-select < 2 (2 1) comp) y x _))}
    {
      u64 data[] = {n2d(10), n2d(11)};
      vector<u64> vec;
      comp_rel4<vector<u64>*>(data, &vec, push_res_to_vec);
      vector<u64> expected = {n2d(100)};
      assert(vec == expected);
    }
  }

  {
    // (crule ((rel-select comp_rel5 3 (1 2) comp) x y _ z)
    //        ((rel-select - 3 (2 1) comp) y x _ z)
    //        ((rel-select = 2 (1 2) comp) x 100 _))
    {
      u64 data[] = {n2d(100), n2d(30)};
      vector<u64> vec;
      comp_rel5<vector<u64>*>(data, &vec, push_res_to_vec);
      vector<u64> expected = {n2d(70)};
      assert(vec == expected);
    }
    {
      u64 data[] = {n2d(101), n2d(30)};
      vector<u64> vec;
      comp_rel5<vector<u64>*>(data, &vec, push_res_to_vec);
      cout << "!vec: "; for (auto x : vec) cout << x << ", "; cout << "\n";
      vector<u64> expected = {};
      assert(vec == expected);
    }
  }

  {
    // (crule ((rel-select in_range 3 (1 2 3) comp) x y z _)
    //        ((rel-select range 3 (1 2) comp) x y _ z)
    {
      u64 data[] = {n2d(10), n2d(30), n2d(20)};
      u64 counter = 0;
      in_range<u64*>(data, &counter, inc_counter);
      u64 expected = 1;
      assert(counter == expected);
    }
    {
      u64 data[] = {n2d(10), n2d(30), n2d(40)};
      u64 counter = 0;
      in_range<u64*>(data, &counter, inc_counter);
      u64 expected = 0;
      assert(counter == expected);
    }

  }

  // BENCHMARK:
  {
    int iterations = 2000000;

    u64 accu = 0;
    std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();
    for (int i = 0; i< iterations; i++){
      u64 data[] = {n2d(i % 10)};
      u64 res;
      cpp_factorial<u64*>(data, &res, [](u64 res, u64* state) {
        *state = res;
        return state;
      });
      accu += d2n(res);
    }
    std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
    std::cout << "generated fac   : " << std::chrono::duration_cast<std::chrono::microseconds>(end - begin).count() << " µs" << std::endl;
    cout << "accu: " << accu << "\n";
    auto generated_duration = (end - begin);

    accu = 0;
    begin = std::chrono::steady_clock::now();
    for (int i = 0; i< iterations; i++){
      accu += hand_written_factorial(i % 10);
    }
    end = std::chrono::steady_clock::now();
    std::cout << "hand-written fac: " << std::chrono::duration_cast<std::chrono::microseconds>(end - begin).count() << " µs" << std::endl;
    cout << "accu: " << accu << "\n";
    auto hand_written_duration = (end - begin);

    cout << "generated_duration / hand_written_duration: " << generated_duration / hand_written_duration << "\n";
    cout << "avg generated_duration: " << chrono::duration_cast<chrono::nanoseconds>(generated_duration / iterations).count() << " ns\n";
    cout << "avg handwritten_duration: " << chrono::duration_cast<chrono::nanoseconds>(hand_written_duration / iterations).count() << " ns\n";

  }

  return 0;
}

