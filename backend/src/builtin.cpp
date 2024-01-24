
#include "parallel_RA_inc.h"
#include <iostream>

std::map<std::string, builtin_impl_t> BUILTIN_IMP_MAP;

std::vector<std::array<u64, 2>> builtin_div_rem(const u64 *const data) {
    if (is_number(data[0]) && is_number(data[1])) {
        auto div = number_to_datum(d2n(data[0]) / d2n(data[1]));
        auto rem = number_to_datum(d2n(data[0]) % d2n(data[1]));
        return {{div, rem}};
    } else {
        return {};
    }
}

#define BUILTIN_BINARY_NUMBER_PRED(name, op)                                              \
    state_t name(const u64 *data, state_t init_state, builtin_impl_callback_t callback) { \
        if (is_number(data[0]) && is_number(data[1]) &&                                   \
            datum_to_number(data[0]) op datum_to_number(data[1])) {                       \
            std::vector<u64> _dummy;                                                      \
            return callback(_dummy, init_state);                                          \
        } else                                                                            \
            return init_state;                                                            \
    }

BUILTIN_BINARY_NUMBER_PRED(builtin_less, <)
BUILTIN_BINARY_NUMBER_PRED(builtin_greater, >)
BUILTIN_BINARY_NUMBER_PRED(builtin_le, <=)
BUILTIN_BINARY_NUMBER_PRED(builtin_ge, >=)

#define BUILTIN_BINARY_NUMBER_FUNC(name, op)                                                      \
    state_t name(const u64 *data, state_t init_state, builtin_impl_callback_t callback) {         \
        if (is_number(data[0]) && is_number(data[1])) {                                           \
            std::vector<u64> res;                                                                 \
            res.push_back(number_to_datum(datum_to_number(data[0]) op datum_to_number(data[1]))); \
            return callback(res, init_state);                                                     \
        } else                                                                                    \
            return init_state;                                                                    \
    }

BUILTIN_BINARY_NUMBER_FUNC(builtin_add, +)
BUILTIN_BINARY_NUMBER_FUNC(builtin_subtract, -)
BUILTIN_BINARY_NUMBER_FUNC(builtin_multiply, *)
BUILTIN_BINARY_NUMBER_FUNC(builtin_divide, /)

#define BUILTIN_BINARY_NUMBER_FUNC2(name, impl)                                                       \
    state_t name(const u64 *data, state_t init_state, builtin_impl_callback_t callback) {             \
        if (is_number(data[0]) && is_number(data[1])) {                                               \
            std::vector<u64> res;                                                                     \
            res.push_back(number_to_datum(impl(datum_to_number(data[0]), datum_to_number(data[1])))); \
            return callback(res, init_state);                                                         \
        } else                                                                                        \
            return init_state;                                                                        \
    }

u64 impl_arg2_minus_arg1(u64 arg1, u64 arg2) { return arg2 - arg1; }
BUILTIN_BINARY_NUMBER_FUNC2(builtin_arg2_minus_arg1, impl_arg2_minus_arg1)

#define BUILTIN_UNARY_NUMBER_FUNC(name, impl)                                             \
    state_t name(const u64 *data, state_t init_state, builtin_impl_callback_t callback) { \
        if (is_number(data[0])) {                                                         \
            std::vector<u64> res;                                                         \
            res.push_back(number_to_datum(impl(datum_to_number(data[0]))));               \
            return callback(res, init_state);                                             \
        } else                                                                            \
            return init_state;                                                            \
    }

u64 add1(u64 x) { return x + 1; }
u64 sub1(u64 x) { return x - 1; }

BUILTIN_UNARY_NUMBER_FUNC(builtin_add1, add1)
BUILTIN_UNARY_NUMBER_FUNC(builtin_add1_2, sub1)
BUILTIN_UNARY_NUMBER_FUNC(builtin_sub1, sub1)
BUILTIN_UNARY_NUMBER_FUNC(builtin_sub1_2, add1)

std::vector<std::array<u64, 1>> builtin_range(const u64 *const data) {
    std::vector<std::array<u64, 1>> res;
    if (is_number(data[0]) && is_number(data[1])) {
        auto lb = datum_to_number(data[0]);
        auto ub = datum_to_number(data[1]);
        res.reserve(ub - lb);
        for (u64 x = lb; x < ub; x++)
            res.push_back({number_to_datum(x)});
    }
    return res;
}

state_t callback_builtin_range(const u64 *data, state_t init_state, builtin_impl_callback_t callback) {
    auto state = init_state;
    if (is_number(data[0]) && is_number(data[1])) {
        auto lb = datum_to_number(data[0]);
        auto ub = datum_to_number(data[1]);
        std::vector<u64> res(1);
        for (u64 x = lb; x < ub; x++) {
            res[0] = number_to_datum(x);
            state = callback(res, state);
        }
    }
    return state;
}

#define BUILTIN_BINARY_PRED(name, op)                                                     \
    state_t name(const u64 *data, state_t init_state, builtin_impl_callback_t callback) { \
        std::vector<u64> _dummy(1);                                                       \
        if (data[0] op data[1])                                                           \
            return callback(_dummy, init_state);                                          \
        else                                                                              \
            return init_state;                                                            \
    }
BUILTIN_BINARY_PRED(builtin_eq, ==)
BUILTIN_BINARY_PRED(builtin_neq, !=)

state_t builtin_eq_1(const u64 *data, state_t init_state, builtin_impl_callback_t callback) {
    std::vector<u64> res(1);
    res[0] = data[0];
    return callback(res, init_state);
}

#define BUILTIN_UNARY_PRED(name, pred)                                                    \
    state_t name(const u64 *data, state_t init_state, builtin_impl_callback_t callback) { \
        std::vector<u64> _dummy(1);                                                       \
        if (pred(data[0]))                                                                \
            return callback(_dummy, init_state);                                          \
        else                                                                              \
            return init_state;                                                            \
    }

bool is_not_number(u64 datum) { return !is_number(datum); }
BUILTIN_UNARY_PRED(builtin_number_huh, is_number)
BUILTIN_UNARY_PRED(builtin_not_number_huh, is_not_number)

// state_t builtin_nop(const u64 *data, state_t init_state, state_t (*callback)(state_t state)) {
//     return callback(init_state);
// }

//
// agg

local_agg_res_t agg_count_local(std::pair<shmap_relation::iterator, shmap_relation::iterator> joined_range) {
    local_agg_res_t cnt = 0;
    for (auto it = joined_range.first; it != joined_range.second; ++it) {
        cnt++;
    }
    return cnt;
}

local_agg_res_t agg_count_reduce(local_agg_res_t x, local_agg_res_t y) {
    return x + y;
}

local_agg_res_t agg_sum_local(std::pair<shmap_relation::iterator, shmap_relation::iterator> joined_range) {
    local_agg_res_t sum_res = 0;
    for (auto it = joined_range.first; it != joined_range.second; ++it) {
        auto tuple = (*it);
        sum_res += tuple[tuple.size() - 1];
    }
    return sum_res;
}

local_agg_res_t agg_sum_reduce(local_agg_res_t x, local_agg_res_t y) {
    return x + y;
}

local_agg_res_t agg_maximum_local(std::pair<shmap_relation::iterator, shmap_relation::iterator> joined_range) {
    local_agg_res_t max_res = 0;
    for (auto it = joined_range.first; it != joined_range.second; ++it) {
        auto tuple = (*it);
        auto current_v = tuple[tuple.size() - 1];
        if (current_v > max_res) {
            max_res = current_v;
        }
    }
    return max_res;
}

local_agg_res_t agg_maximum_reduce(local_agg_res_t x, local_agg_res_t y) {
    if (x > y) {
        return x;
    } else {
        return y;
    }
}

local_agg_res_t agg_minimum_local(std::pair<shmap_relation::iterator, shmap_relation::iterator> joined_range) {
    local_agg_res_t min_res = std::numeric_limits<u32>::max();
    for (auto it = joined_range.first; it != joined_range.second; ++it) {
        auto tuple = (*it);
        auto current_v = tuple[tuple.size() - 1];
        if (current_v < min_res) {
            min_res = current_v;
        }
    }
    return min_res;
}

local_agg_res_t agg_minimum_reduce(local_agg_res_t x, local_agg_res_t y) {
    if (x < y) {
        return x;
    } else {
        return y;
    }
}

// map

builtin_impl_t builtin_map(std::string &func_name) {
    if (func_name == "builtin_less")
        return builtin_less;
    else if (func_name == "builtin_greater")
        return builtin_greater;
    else if (func_name == "builtin_le")
        return builtin_le;
    else if (func_name == "builtin_le")
        return builtin_le;
    else if (func_name == "builtin_ge")
        return builtin_ge;
    else if (func_name == "builtin_add")
        return builtin_add;
    else if (func_name == "builtin_subtract")
        return builtin_subtract;
    else if (func_name == "builtin_multiply")
        return builtin_multiply;
    else if (func_name == "builtin_divide")
        return builtin_divide;
    else if (func_name == "builtin_arg2_minus_arg1")
        return builtin_arg2_minus_arg1;
    else if (func_name == "builtin_add1")
        return builtin_add1;
    else if (func_name == "builtin_add1_2")
        return builtin_add1_2;
    else if (func_name == "builtin_sub1")
        return builtin_sub1;
    else if (func_name == "builtin_sub1_2")
        return builtin_sub1_2;
    else if (func_name == "builtin_eq")
        return builtin_eq;
    else if (func_name == "builtin_neq")
        return builtin_neq;
    else if (func_name == "builtin_number_huh")
        return builtin_number_huh;
    else if (func_name == "builtin_not_number_huh")
        return builtin_not_number_huh;
    else if (func_name == "builtin_eq_1")
        return builtin_eq_1;
    else
        std::cout << func_name << " builtin not implemented" << std::endl;
}

local_agg_func_t agg_local_map(std::string &agg_name) {
    if (agg_name == "agg_count_local")
        return agg_count_local;
    else if (agg_name == "agg_sum_local")
        return agg_sum_local;
    else if (agg_name == "agg_maximum_local")
        return agg_maximum_local;
    else if (agg_name == "agg_minimum_local")
        return agg_minimum_local;
    else
        std::cout << agg_name << " agg not implemented" << std::endl;
}

reduce_agg_func_t agg_reduce_map(std::string &agg_name) {
    if (agg_name == "agg_count_reduce")
        return agg_count_reduce;
    else if (agg_name == "agg_sum_reduce")
        return agg_sum_reduce;
    else if (agg_name == "agg_maximum_reduce")
        return agg_maximum_reduce;
    else if (agg_name == "agg_minimum_reduce")
        return agg_minimum_reduce;
    else
        std::cout << agg_name << " agg not implemented" << std::endl;
}

// global_agg_func_t agg_global_map(std::string& agg_name) {
//     if (agg_name == )
// }
