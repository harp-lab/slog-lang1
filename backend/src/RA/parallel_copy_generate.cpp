/*
 * copy
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */

#include "../parallel_RA_inc.h"
#include <iostream>
#include <variant>

int copy_gen_functor::operator()(const u64 *const data, u64 *const output) {
    std::vector<u64> args_for_old_bi;
    for (auto &i : func_def_ptr->old_bi_args) {
        if (i.type == "num") {
            args_for_old_bi.push_back(n2d((int)std::get<float>(i.val)));
        } else if (i.type == "data") {
            int index = (int)std::get<float>(i.val);
            args_for_old_bi.push_back(data[index]);
        } else if (i.type == "str") {
            args_for_old_bi.push_back(s2d(std::get<std::string>(i.val)));
        } else {
            std::cout << "IR error >>>>>>>>>>>>>>>>>>>>>>>!!!!" << std::endl;
        }
    }

    state_t state = std::make_tuple(data, output, func_def_ptr);
    auto callback = [](std::vector<u64> &res, state_t state) -> state_t {
        auto [data, output, f_def] = state;
        auto head_tuple = output;
        // 0 arity extra
        if (f_def->hvar_len == 0) {
            head_tuple[0] = 0;
        }
        // check comp
        bool compatible = true;
        for (auto& cp: f_def->compatibility_map) {
            if (cp.second.type == "data") {
                int index = (int)std::get<float>(cp.second.val);
                compatible = compatible && res[cp.first] == data[index];
            } else if (cp.second.type == "num") {
                compatible = compatible &&
                    res[cp.first] == n2d((int)std::get<float>(cp.second.val));
            } else if (cp.second.type == "str") {
                compatible = compatible &&
                    res[cp.first] == s2d(std::get<std::string>(cp.second.val));
            } else {
                std::cout << "IR error >>>>>>>>>>>>>>>>>>>>>>>!!!!" << std::endl;
            }
        }
        if (!compatible)
            return state;

        // populate head tuple
        for (auto& cp: f_def->reorder_mapping) {
            if (cp.second.type == "data") {
                int index = (int)std::get<float>(cp.second.val);
                head_tuple[cp.first] = data[index];
            } else if (cp.second.type == "num") {
                head_tuple[cp.first] = n2d((int)std::get<float>(cp.second.val));
            } else if (cp.second.type == "str") {
                head_tuple[cp.first] = s2d(std::get<std::string>(cp.second.val));
            } else if (cp.second.type == "res") {
                int index = (int)std::get<float>(cp.second.val);
                head_tuple[cp.first] = res[index];
            } else {
                std::cout << "IR error >>>>>>>>>>>>>>>>>>>>>>>!!!!" << std::endl;
            }
        }

        return std::make_tuple(data, output + 2, f_def);
    };
    auto [_, new_ptr, f_def] = builtin_impl(args_for_old_bi.data(), state, callback);
    auto tuples_count = (func_def_ptr->hvar_len == 0) ? new_ptr[0] : ((new_ptr - output) / 2);
    return tuples_count;
}

void parallel_copy_generate::local_copy_generate(u32 buckets, shmap_relation *input, u32 *input_bucket_map, relation *output, u32 arity, u32 join_column_count, all_to_allv_buffer &copy_filter_buffer, int ra_counter) {
    u32 *output_sub_bucket_count = output->get_sub_bucket_per_bucket_count();
    u32 **output_sub_bucket_rank = output->get_sub_bucket_rank();

    copy_filter_buffer.width[ra_counter] = (int)output->get_arity(); // reorder_map.size();
    // assert(copy_filter_buffer.width[ra_counter] == (int)output->get_arity());

    for (u32 i = 0; i < buckets; i++) {
        if (input_bucket_map[i] != 1)
            continue;

        if (input[i].size() == 0)
            return;

        auto head_rel_hash_col_count = output->get_join_column_count();

        for (auto &cur_path : input[i]) {
            int output_length = copy_filter_buffer.width[ra_counter];
            if (copy_filter_buffer.width[ra_counter] == 0) {
                output_length = 1;
            }
            u64 reordered_cur_path[output_length];
            u64 cur_path_array[cur_path.size()];
            for (u32 i = 0; i < cur_path.size(); i++)
                cur_path_array[i] = cur_path[i];

            if (use_new_api && lambda_f(cur_path_array, reordered_cur_path) != 0) {
                uint64_t bucket_id = tuple_hash(reordered_cur_path, head_rel_hash_col_count) % buckets;
                uint64_t sub_bucket_id = 0;
                if (output->get_is_canonical() == false && arity != 0 && arity >= head_rel_hash_col_count)
                    sub_bucket_id = tuple_hash(reordered_cur_path + head_rel_hash_col_count, arity - head_rel_hash_col_count) % output_sub_bucket_count[bucket_id];

                int index = output_sub_bucket_rank[bucket_id][sub_bucket_id];
                copy_filter_buffer.local_compute_output_size_rel[ra_counter] = copy_filter_buffer.local_compute_output_size_rel[ra_counter] + copy_filter_buffer.width[ra_counter];
                copy_filter_buffer.local_compute_output_size_total = copy_filter_buffer.local_compute_output_size_total + copy_filter_buffer.width[ra_counter];
                copy_filter_buffer.local_compute_output_size_flat[index * copy_filter_buffer.ra_count + ra_counter] = copy_filter_buffer.local_compute_output_size_flat[index * copy_filter_buffer.ra_count + ra_counter] + copy_filter_buffer.width[ra_counter];
                copy_filter_buffer.local_compute_output_count_flat[index * copy_filter_buffer.ra_count + ra_counter]++;

                copy_filter_buffer.local_compute_output_size[ra_counter][index] = copy_filter_buffer.local_compute_output_size[ra_counter][index] + copy_filter_buffer.width[ra_counter];
                copy_filter_buffer.cumulative_tuple_process_map[index] = copy_filter_buffer.cumulative_tuple_process_map[index] + copy_filter_buffer.width[ra_counter];
                copy_filter_buffer.local_compute_output[ra_counter][index].vector_buffer_append((const unsigned char *)reordered_cur_path, sizeof(u64) * copy_filter_buffer.width[ra_counter]);
            }
        }
    }
}
