/**
 * @file ast.h
 * @author your name (you@domain.com)
 * @brief  backend input ir parser/tokenizer/ast
 * @version 0.1
 * @date 2023-03-02
 *
 * @copyright Copyright (c) 2023
 *
 */

#pragma once

#include <map>
#include <memory>
#include <string>
#include <utility>
#include <variant>
#include <vector>

enum class slogc_token_type { none,
                              left_paren,
                              right_paren,
                              symbol,
                              boolean,
                              number };
enum class slogc_char_type { left_paren,
                             right_paren,
                             space,
                             other };
enum class parse_state { init,
                         symbol };
enum class slogc_relation_version { DELTA,
                                    FULL };

struct slogc_token {
    slogc_token_type type = slogc_token_type::none;
    std::variant<std::string, float> data;
};

class slogc_number;
class slogc_symbol;
class slogc_prog;
class slogc_relation_decl;
class slogc_scc_decl;
class slogc_scc_rel;
class slogc_ra_operation_fact;
class slogc_ra_operation_acopy;
class slogc_ra_operation_copy;
class slogc_ra_operation_join;
class slogc_ra_operation_copy_generate;
class slogc_ra_external_function;
class slogc_ra_operation_aggregation;

using slogc_ra_operation_ptr = std::variant<
    std::shared_ptr<slogc_ra_operation_fact>,
    std::shared_ptr<slogc_ra_operation_acopy>,
    std::shared_ptr<slogc_ra_operation_copy>,
    std::shared_ptr<slogc_ra_operation_join>,
    std::shared_ptr<slogc_ra_operation_copy_generate>,
    std::shared_ptr<slogc_ra_operation_aggregation>>;

template <class... Variants>
struct dynamic_dispatch : Variants... {
    using Variants::operator()...;
};
template <class... Variants>
dynamic_dispatch(Variants...) -> dynamic_dispatch<Variants...>;

class slogc_val {
public:
    std::string type;
    std::variant<std::string, float> val;

    slogc_val() {}
    slogc_val(std::string &type, std::variant<std::string, float> val) : type(type), val(val) {}
};

class slogc_visitor {

public:
    virtual void visit(std::shared_ptr<slogc_prog> node) = 0;

    virtual void visit(std::shared_ptr<slogc_relation_decl> node) = 0;
    virtual void visit(std::shared_ptr<slogc_scc_decl> node) = 0;
    virtual void visit(std::shared_ptr<slogc_scc_rel> node) = 0;
    virtual void visit(std::shared_ptr<slogc_ra_operation_fact> node) = 0;
    virtual void visit(std::shared_ptr<slogc_ra_operation_acopy> node) = 0;
    virtual void visit(std::shared_ptr<slogc_ra_operation_copy> node) = 0;
    virtual void visit(std::shared_ptr<slogc_ra_operation_join> node) = 0;
    virtual void visit(std::shared_ptr<slogc_ra_operation_copy_generate> node) = 0;
    // virtual void visit(std::shared_ptr<slogc_ra_external_function> node);
    virtual void visit(std::shared_ptr<slogc_ra_operation_aggregation> node) = 0;
};

class slogc_object {
public:
    virtual ~slogc_object() {}
    virtual void accept(slogc_visitor &visitor) = 0;
};

class slogc_symbol : public slogc_object {
public:
    explicit slogc_symbol(const std::string &str) : data(str) {}
    std::string data;
};

class slogc_number : public slogc_object {
public:
    explicit slogc_number(float num) : data(num) {}
    float data;
};

class slogc_relation_decl : public slogc_object, std::enable_shared_from_this<slogc_relation_decl> {
public:
    std::string name;
    std::string rel_name;
    int jcc;
    bool canonical_flag;
    int arity;

    std::weak_ptr<slogc_prog> parent;

    slogc_relation_decl(std::string &name, std::string& rel_name, int jcc, bool canonical_flag, int arity, std::weak_ptr<slogc_prog> parent) :
        name(name), rel_name(rel_name), jcc(jcc), canonical_flag(canonical_flag), arity(arity), parent(parent){};

    void accept(slogc_visitor &visitor) override { visitor.visit(shared_from_this()); }
};

class slogc_scc_rel : public slogc_object, std::enable_shared_from_this<slogc_relation_decl> {
public:
    std::string name;
    bool dynamic_flag;
    bool delete_flag;

    std::weak_ptr<slogc_scc_decl> parent;

    slogc_scc_rel() {}
    slogc_scc_rel(std::string name, bool dynamic_flag, bool delete_flag, std::weak_ptr<slogc_scc_decl> parent) :
        name(name), dynamic_flag(dynamic_flag), delete_flag(delete_flag), parent(parent){};

    void accept(slogc_visitor &visitor) override { visitor.visit(shared_from_this()); }
};

class slogc_ra_operation_fact : public slogc_object, std::enable_shared_from_this<slogc_ra_operation_fact> {
public:
    std::string rel_name;
    std::vector<slogc_val> v;

    std::weak_ptr<slogc_scc_decl> parent;

    slogc_ra_operation_fact(){};
    slogc_ra_operation_fact(std::string &rel_name, std::vector<slogc_val> &v, std::weak_ptr<slogc_scc_decl> parent) :
        rel_name(rel_name), v(v), parent(parent){};

    void accept(slogc_visitor &visitor) override { visitor.visit(shared_from_this()); }
};

class slogc_ra_operation_acopy : public slogc_object, std::enable_shared_from_this<slogc_ra_operation_acopy> {
public:
    std::string output_rel_name;
    std::string input_rel_name;
    slogc_relation_version version;
    std::vector<int> reorder_mapping;

    std::weak_ptr<slogc_scc_decl> parent;

    slogc_ra_operation_acopy() {}
    slogc_ra_operation_acopy(std::string &output_rel_name, std::string &input_rel_name,
                             slogc_relation_version version, std::vector<int> &reorder_mapping,
                             std::weak_ptr<slogc_scc_decl> parent) : output_rel_name(output_rel_name), input_rel_name(input_rel_name),
                                                                     version(version), reorder_mapping(reorder_mapping),
                                                                     parent(parent){};

    void accept(slogc_visitor &visitor) override { visitor.visit(shared_from_this()); }
};

class slogc_ra_operation_copy : public slogc_object, std::enable_shared_from_this<slogc_ra_operation_copy> {
public:
    std::string output_rel_name;
    std::string input_rel_name;
    slogc_relation_version version;
    std::vector<int> reorder_mapping;

    std::weak_ptr<slogc_scc_decl> parent;

    slogc_ra_operation_copy() {}
    slogc_ra_operation_copy(std::string &output_rel_name, std::string &input_rel_name,
                            slogc_relation_version version, std::vector<int> &reorder_mapping,
                            std::weak_ptr<slogc_scc_decl> parent) : output_rel_name(output_rel_name), input_rel_name(input_rel_name),
                                                                    version(version), reorder_mapping(reorder_mapping),
                                                                    parent(parent){};

    void accept(slogc_visitor &visitor) override { visitor.visit(shared_from_this()); }
};

class slogc_ra_operation_join : public slogc_object, std::enable_shared_from_this<slogc_ra_operation_join> {
public:
    std::string output_rel_name;
    std::string input1_rel_name;
    slogc_relation_version input1_version;
    std::string input2_rel_name;
    slogc_relation_version input2_version;
    std::vector<int> reorder_mapping;

    std::weak_ptr<slogc_scc_decl> parent;

    slogc_ra_operation_join() {}
    slogc_ra_operation_join(std::string &output_rel_name,
                            std::string &input1_rel_name, slogc_relation_version input1_version,
                            std::string &input2_rel_name, slogc_relation_version input2_version,
                            std::vector<int> &reorder_mapping, std::weak_ptr<slogc_scc_decl> parent) : output_rel_name(output_rel_name),
                                                                                                       input1_rel_name(input1_rel_name), input1_version(input1_version),
                                                                                                       input2_rel_name(input2_rel_name), input2_version(input2_version),
                                                                                                       reorder_mapping(reorder_mapping), parent(parent){};

    void accept(slogc_visitor &visitor) override { visitor.visit(shared_from_this()); }
};

class slogc_ra_external_function : public slogc_object, std::enable_shared_from_this<slogc_ra_external_function> {
public:
    std::string cpp_func_name;
    int hvar_len;
    std::vector<slogc_val> old_bi_args;
    int output_indices_len;
    std::map<int, slogc_val> compatibility_map;
    std::map<int, slogc_val> reorder_mapping;

    std::weak_ptr<slogc_ra_operation_copy_generate> parent;

    slogc_ra_external_function() {}
    slogc_ra_external_function(std::string cpp_func_name, int hvar_len,
                               std::vector<slogc_val> &old_bi_args,
                               int output_indices_len,
                               std::map<int, slogc_val> &compatibility_map,
                               std::map<int, slogc_val> &reorder_mapping,
                               std::weak_ptr<slogc_ra_operation_copy_generate> parent) : cpp_func_name(cpp_func_name), hvar_len(hvar_len),
                                                                                         old_bi_args(old_bi_args), output_indices_len(output_indices_len),
                                                                                         compatibility_map(compatibility_map),
                                                                                         reorder_mapping(reorder_mapping),
                                                                                         parent(parent){};

    void accept(slogc_visitor &visitor) override { return; }
};

class slogc_ra_operation_copy_generate : public slogc_object, std::enable_shared_from_this<slogc_ra_operation_copy_generate> {
public:
    std::string output_rel_name;
    std::string input_rel_name;
    slogc_relation_version version;
    std::shared_ptr<slogc_ra_external_function> extern_func;

    std::weak_ptr<slogc_scc_decl> parent;

    slogc_ra_operation_copy_generate() {}
    slogc_ra_operation_copy_generate(
        std::string output_rel_name, std::string input_rel_name,
        slogc_relation_version version,
        std::shared_ptr<slogc_ra_external_function> extern_func,
        std::weak_ptr<slogc_scc_decl> parent) : output_rel_name(output_rel_name), input_rel_name(input_rel_name),
                                                version(version), extern_func(extern_func),
                                                parent(parent){};

    void accept(slogc_visitor &visitor) override { visitor.visit(shared_from_this()); }
};

class slogc_ra_operation_aggregation : public slogc_object, std::enable_shared_from_this<slogc_ra_operation_aggregation> {
public:
    std::string output_rel_name;
    std::string input1_rel_name;
    std::string input2_rel_name;
    slogc_relation_version rel_version;
    std::string local_func_name;
    std::string agg_type;
    std::string reduce_func_name;
    std::string global_func_name;
    std::vector<int> reorder_mapping;

    std::weak_ptr<slogc_scc_decl> parent;

    slogc_ra_operation_aggregation() {}
    slogc_ra_operation_aggregation(
        std::string output_rel_name,
        std::string input1_rel_name,
        std::string input2_rel_name, slogc_relation_version rel_version,
        std::string local_func_name, std::string agg_type,
        std::string reduce_func_name, std::string global_func_name,
        std::vector<int> &reorder_mapping, std::weak_ptr<slogc_scc_decl> parent) : output_rel_name(output_rel_name), input1_rel_name(input1_rel_name),
                                                                                   input2_rel_name(input2_rel_name),
                                                                                   rel_version(rel_version), local_func_name(local_func_name),
                                                                                   agg_type(agg_type), reduce_func_name(reduce_func_name),
                                                                                   global_func_name(global_func_name), reorder_mapping(reorder_mapping),
                                                                                   parent(parent) {}

    void accept(slogc_visitor &visitor) override { visitor.visit(shared_from_this()); }
};

class slogc_scc_decl : public slogc_object, std::enable_shared_from_this<slogc_scc_decl> {
public:
    std::string name;
    int id;
    bool fixpoint_flag;
    std::vector<std::shared_ptr<slogc_scc_rel>> scc_rels;
    std::vector<slogc_ra_operation_ptr> ra_operations;

    std::weak_ptr<slogc_prog> parent;

    slogc_scc_decl() {}
    slogc_scc_decl(std::string &name, int id, bool fixpoint_flag,
                   std::vector<std::shared_ptr<slogc_scc_rel>> &scc_rels,
                   std::vector<slogc_ra_operation_ptr> &ra_operations,
                   std::weak_ptr<slogc_prog> parent) : name(name), id(id), fixpoint_flag(fixpoint_flag), scc_rels(scc_rels),
                                                       ra_operations(ra_operations), parent(parent){};

    void accept(slogc_visitor &visitor) override { visitor.visit(shared_from_this()); }
};

using slogc_order_pair = std::pair<int, int>;

// top rule
class slogc_prog : public slogc_object, std::enable_shared_from_this<slogc_scc_decl>  {
public:
    std::vector<std::shared_ptr<slogc_relation_decl>> relation_decls;
    std::vector<std::shared_ptr<slogc_scc_decl>> scc_decls;
    std::vector<slogc_order_pair> scc_orders;

    slogc_prog() {}
    slogc_prog(std::vector<std::shared_ptr<slogc_relation_decl>> &relation_decls,
               std::vector<std::shared_ptr<slogc_scc_decl>> &scc_decls,
               std::vector<slogc_order_pair> &scc_orders) : relation_decls(relation_decls), scc_decls(scc_decls),
                                                            scc_orders(scc_orders){};
    void accept(slogc_visitor &visitor) override { visitor.visit(shared_from_this()); }
};

class slogc_tokenizer {
public:
    slogc_tokenizer(std::istream &in) : in_(in) {}
    bool next();
    const slogc_token &current() const {
        return current_;
    }
    void putback() {
        putback_ = true;
    }

    std::istream &in_;
    bool putback_ = false;
    slogc_token current_;
};
