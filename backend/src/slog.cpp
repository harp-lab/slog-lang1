/**
 * @file slog.cpp
 * @author Yihao Sun
 * @brief Slog main entrance file
 * @version 0.1
 * @date 2023-02-26
 *
 * @copyright Copyright (c) 2023
 *
 */

#include "ast.h"
#include "parallel_RA_inc.h"
#include <iostream>

void print_token(const slogc_token &token) {
    if (token.type == slogc_token_type::symbol) {
        std::cout << "symbol" << std::endl;
    } else if (token.type == slogc_token_type::none) {
        std::cout << "none" << std::endl;
    } else if (token.type == slogc_token_type::number) {
        std::cout << "number" << std::endl;
    } else if (token.type == slogc_token_type::left_paren) {
        std::cout << "left_paren" << std::endl;
    } else if (token.type == slogc_token_type::right_paren) {
        std::cout << "right_paren" << std::endl;
    } else if (token.type == slogc_token_type::boolean) {
        std::cout << "boolean" << std::endl;
    }
    std::visit(dynamic_dispatch{
                   [](auto v) { std::cout << v << std::endl; }},
               token.data);
}

slogc_char_type get_char_type(char ch) {
    switch (ch) {
    case '(':
        return slogc_char_type::left_paren;
    case ')':
        return slogc_char_type::right_paren;
    }
    if (isspace(static_cast<unsigned char>(ch)))
        return slogc_char_type::space;
    return slogc_char_type::other;
}

bool parse_number(const std::string &str, slogc_token &tok) {
    try {
        size_t pos = 0;
        float num = std::stof(str, &pos);
        if (pos == str.size()) {
            tok.type = slogc_token_type::number;
            tok.data = num;
            return true;
        }
    } catch (const std::exception &) {
    }
    return false;
}

bool get_token(std::istream &in, slogc_token &tok) {
    char ch;
    parse_state state = parse_state::init;
    std::string str;
    slogc_token_type type = slogc_token_type::none;
    while (in.get(ch)) {
        slogc_char_type ctype = get_char_type(ch);

        if (state == parse_state::symbol) {
            if (ctype == slogc_char_type::space)
                break;
            if (ctype != slogc_char_type::other) {
                in.putback(ch);
                break;
            }
            str += ch;
        } else if (ctype == slogc_char_type::other) {
            state = parse_state::symbol;
            type = slogc_token_type::symbol;
            str = ch;
        } else if (ctype == slogc_char_type::left_paren) {
            type = slogc_token_type::left_paren;
            break;
        } else if (ctype == slogc_char_type::right_paren) {
            type = slogc_token_type::right_paren;
            break;
        }
    }

    tok.type = type;
    if (type == slogc_token_type::symbol) {
        if (!parse_number(str, tok))
            tok.data = str;
    }
    return true;
}

bool slogc_tokenizer::next() {
    if (putback_) {
        putback_ = false;
        return true;
    }
    return get_token(in_, current_);
}

void parse_lparen(slogc_tokenizer &tokenizer) {
    if (!tokenizer.next())
        throw std::runtime_error("syntax error: missing `(` end");
    const slogc_token &tok_lpar = tokenizer.current();
    if (tok_lpar.type != slogc_token_type::left_paren) {
        print_token(tokenizer.current());
        throw std::runtime_error("syntax error: missing `(`");
    }
}

void parse_rparen(slogc_tokenizer &tokenizer) {
    if (!tokenizer.next())
        throw std::runtime_error("syntax error: missing `)`");
    const slogc_token &tok_rpar = tokenizer.current();
    if (tok_rpar.type != slogc_token_type::right_paren) {
        print_token(tokenizer.current());
        throw std::runtime_error("syntax error: missing `)`");
    }
}

std::string parse_sym(slogc_tokenizer &tokenizer) {
    tokenizer.next();
    const slogc_token &tok_sym = tokenizer.current();
    std::string sym;
    if (tok_sym.type == slogc_token_type::symbol) {
        sym = std::get<std::string>(tok_sym.data);
    } else {
        throw std::runtime_error("syntax error: invalid symbol");
    }
    return sym;
}

int parse_int(slogc_tokenizer &tokenizer) {
    tokenizer.next();
    const slogc_token &tok_num = tokenizer.current();
    int v;
    if (tok_num.type == slogc_token_type::number) {
        v = (int)std::get<float>(tok_num.data);
    } else {
        throw std::runtime_error("syntax error: invalid number");
    }
    return v;
}

int parse_bool(slogc_tokenizer &tokenizer) {
    tokenizer.next();
    const slogc_token &tok_bool = tokenizer.current();
    bool flag;
    if (tok_bool.type == slogc_token_type::symbol) {
        std::string c_flag = std::get<std::string>(tok_bool.data);
        if (c_flag == "#t") {
            flag = true;
        } else if (c_flag == "#f") {
            flag = false;
        } else {
            throw std::runtime_error("syntax error: invalid bool");
        }
    } else {
        throw std::runtime_error("syntax error: invalid bool");
    }
    return flag;
}

std::vector<int>
parse_reorder_list(slogc_tokenizer &tokenizer) {
    parse_lparen(tokenizer);
    std::vector<int> reorders;
    bool unfinished_flag = true;
    while (tokenizer.next()) {
        const slogc_token &tok_ll = tokenizer.current();
        if (tok_ll.type == slogc_token_type::right_paren) {
            unfinished_flag = false;
            break;
        } else {
            tokenizer.putback();
        }
        reorders.push_back(parse_int(tokenizer));
    }
    if (unfinished_flag) {
        throw std::runtime_error("syntax error: missing rpar.");
    }
    return reorders;
}

slogc_val parse_slogc_val(slogc_tokenizer &tokenizer) {
    parse_lparen(tokenizer);

    std::string val_type(parse_sym(tokenizer));
    tokenizer.next();
    auto v = tokenizer.current().data;
    parse_rparen(tokenizer);
    return slogc_val(val_type, v);
}

std::shared_ptr<slogc_relation_decl>
parse_relation_decl(slogc_tokenizer &tokenizer, std::weak_ptr<slogc_prog> parent) {
    parse_lparen(tokenizer);
    std::string rel_sym = parse_sym(tokenizer);
    if (rel_sym != "relation-decl") {
        throw std::runtime_error("syntax error: missing `relation-decl` tag");
    }

    std::string name = parse_sym(tokenizer);
    std::string rel_name = parse_sym(tokenizer);
    int jcc = parse_int(tokenizer);
    bool canonical_flag = parse_bool(tokenizer);
    int arity = parse_int(tokenizer);

    parse_rparen(tokenizer);
    return std::make_shared<slogc_relation_decl>(name, rel_name, jcc, canonical_flag, arity, parent);
}

std::vector<std::shared_ptr<slogc_relation_decl>>
parse_relation_decl_list(slogc_tokenizer &tokenizer, std::weak_ptr<slogc_prog> parent) {
    parse_lparen(tokenizer);
    std::vector<std::shared_ptr<slogc_relation_decl>> rel_list;
    bool unfinished_flag = true;
    while (tokenizer.next()) {
        const slogc_token &tok_ll = tokenizer.current();
        if (tok_ll.type == slogc_token_type::right_paren) {
            unfinished_flag = false;
            break;
        } else {
            tokenizer.putback();
        }
        std::shared_ptr<slogc_relation_decl> rel = parse_relation_decl(tokenizer, parent);
        rel_list.push_back(rel);
    }
    if (unfinished_flag)
        throw std::runtime_error("syntax error: missing right par in relation declaration");

    return rel_list;
}

std::shared_ptr<slogc_scc_rel>
parse_scc_rel(slogc_tokenizer &tokenizer, std::weak_ptr<slogc_scc_decl> parent) {
    parse_lparen(tokenizer);

    std::string scc_rel_sym = parse_sym(tokenizer);
    if (scc_rel_sym != "scc-rel") {
        throw std::runtime_error("syntax error: missing `scc-rel` tag");
    }
    std::string name = parse_sym(tokenizer);
    bool dynamic_flag = parse_bool(tokenizer);
    bool delete_flag = parse_bool(tokenizer);

    parse_rparen(tokenizer);
    return std::make_shared<slogc_scc_rel>(name, dynamic_flag, delete_flag, parent);
}

std::vector<std::shared_ptr<slogc_scc_rel>>
parse_scc_rel_list(slogc_tokenizer &tokenizer, std::weak_ptr<slogc_scc_decl> parent) {
    parse_lparen(tokenizer);

    std::vector<std::shared_ptr<slogc_scc_rel>> rel_list;
    bool unfinished_flag = true;
    while (tokenizer.next()) {
        const slogc_token &tok_ll = tokenizer.current();
        if (tok_ll.type == slogc_token_type::right_paren) {
            unfinished_flag = false;
            break;
        } else {
            tokenizer.putback();
        }
        std::shared_ptr<slogc_scc_rel> rel = parse_scc_rel(tokenizer, parent);
        rel_list.push_back(rel);
    }
    if (unfinished_flag)
        throw std::runtime_error("syntax error: missing right par in scc relation declaration list");

    return rel_list;
}

std::vector<slogc_val>
parse_slogc_val_list(slogc_tokenizer &tokenizer) {
    parse_lparen(tokenizer);
    std::vector<slogc_val> vs;
    bool unfinished_flag = true;
    while (tokenizer.next()) {
        const slogc_token &tok_ll = tokenizer.current();
        if (tok_ll.type == slogc_token_type::right_paren) {
            unfinished_flag = false;
            break;
        } else {
            tokenizer.putback();
        }
        vs.push_back(parse_slogc_val(tokenizer));
    }
    if (unfinished_flag)
        throw std::runtime_error("syntax error: missing right par.");
    return vs;
}

std::shared_ptr<slogc_ra_external_function>
parse_ra_external_function(slogc_tokenizer &tokenizer, std::weak_ptr<slogc_ra_operation_copy_generate> parent) {
    parse_lparen(tokenizer);
    std::string ext_sym(parse_sym(tokenizer));
    if (ext_sym != "external-function")
        throw std::runtime_error("syntax error: missing `external-function` tag");
    std::string cpp_func_name(parse_sym(tokenizer));

    int hvar_len = parse_int(tokenizer);

    std::vector<slogc_val> old_bi_args(parse_slogc_val_list(tokenizer));
    int output_indices_len = parse_int(tokenizer);

    std::map<int, slogc_val> r_map;
    parse_lparen(tokenizer);
    bool unfinished_flag = true;
    while (tokenizer.next()) {
        const slogc_token &tok_ll = tokenizer.current();
        if (tok_ll.type == slogc_token_type::right_paren) {
            unfinished_flag = false;
            break;
        } else {
            tokenizer.putback();
        }
        parse_lparen(tokenizer);
        int k = parse_int(tokenizer);
        slogc_val v = parse_slogc_val(tokenizer);
        r_map[k] = v;
        parse_rparen(tokenizer);
    }

    if (unfinished_flag)
        throw std::runtime_error("syntax error: missing right par in result mapping list.");

    std::map<int, slogc_val> v_map;
    parse_lparen(tokenizer);
    unfinished_flag = true;
    while (tokenizer.next()) {
        const slogc_token &tok_ll = tokenizer.current();
        if (tok_ll.type == slogc_token_type::right_paren) {
            unfinished_flag = false;
            break;
        } else {
            tokenizer.putback();
        }
        parse_lparen(tokenizer);
        int k = parse_int(tokenizer);
        slogc_val v = parse_slogc_val(tokenizer);
        v_map[k] = v;
        parse_rparen(tokenizer);
    }

    if (unfinished_flag)
        throw std::runtime_error("syntax error: missing right par in result mapping list.");

    parse_rparen(tokenizer);
    return std::make_shared<slogc_ra_external_function>(
        cpp_func_name, hvar_len, old_bi_args, output_indices_len,
        r_map, v_map, parent);
}

slogc_relation_version parse_rel_version(slogc_tokenizer &tokenizer) {
    auto vs = parse_sym(tokenizer);
    if (vs == "DELTA") {
        return slogc_relation_version::DELTA;
    } else {
        return slogc_relation_version::FULL;
    }
}

slogc_ra_operation_ptr
parse_ra_operation(slogc_tokenizer &tokenizer, std::weak_ptr<slogc_scc_decl> parent) {
    parse_lparen(tokenizer);
    slogc_ra_operation_ptr ptr;
    std::string ra_type(parse_sym(tokenizer));
    if (ra_type == "fact") {
        std::string rel_name(parse_sym(tokenizer));
        std::vector<slogc_val> vs(parse_slogc_val_list(tokenizer));
        ptr = std::make_shared<slogc_ra_operation_fact>(rel_name, vs, parent);
    } else if (ra_type == "acopy" || ra_type == "copy") {
        std::string output_rel_name(parse_sym(tokenizer));
        std::string input_rel_name(parse_sym(tokenizer));
        slogc_relation_version rel_version = parse_rel_version(tokenizer);
        std::vector<int> reorders(parse_reorder_list(tokenizer));
        if (ra_type == "acopy") {
            ptr = std::make_shared<slogc_ra_operation_acopy>(
                output_rel_name, input_rel_name, rel_version, reorders,
                parent);
        } else {
            ptr = std::make_shared<slogc_ra_operation_copy>(
                output_rel_name, input_rel_name, rel_version, reorders,
                parent);
        }
    } else if (ra_type == "join") {
        std::string output_rel_name(parse_sym(tokenizer));
        std::string input1_rel_name(parse_sym(tokenizer));
        slogc_relation_version rel1_version = parse_rel_version(tokenizer);
        std::string input2_rel_name(parse_sym(tokenizer));
        slogc_relation_version rel2_version = parse_rel_version(tokenizer);
        std::vector<int> reorders(parse_reorder_list(tokenizer));
        ptr = std::make_shared<slogc_ra_operation_join>(
            output_rel_name, input1_rel_name, rel1_version,
            input2_rel_name, rel2_version, reorders,
            parent);
    } else if (ra_type == "copy-generate") {
        auto new_copy_generate = std::make_shared<slogc_ra_operation_copy_generate>();
        new_copy_generate->output_rel_name = parse_sym(tokenizer);
        new_copy_generate->input_rel_name = parse_sym(tokenizer);
        new_copy_generate->version = parse_rel_version(tokenizer);
        new_copy_generate->extern_func = parse_ra_external_function(tokenizer, new_copy_generate);
        new_copy_generate->parent = parent;
        ptr = new_copy_generate;
    } else if (ra_type == "aggregation") {
        std::string output_rel_name(parse_sym(tokenizer));
        std::string input1_rel_name(parse_sym(tokenizer));
        std::string input2_rel_name(parse_sym(tokenizer));
        slogc_relation_version rel_version = parse_rel_version(tokenizer);
        std::string local_func_name(parse_sym(tokenizer));
        std::string agg_type(parse_sym(tokenizer));
        std::string reduce_func_name(parse_sym(tokenizer));
        std::string global_func_name(parse_sym(tokenizer));
        std::vector<int> reorders(parse_reorder_list(tokenizer));
        ptr = std::make_shared<slogc_ra_operation_aggregation>(
            output_rel_name, input1_rel_name, input2_rel_name,
            rel_version, local_func_name, agg_type,
            reduce_func_name, global_func_name, reorders,
            parent);
    } else {
        throw std::runtime_error("syntax error: Unknown RA type");
    }
    parse_rparen(tokenizer);
    return ptr;
}

std::vector<slogc_ra_operation_ptr>
parse_scc_ra_operation_list(slogc_tokenizer &tokenizer, std::weak_ptr<slogc_scc_decl> parent) {
    parse_lparen(tokenizer);
    std::vector<slogc_ra_operation_ptr> ra_ops;
    bool unfinished_flag = true;
    while (tokenizer.next()) {
        const slogc_token &tok_ll = tokenizer.current();
        if (tok_ll.type == slogc_token_type::right_paren) {
            unfinished_flag = false;
            break;
        } else {
            tokenizer.putback();
        }
        ra_ops.push_back(parse_ra_operation(tokenizer, parent));
        
    }
    if (unfinished_flag)
        throw std::runtime_error("syntax error: missing right par in ra operation list");

    return ra_ops;
}

std::shared_ptr<slogc_scc_decl>
parse_scc_decl(slogc_tokenizer &tokenizer, std::weak_ptr<slogc_prog> parent) {
    parse_lparen(tokenizer);
    auto new_scc_decl = std::make_shared<slogc_scc_decl>();
    // print_token(tokenizer.current());
    std::string scc_decl_sym(parse_sym(tokenizer));

    if (scc_decl_sym != "scc-decl") {
        throw std::runtime_error("syntax error: missing `scc-decl` tag");
    }
        

    new_scc_decl->name = parse_sym(tokenizer);
    new_scc_decl->id = parse_int(tokenizer);
    new_scc_decl->fixpoint_flag = parse_bool(tokenizer);
    new_scc_decl->scc_rels = parse_scc_rel_list(tokenizer, new_scc_decl);
    new_scc_decl->ra_operations = parse_scc_ra_operation_list(tokenizer, new_scc_decl);
    new_scc_decl->parent = parent;

    parse_rparen(tokenizer);

    return new_scc_decl;
}

std::vector<std::shared_ptr<slogc_scc_decl>>
parse_scc_decl_list(slogc_tokenizer &tokenizer, std::weak_ptr<slogc_prog> parent) {
    parse_lparen(tokenizer);
    std::vector<std::shared_ptr<slogc_scc_decl>> sccs;
    bool unfinished_flag = true;
    while (tokenizer.next()) {
        const slogc_token &tok_ll = tokenizer.current();
        if (tok_ll.type == slogc_token_type::right_paren) {
            unfinished_flag = false;
            break;
        } else {
            tokenizer.putback();
        }
        auto new_scc = parse_scc_decl(tokenizer, parent);
        // std::cout << new_scc->name << std::endl;
        sccs.push_back(new_scc);
    }
    if (unfinished_flag)
        throw std::runtime_error("syntax error: missing right par in ra operation list");

    return sccs;
}

std::vector<slogc_order_pair>
parse_scc_order(slogc_tokenizer &tokenizer) {
    parse_lparen(tokenizer);
    std::vector<slogc_order_pair> pairs;
    bool unfinished_flag = true;
    while (tokenizer.next()) {
        const slogc_token &tok_ll = tokenizer.current();
        if (tok_ll.type == slogc_token_type::right_paren) {
            unfinished_flag = false;
            break;
        } else {
            tokenizer.putback();
        }
        parse_lparen(tokenizer);
        int from = parse_int(tokenizer);
        int to = parse_int(tokenizer);
        parse_rparen(tokenizer);
        pairs.push_back(std::make_pair(from, to));
    }
    if (unfinished_flag)
        throw std::runtime_error("syntax error: missing right par in ra operation list");

    return pairs;
}

std::shared_ptr<slogc_prog>
parse_prog(slogc_tokenizer &tokenizer) {
    parse_lparen(tokenizer);

    auto new_prog = std::make_shared<slogc_prog>();
    std::string prog_sym(parse_sym(tokenizer));
    if (prog_sym != "slog-prog")
        throw std::runtime_error("syntax error: missing prog token.");
    new_prog->relation_decls = parse_relation_decl_list(tokenizer, new_prog);
    new_prog->scc_decls = parse_scc_decl_list(tokenizer, new_prog);
    new_prog->scc_orders = parse_scc_order(tokenizer);

    parse_rparen(tokenizer);

    return new_prog;
}

class lie_visitor : public slogc_visitor {

public:
    lie_visitor(LIE *lie, char *input_dir) : lie(lie), input_dir(input_dir) {
        max_rel_tag_counter = 256;
        max_scc_id_counter = 0;
    }

    void visit(std::shared_ptr<slogc_prog> node) override {
        for (auto rel_decl : node->relation_decls) {
            visit(rel_decl);
        }
        for (auto scc_decl : node->scc_decls) {
            visit(scc_decl);
        }

        for (auto &p : node->scc_orders) {
            if (id_scc_map.find(p.first) == id_scc_map.end() ||
                id_scc_map.find(p.second) == id_scc_map.end()) {
                std::cout << "SCC not exists!!! " << std::endl;
            }
            lie->add_scc_dependance(id_scc_map[p.first], id_scc_map[p.second]);
        }
    }

    void visit(std::shared_ptr<slogc_relation_decl> node) override {
        int rel_tag;
        if (rel_name_tag_map.find(node->name) == rel_name_tag_map.end()) {
            rel_tag = max_rel_tag_counter++;
        } else {
            rel_tag = rel_name_tag_map[node->name];
        }

        std::string fname = std::to_string(rel_tag) + "." + node->rel_name + "." + std::to_string(node->arity) + ".table";
        relation *new_rel;
        if (node->canonical_flag) {
            new_rel = new relation(node->jcc, node->canonical_flag,
                                   node->arity, rel_tag,
                                   fname,
                                   FULL);
        } else {
            new_rel = new relation(node->jcc, node->canonical_flag,
                                   node->arity, rel_tag,
                                   fname, std::string(input_dir) + "/" + fname,
                                   FULL);
        }
        lie->add_relation(new_rel);
        rel_map[node->name] = new_rel;
    }

    void visit(std::shared_ptr<slogc_ra_operation_fact> node) override {
        std::vector<u64> tuple;
        for (auto c : node->v) {
            u64 d;
            std::visit(
                dynamic_dispatch{
                    [d](std::string s) mutable { d = s2d(s); },
                    [d](float n) mutable { d = n2d((int)n); }},
                c.val);
            tuple.push_back(d);
        }
        _current_scc->add_rule(new fact(
            rel_map[node->rel_name], tuple));
    }

    void visit(std::shared_ptr<slogc_ra_operation_acopy> node) override {
        _current_scc->add_rule(new parallel_acopy(
            rel_map[node->output_rel_name],
            rel_map[node->input_rel_name],
            node->version == slogc_relation_version::DELTA ? DELTA : FULL,
            node->reorder_mapping));
    }

    void visit(std::shared_ptr<slogc_ra_operation_copy> node) override {
        _current_scc->add_rule(new parallel_copy(
            rel_map[node->output_rel_name],
            rel_map[node->input_rel_name],
            node->version == slogc_relation_version::DELTA ? DELTA : FULL,
            node->reorder_mapping));
    }

    void visit(std::shared_ptr<slogc_ra_operation_join> node) override {
        _current_scc->add_rule(new parallel_join(
            rel_map[node->output_rel_name],
            rel_map[node->input1_rel_name],
            node->input1_version == slogc_relation_version::DELTA ? DELTA : FULL,
            rel_map[node->input2_rel_name],
            node->input2_version == slogc_relation_version::DELTA ? DELTA : FULL,
            node->reorder_mapping));
    }

    void visit(std::shared_ptr<slogc_ra_operation_copy_generate> node) override {
        auto func = node->extern_func;

        _current_scc->add_rule(new parallel_copy_generate(
            rel_map[node->output_rel_name],
            rel_map[node->input_rel_name],
            node->version == slogc_relation_version::DELTA ? DELTA : FULL,
            copy_gen_functor(func, builtin_map(func->cpp_func_name))));
    }

    // void visit(std::shared_ptr<slogc_ra_external_function> node) override {}

    void visit(std::shared_ptr<slogc_ra_operation_aggregation> node) override {
        _current_scc->add_rule(new parallel_join_aggregate(
            rel_map[node->output_rel_name],
            rel_map[node->input1_rel_name],
            rel_map[node->input2_rel_name],
            node->rel_version == slogc_relation_version::DELTA ? DELTA : FULL,
            agg_local_map(node->local_func_name),
            SpecialAggregator::none,
            agg_reduce_map(node->reduce_func_name),
            nullptr,
            node->reorder_mapping));
    }

    void visit(std::shared_ptr<slogc_scc_decl> node) override {
        RAM *new_scc = new RAM(node->fixpoint_flag, node->id);
        _current_scc = new_scc;
        for (auto sr : node->scc_rels) {
            visit(sr);
        }

        auto _this_visitor = this;

        for (auto ra : node->ra_operations) {
            std::visit(dynamic_dispatch{[_this_visitor](auto ra_v) { _this_visitor->visit(ra_v); }}, ra);
        }

        lie->add_scc(new_scc);
        id_scc_map[node->id] = new_scc;
    }

    void visit(std::shared_ptr<slogc_scc_rel> node) override {
        _current_scc->add_relation(rel_map[node->name], node->dynamic_flag, node->delete_flag);
    }

    // void visit(std::shared_ptr<slo>)

private:
    LIE *lie;
    char *input_dir;
    int max_rel_tag_counter;
    int max_scc_id_counter;
    std::map<std::string, relation *> rel_map;
    std::map<std::string, int> rel_name_tag_map;
    std::map<int, RAM *> id_scc_map;

    RAM *_current_scc;
    // parallel_copy_generate* _current_copy_gen;
};

int main(int argc, char *argv[]) {

    std::string help =
        "Slog toolchain: a language, compiler, and runtime system for implementing data-parallel deductive programming. \n"
        "This backend execution engine for compiled slog ir (.slogc) file. \n"
        "Usage : mpirun -np <cores> slog <ir_file> <input_dir> <output_dir> \n"
        "User defined builtin/aggregator can be passed through environment variable `SLOG_EXT_LIBRARY`.";

    if (argc != 4) {
        std::cout << "Arguments wrong! \n\n"
                  << help << std::endl;
        return 22;
    }

    char *ir_file = argv[1];
    char *input_dir = argv[2];
    char *output_dir = argv[3];

    std::ifstream ir_f(ir_file);
    slogc_tokenizer tokenizer(ir_f);
    auto ast_root = parse_prog(tokenizer);

    LIE *lie = new LIE();
    lie_visitor visitor(lie, input_dir);
    visitor.visit(ast_root);

    mpi_comm mcomm;
    mcomm.create(argc, argv);

    lie->enable_data_IO();
    lie->enable_IO();
    lie->set_output_dir(output_dir); // Write to this directory
    lie->set_comm(mcomm);
    lie->set_batch_size(1);
    lie->execute();
    lie->print_all_relation_size(); // Continuously print relation sizes
    lie->stat_intermediate();

    delete lie;

    mcomm.destroy();

    return 0;
}
