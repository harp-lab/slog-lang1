/*
 * scc (tasks)
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */



#include "../parallel_RA_inc.h"
#include <iostream>

RAM::~RAM()
{
    loop_count_tracker = 0;
    for (std::vector<parallel_RA*>::iterator it = RA_list.begin() ; it != RA_list.end(); ++it)
        delete (*it);
}

/// Example: RAM* scc13237 = new RAM(true, 1);
/// true: run this scc till fixed point is reached 
/// false: run this scc for only one iteration, for copy and acopy rules
/// 1: id of scc (used for internal debugging)
RAM::RAM (bool ic, int r_id)
{
    ram_relation_count = 0;
    loop_count_tracker = 0;
    if (ic==false)
        iteration_count=1;
    ram_id = r_id;
    RA_list = {};
}


/// Example
/// rel_edge_2_2: relation that is being added to the SCC
/// false: keep the delta and full the way they are 
/// true: move whatever is in full to delta, once before the start of the fixed point loop)
void RAM::add_relation(relation*& G, bool i_status, bool gc_flag)
{
    //ram_relations[ram_relation_count] = G;
    ram_relations.push_back(G);
    ram_relation_status.push_back(i_status);
    if (gc_flag) {
        // TODO gc_relations GC
        gc_relations.push_back(G);
    }
    //ram_relation_status[ram_relation_count] = i_status;
    ram_relation_count++;
}

void RAM::set_comm(mpi_comm& mcomm)
{
    this->mcomm = mcomm;

    for (parallel_RA* ra : RA_list)
        ra->set_comm(mcomm);

    for (u32 i=0; i < ram_relation_count; i++)
        ram_relations[i]->set_mcomm(mcomm);
}

void RAM::print_all_relation()
{
    for (u32 i=0; i < ram_relation_count; i++)
        ram_relations[i]->print();
}

bool RAM::contains_relation(int tag) {
    for (auto rel : ram_relations) {
        if (rel->get_intern_tag() == tag) {
            return true;
        }
    }
    return false;
}
