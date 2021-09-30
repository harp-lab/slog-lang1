#include "../src/parallel_RA_inc.h"


int main(int argc, char **argv)
{
#if 0
    mpi_comm mcomm;
    mcomm.create(argc, argv);

#if 0
    relation* rel_bar_1_1 = new relation(1, 1, 258, "rel_bar_1_1", "/var/tmp/g5390/bar_1_1", FULL);
    relation* rel_foo_2_2_1 = new relation(2, 2, 256, "rel_foo_2_2_1", "/var/tmp/g5390/foo_2_2_1", FULL);

    RAM* scc5391 = new RAM(false, 1);
    scc5391->add_relation(rel_foo_2_2_1, false);
    scc5391->add_relation(rel_bar_1_1, true);
    scc5391->add_rule(new parallel_copy_filter(rel_bar_1_1, rel_foo_2_2_1, FULL, {1}, [](const u64* const data){ return (data[0] == data[1]); }));

    LIE* lie = new LIE();
    lie->add_relation(rel_bar_1_1);
    lie->add_relation(rel_foo_2_2_1);
    lie->add_scc(scc5391);
    lie->set_comm(mcomm);
    lie->set_batch_size(1);
    lie->execute();
#endif

#if 0
    relation* rel_bar_2_1_2 = new relation(2, 2, 256, "rel_bar_2_1_2", "/var/tmp/g5472/bar_2_1_2", FULL);
    relation* rel_foo_2_2_1 = new relation(2, 2, 257, "rel_foo_2_2_1", "/var/tmp/g5472/foo_2_2_1", FULL);

    RAM* scc5473 = new RAM(false, 1);
    scc5473->add_relation(rel_foo_2_2_1, false);
    scc5473->add_relation(rel_bar_2_1_2, true);
    scc5473->add_rule(new parallel_copy_filter(rel_bar_2_1_2, rel_foo_2_2_1, FULL, {1, 0}, [](const u64* const data){ return !(data[0] == data[1]); }));

    LIE* lie = new LIE();
    lie->add_relation(rel_bar_2_1_2);
    lie->add_relation(rel_foo_2_2_1);
    lie->add_scc(scc5473);
    lie->set_comm(mcomm);
    lie->set_batch_size(1);
    lie->execute();
#endif

#if 0
    relation* rel_inter_body8_1_1 = new relation(1, 1, 257, "rel_inter_body8_1_1", "/var/tmp/g5504/inter-body8_1_1", FULL);
    relation* rel_bar_1_1 = new relation(1, 1, 258, "rel_bar_1_1", "/var/tmp/g5504/bar_1_1", FULL);
    relation* rel_foo_2_2 = new relation(1, 2, 256, "rel_foo_2_2", "/var/tmp/g5504/foo_2_2", FULL);
    relation* rel_foo_2_2_1 = new relation(2, 2, 256, "rel_foo_2_2_1", "/var/tmp/g5504/foo_2_2_1", FULL);

    RAM* scc5505 = new RAM(false, 1);
    scc5505->add_relation(rel_foo_2_2, false);
    scc5505->add_relation(rel_bar_1_1, true);
    scc5505->add_relation(rel_inter_body8_1_1, false);
    scc5505->add_rule(new parallel_join(rel_bar_1_1, rel_inter_body8_1_1, FULL, rel_foo_2_2, FULL, {3}));

    RAM* scc5506 = new RAM(false, 3);
    scc5506->add_relation(rel_foo_2_2_1, true);
    scc5506->add_relation(rel_foo_2_2, true);
    scc5506->add_rule(new parallel_acopy(rel_foo_2_2, rel_foo_2_2_1, DELTA, {1, 2, 0}));

    RAM* scc5507 = new RAM(false, 2);
    scc5507->add_relation(rel_foo_2_2_1, false);
    scc5507->add_relation(rel_inter_body8_1_1, true);
    scc5507->add_rule(new parallel_copy_filter(rel_inter_body8_1_1, rel_foo_2_2_1, FULL, {1}, [](const u64* const data){ return !(data[0] == data[1]); }));

    LIE* lie = new LIE();
    lie->add_relation(rel_inter_body8_1_1);
    lie->add_relation(rel_bar_1_1);
    lie->add_relation(rel_foo_2_2);
    lie->add_relation(rel_foo_2_2_1);
    lie->add_scc(scc5505);
    lie->add_scc(scc5506);
    lie->add_scc(scc5507);
    lie->add_scc_dependance(scc5506, scc5505);
    lie->add_scc_dependance(scc5507, scc5505);
    lie->set_comm(mcomm);
    lie->set_batch_size(1);
    lie->execute();
#endif

    relation* rel_foo_2_1_2 = new relation(2, 2, true, 256, "rel_foo_2_1_2", "/var/tmp/g5455/foo_2_1_2", FULL);
    relation* rel_inter_body8_1_1 = new relation(1, 1, true, 257, "rel_inter_body8_1_1", "/var/tmp/g5455/inter-body8_1_1", FULL);
    relation* rel_bar_1_1 = new relation(1, 1, true, 259, "rel_bar_1_1", "/var/tmp/g5455/bar_1_1", FULL);
    relation* rel_foo_2_2 = new relation(1, 2, false, 256, "rel_foo_2_2", "/var/tmp/g5455/foo_2_2", FULL);

    RAM* scc5456 = new RAM(false, 1);
    scc5456->add_relation(rel_foo_2_2, true);
    scc5456->add_relation(rel_foo_2_1_2, true);
    scc5456->add_rule(new parallel_acopy(rel_foo_2_2, rel_foo_2_1_2, DELTA, {1, 2, 0}));

    RAM* scc5457 = new RAM(false, 3);
    scc5457->add_relation(rel_inter_body8_1_1, true);
    scc5457->add_relation(rel_foo_2_1_2, false);
    scc5457->add_rule(new parallel_copy_filter(rel_inter_body8_1_1, rel_foo_2_1_2, FULL, {1}, [](const u64* const data){ return (data[0] == data[1]); }));

    RAM* scc5458 = new RAM(false, 2);
    scc5458->add_relation(rel_foo_2_2, false);
    scc5458->add_relation(rel_bar_1_1, true);
    scc5458->add_relation(rel_inter_body8_1_1, false);
    scc5458->add_rule(new parallel_join(rel_bar_1_1, rel_inter_body8_1_1, FULL, rel_foo_2_2, FULL, {3}));

    LIE* lie = new LIE();
    lie->add_relation(rel_foo_2_1_2);
    lie->add_relation(rel_inter_body8_1_1);
    lie->add_relation(rel_bar_1_1);
    lie->add_relation(rel_foo_2_2);
    lie->add_scc(scc5456);
    lie->add_scc(scc5457);
    lie->add_scc(scc5458);
    lie->add_scc_dependance(scc5456, scc5458);
    lie->add_scc_dependance(scc5457, scc5458);


    lie->set_comm(mcomm);
    lie->set_batch_size(1);
    lie->execute();




    rel_bar_1_1->print();
    //rel_bar_2_1_2->print();


    mcomm.destroy();
    return 0;
#endif
}
