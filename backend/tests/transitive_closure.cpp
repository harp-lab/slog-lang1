#include "../src/parallel_RA_inc.h"


void func(mpi_comm mcomm, int mode, const char* log_name, const char* input_name)
{
relation* T1 = new relation(1, true, 2, 257, "T", "../data/g5955/path_2_1_2", FULL);
relation* G1 = new relation(1, true, 2, 256, "G", input_name, FULL);

RAM* join1 = new RAM(true, 1);
join1->add_relation(G1, false);
join1->add_relation(T1, true);
join1->add_rule(new parallel_join(T1, T1, DELTA, G1, FULL, {3, 1}));
RAM* copy1 = new RAM(false, 0);
copy1->add_relation(G1, false);
copy1->add_relation(T1, true);
copy1->add_rule(new parallel_copy(T1, G1, FULL, {1, 0}));
LIE* lie1 = new LIE();
lie1->add_relation(T1);
lie1->add_relation(G1);
lie1->add_scc(join1);
lie1->add_scc(copy1);
lie1->add_scc_dependance(copy1, join1);
lie1->set_sloav_mode(mode);
lie1->enable_all_to_all_dump();
lie1->set_output_dir(log_name);
lie1->enable_IO();
lie1->set_name("TC");
lie1->set_comm(mcomm);
lie1->set_batch_size(10);
lie1->execute();
lie1->print_all_relation_size();
delete lie1;
}


int main(int argc, char **argv)
{
    mpi_comm mcomm;
    mcomm.create(argc, argv);

    func(mcomm, 0, "final-TC-412148-log_0_0", argv[1]);
    func(mcomm, 1, "final-TC-412148-log_0_1", argv[1]);
    func(mcomm, 3, "final-TC-412148-log_0_3", argv[1]);

    func(mcomm, 0, "final-TC-412148-log_1_0", argv[1]);
    func(mcomm, 1, "final-TC-412148-log_1_1", argv[1]);
    func(mcomm, 3, "final-TC-412148-log_1_3", argv[1]);

    func(mcomm, 0, "final-TC-412148-log_2_0", argv[1]);
    func(mcomm, 1, "final-TC-412148-log_2_1", argv[1]);
    func(mcomm, 3, "final-TC-412148-log_2_3", argv[1]);


    func(mcomm, 0, "final-TC-412148-log_3_0", argv[1]);
    func(mcomm, 1, "final-TC-412148-log_3_1", argv[1]);
    func(mcomm, 3, "final-TC-412148-log_3_3", argv[1]);


    func(mcomm, 0, "final-TC-drim-log_0_0", "../data/new_data/Drim");
    func(mcomm, 1, "final-TC-drim-log_0_1", "../data/new_data/Drim");
    func(mcomm, 3, "final-TC-drim-log_0_3", "../data/new_data/Drim");

    func(mcomm, 0, "final-TC-drim-log_1_0", "../data/new_data/Drim");
    func(mcomm, 1, "final-TC-drim-log_1_1", "../data/new_data/Drim");
    func(mcomm, 3, "final-TC-drim-log_1_3", "../data/new_data/Drim");

    func(mcomm, 0, "final-TC-drim-log_2_0", "../data/new_data/Drim");
    func(mcomm, 1, "final-TC-drim-log_2_1", "../data/new_data/Drim");
    func(mcomm, 3, "final-TC-drim-log_2_3", "../data/new_data/Drim");

    func(mcomm, 0, "final-TC-drim-log_3_0", "../data/new_data/Drim");
    func(mcomm, 1, "final-TC-drim-log_3_1", "../data/new_data/Drim");
    func(mcomm, 3, "final-TC-drim-log_3_3", "../data/new_data/Drim");

    mcomm.destroy();
    return 0;
}
