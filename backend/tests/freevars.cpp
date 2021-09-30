#include "../src/parallel_RA_inc.h"


int main(int argc, char **argv)
{
    mpi_comm mcomm;
    mcomm.create(argc, argv);

#if 1
    relation* rel_inter_body27_3_1_2_3 = new relation(3, true, 3, 258, "rel_inter_body27_3_1_2_3", "/var/tmp/g6122/inter-body27_3_1_2_3", FULL);
    relation* rel_var_ref_1_1 = new relation(1, true, 1, 257, "rel_var_ref_1_1", "/var/tmp/g6122/var-ref_1_1", FULL);
    relation* rel_app_2_1_2 = new relation(2, true, 2, 256, "rel_app_2_1_2", "/var/tmp/g6122/app_2_1_2", FULL);
    relation* rel_args_1_1 = new relation(1, true, 1, 262, "rel_args_1_1", "/var/tmp/g6122/args_1_1", FULL);
    relation* rel_app_2_1 = new relation(1, false, 2, 256, "rel_app_2_1", "/var/tmp/g6122/app_2_1", FULL);
    relation* rel_app_2_2 = new relation(1, false, 2, 256, "rel_app_2_2", "/var/tmp/g6122/app_2_2", FULL);
    relation* rel_lambda_2_2 = new relation(1, false, 2, 261, "rel_lambda_2_2", "/var/tmp/g6122/lambda_2_2", FULL);
    relation* rel_inter_body25_3_2 = new relation(1, false, 3 , 259, "rel_inter_body25_3_2", "/var/tmp/g6122/inter-body25_3_2", FULL);
    relation* rel_lambda_2_1_2 = new relation(2, true, 2 , 261, "rel_lambda_2_1_2", "/var/tmp/g6122/lambda_2_1_2", FULL);
    relation* rel_inter_body27_3_3_2 = new relation(2, false, 3 , 258, "rel_inter_body27_3_3_2", "/var/tmp/g6122/inter-body27_3_3_2", FULL);
    relation* rel_free_2_1_2 = new relation(2, true, 2, 260, "rel_free_2_1_2", "/var/tmp/g6122/free_2_1_2", FULL);
    relation* rel_args_1_0 = new relation(1, true, 1, 262, "rel_args_1_0", "/var/tmp/g6122/args_1_0", FULL);
    relation* rel_free_2_2 = new relation(1, false, 2 , 260, "rel_free_2_2", "/var/tmp/g6122/free_2_2", FULL);
    relation* rel_inter_body25_3_1_2_3 = new relation(3, true, 3 , 259, "rel_inter_body25_3_1_2_3", "/var/tmp/g6122/inter-body25_3_1_2_3", FULL);
    relation* rel_var_ref_1_ = new relation(0, false, 1 , 257, "rel_var_ref_1_", "/var/tmp/g6122/var-ref_1_", FULL);

    RAM* scc6123 = new RAM(false, 1);
    scc6123->add_relation(rel_app_2_1, true);
    scc6123->add_relation(rel_app_2_1_2, true);
    scc6123->add_rule(new parallel_acopy(rel_app_2_1, rel_app_2_1_2, DELTA, {0, 2, 1}));

    RAM* scc6124 = new RAM(false, 5);
    scc6124->add_relation(rel_args_1_0, true);
    scc6124->add_relation(rel_args_1_1, true);
    scc6124->add_rule(new parallel_acopy(rel_args_1_0, rel_args_1_1, DELTA, {1, 0}));

    RAM* scc6125 = new RAM(false, 3);
    scc6125->add_relation(rel_var_ref_1_, true);
    scc6125->add_relation(rel_var_ref_1_1, true);
    scc6125->add_rule(new parallel_acopy(rel_var_ref_1_, rel_var_ref_1_1, DELTA, {1, 0}));

    RAM* scc6126 = new RAM(true, 7);
    scc6126->add_relation(rel_inter_body25_3_1_2_3, true);
    scc6126->add_relation(rel_free_2_2, true);
    scc6126->add_relation(rel_args_1_0, false);
    scc6126->add_relation(rel_free_2_1_2, true);
    scc6126->add_relation(rel_inter_body27_3_3_2, true);
    scc6126->add_relation(rel_inter_body25_3_2, true);
    scc6126->add_relation(rel_lambda_2_2, false);
    scc6126->add_relation(rel_app_2_2, false);
    scc6126->add_relation(rel_app_2_1, false);
    scc6126->add_relation(rel_inter_body27_3_1_2_3, true);
    scc6126->add_rule(new parallel_acopy(rel_free_2_2, rel_free_2_1_2, DELTA, {1, 2, 0}));
    scc6126->add_rule(new parallel_acopy(rel_inter_body25_3_2, rel_inter_body25_3_1_2_3, DELTA, {1, 3, 0, 2}));
    scc6126->add_rule(new parallel_join(rel_free_2_1_2, rel_app_2_2, FULL, rel_free_2_2, DELTA, {4, 1}));
    scc6126->add_rule(new parallel_join(rel_free_2_1_2, rel_free_2_2, DELTA, rel_app_2_1, FULL, {2, 3}));
    scc6126->add_rule(new parallel_join(rel_inter_body27_3_1_2_3, rel_args_1_0, FULL, rel_inter_body25_3_2, DELTA, {3, 1, 4}));
    scc6126->add_rule(new parallel_acopy(rel_inter_body27_3_3_2, rel_inter_body27_3_1_2_3, DELTA, {2, 1, 3, 0}));
    scc6126->add_rule(new parallel_join(rel_inter_body25_3_1_2_3, rel_free_2_2, DELTA, rel_lambda_2_2, FULL, {3, 4, 2}));
    scc6126->add_rule(new parallel_copy_filter(rel_free_2_1_2, rel_inter_body27_3_3_2, DELTA, {0, 3}, [](const u64* const data){ return !(data[0] == data[1]); }));

    RAM* scc6127 = new RAM(false, 2);
    scc6127->add_relation(rel_free_2_1_2, true);
    scc6127->add_relation(rel_var_ref_1_1, false);
    scc6127->add_rule(new parallel_copy(rel_free_2_1_2, rel_var_ref_1_1, FULL, {0, 1}));

    RAM* scc6128 = new RAM(false, 6);
    scc6128->add_relation(rel_lambda_2_1_2, true);
    scc6128->add_relation(rel_lambda_2_2, true);
    scc6128->add_rule(new parallel_acopy(rel_lambda_2_2, rel_lambda_2_1_2, DELTA, {1, 2, 0}));

    RAM* scc6129 = new RAM(false, 4);
    scc6129->add_relation(rel_app_2_2, true);
    scc6129->add_relation(rel_app_2_1_2, true);
    scc6129->add_rule(new parallel_acopy(rel_app_2_2, rel_app_2_1_2, DELTA, {1, 2, 0}));

    LIE* lie = new LIE();
    lie->add_relation(rel_inter_body27_3_1_2_3);
    lie->add_relation(rel_var_ref_1_1);//
    lie->add_relation(rel_app_2_1_2);//
    lie->add_relation(rel_args_1_1);
    lie->add_relation(rel_app_2_1);//
    lie->add_relation(rel_app_2_2);
    lie->add_relation(rel_lambda_2_2);
    lie->add_relation(rel_inter_body25_3_2);
    lie->add_relation(rel_lambda_2_1_2);
    lie->add_relation(rel_inter_body27_3_3_2);
    lie->add_relation(rel_free_2_1_2);//
    lie->add_relation(rel_args_1_0);
    lie->add_relation(rel_free_2_2);
    lie->add_relation(rel_inter_body25_3_1_2_3);
    lie->add_relation(rel_var_ref_1_);
    lie->add_scc(scc6123);
    lie->add_scc(scc6124);
    lie->add_scc(scc6125);
    lie->add_scc(scc6126);
    lie->add_scc(scc6127);
    lie->add_scc(scc6128);
    lie->add_scc(scc6129);
    lie->add_scc_dependance(scc6123, scc6126);
    lie->add_scc_dependance(scc6124, scc6126);
    lie->add_scc_dependance(scc6127, scc6126);
    lie->add_scc_dependance(scc6128, scc6126);
    lie->add_scc_dependance(scc6129, scc6126);

    lie->set_comm(mcomm);
    lie->set_batch_size(1);
    lie->execute();
    lie->print_all_relation_size();


    //rel_free_2_2->print();
    //rel_free_2_2->print();
    //rel_app_2_1->print();
    //rel_app_2_2->print();
    //rel_app_2_1_2->print();


    delete lie;
    mcomm.destroy();
    return 0;
#endif
}
