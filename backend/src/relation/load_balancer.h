#include "comm.h"
#include "balanced_hash_relation.h"

class load_balancer
{
private:
    mpi_comm mcomm;


public:
    load_balancer()
    {

    }

    bool load_balance_merge_full(relation* rel, float rf);
    bool load_balance_split_full(relation* rel, float rf);
    bool load_balance_merge_full_and_delta(relation* rel, float rf);
    bool load_balance_split_full_and_delta(relation* rel, float rf, int rc);
};
