
# Backend change for adding negagtion/aggreagattion


## Overview

Aggregation/Negagtion is considered as special variant of `join` operation. In raltion algebera it can be considered as a operation similar to a left outer join.

## Possible file needed to be modified


1. a new enum branch in `balanced_hash_relation` called `NEGATION`.
2. a new if bran case in `RA_task.cpp` to check:
```
else if ((*it)->get_RA_type() == NEGATION)
```
and inside this work both on `FULL` and `DELTA`.
3. add a new file called `parallel_agg.cpp/.h` under `src/relation`. It contain a API as frontend's compilation result.
   This new class contain a `local_...` function having same argument as `local_join` in `parallel_join.h`.
4. a new function in `shmap_relation.h/.cpp` doing `out_join`, having same argument as `as_all_to_allv_right_join_buffer`.
   In new function return when found a prefix in trie, if not trying to add that value to all_to_all_buffer
5. all code related to `JOIN` in `RA_task.cpp` shall have a variant of negation code
