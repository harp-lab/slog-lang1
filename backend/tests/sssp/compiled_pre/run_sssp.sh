#!/bin/bash

for i in {1..5}
do
    mpirun -np 1 ./build/sssp ./input-data ./ $i || exit 1;
done
