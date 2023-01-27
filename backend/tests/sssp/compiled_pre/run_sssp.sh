#!/bin/bash

for i in {0..5000}
do
    /usr/bin/time --verbose mpirun --use-hwthread-cpus -np 128 ./build/sssp /home/ubuntu/workspace/dataset/livejournal-bin ../ $i || exit 2;
done
