# parallel-RA
Parallel Relational Algebra

Copyright (c) Sidharth Kumar, Thomas Gilray, Kristopher Micinski, see License.md


## Build instructions
mkdir build

ccmake ../

make

This will create three executable TC, freevars, kcfa. TC stands for transitive closure

## Running instructions
mpirun -n 5 ./TC <input_file>

We have two example input files in the data folder 

../data/g13236/edge_2_1_2 (this has 60 edges and the output TC generates 573 edges)

../data/g5955/edge_2_1_2 (this has 4 edges and the output TC generates 28 edges)


Utility binary_parser can be used to print the content of the input file. Usage:

./bin_parser ../data/g5955/edge_2_1_2

Filename ../data/g5955/edge_2_1_2.size Row Count 4 Column count 3

3 4 18014398777917437 

2 3 18014398777917439 

1 2 18014398777917436 

0 1 18014398777917438 

The third column is the id column which we add for any input dataset.



If you have your own input graph for which you want to compute the transitive closure, then use the utility tsv_to_bin to generate data format that could be fed to the code. The utility can be run as follows:

./tsv_to_bin input_tsv output

This will create a file (output) with data written in binary along with a metadata file (output.size) that tells the total number of rows and columns. The utillity adds an extra column and therefore you will see 3 for total number of columns. Once you have the data in the correct format, you can use it with TC.

## How do I see the output:

Currently we are reingineering our parallel I/O system, the outputting part to the dump the output TC to a binary file is something that we are currently working on. For now you can print the size of the TC by making a call lie->print_all_relation_size(); (line 55 of tests/transitive_closure.cpp) and also print out the content of the TC to the std output by enabling un-commenting line 54 (#define DEBUG_OUTPUT 1) in parallel_RA_inc.h. Note that this will print a lot of  information to the console and is recommended only to debug the code at one process run.

### Please contact Sidharth Kumar sid14@uab.edu if you have any problems running the code
