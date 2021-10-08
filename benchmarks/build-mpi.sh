# Generates Slog MPI run for a program
#
# Takes two arguments:
# ./build-mpi.sh <program-name> <slog-source-file>
# Generates:
# <program-name>-output (where output is written)
# <program-name>-input (where input is written)
# Completed program in <program-name>-output
input_dir=$1-input
output_dir=$1-output
here=$(pwd)
mkdir $input_dir &>/dev/null
mkdir $output_dir &>/dev/null
cd ../compiler; racket slog.rkt -c --build-input-db $here/$input_dir --output-db $here/$output_dir $here/$2
cd $here

