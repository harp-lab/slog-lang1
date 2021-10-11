# Generates Slog MPI run for a program
#
# Takes two arguments:
# ./build-mpi.sh <program-name> <slog-source-file>
# Generates:
# <program-name>-output (where output is written)
# <program-name>-input (where input is written)
# Completed program in <program-name>-output
here=$(pwd)
input_dir=$here/$1-input
output_dir=$here/$1-output
rm -r $input_dir
rm -r $output_dir
mkdir $input_dir &>/dev/null
mkdir $output_dir &>/dev/null
echo "cd ../compiler; racket slog.rkt -c --build-input-db $input_dir --output-db $output_dir $here/$2"
cd ../compiler; racket slog.rkt -c --build-input-db $input_dir --output-db $output_dir $here/$2
cd $output_dir
cp $here/CMakeLists.txt .
echo "Running cmake"
cmake .
echo "Running make"
make
cd $here
