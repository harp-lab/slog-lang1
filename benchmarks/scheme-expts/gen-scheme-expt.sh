# Generates Slog and Souffle inputs for a program
#
# Takes three arguments
# ./gen-scheme-expt.sh <program> <source-file>
echo "Generating compiled code (Slog+Souffle) for experiment $1"
program=$1
here=$(pwd)
fact_dir=souffle-facts-$program
slog_dir=$here/$1-slog
mkdir $fact_dir &>/dev/null
output=$here/../souffle-compiled-$program
cat scheme-mcfa.slog >$program.slog
racket scheme-fact-gen.rkt $2 $fact_dir >>$program.slog
# echo "-- Compiling Souffle sources --"
# echo "-- Output in parent directory --"
# for cores in 1 2 4 8 16
# do
#     "-- Compiling Souffle $cores cores --"
#     echo "souffle -g -j$cores -o $output-$cores-cores scheme_mcfa.dl -F $fact_dir"
#     time souffle -g -j$cores -o $output-$cores-cores scheme_mcfa.dl -F $fact_dir 
# done
echo "-- Compiling Slog source (output db in $slog_dir) --"
echo "-- Output in parent directory --"
cd $here/..
cp $here/$program.slog $here/..
bash build-mpi.sh $program $program.slog
