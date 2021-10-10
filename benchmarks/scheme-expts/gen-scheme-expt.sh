# Generates Slog and Souffle inputs for a program
#
# Takes three arguments
# ./gen-scheme-expt.sh <experiment-name> <source-file> <core-count>
echo "Generating compiled code (Slog+Souffle) for experiment $1"
program=$1
here=$(pwd)
fact_dir=souffle-facts-$program
slog_dir=$here/$1-slog
mkdir $fact_dir &>/dev/null
output_exe=$here/../souffle-compiled-$program
cat scheme-mcfa.slog >$program.slog
racket scheme-fact-gen.rkt $2 $fact_dir >>$program.slog
echo "-- Compiling Souffle source (using $3 cores) --"
echo "-- Output in parent directory --"
echo "souffle -c -j$3 -o $output_exe scheme_mcfa.dl -F $fact_dir"
souffle -c -j$3 -o $output_exe scheme_mcfa.dl -F $fact_dir &>/dev/null
echo "-- Compiling Slog source (output db in $slog_dir) --"
echo "-- Output in parent directory --"
cd $here/..
cp $here/$program.slog $here/..
bash build-mpi.sh $program $program.slog
