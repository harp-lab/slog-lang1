# Generates Slog and Souffle inputs for a program
#
# Takes three arguments
# ./gen-scheme-expt.sh <experiment-name> <source-file> <core-count>
echo "Generating compiled code (Slog+Souffle) for experiment $1"
program=$1
here=$(pwd)
fact_dir=souffle-facts-$program
slog_dir=$(pwd)/$1-slog
slog_input=$slog_dir/input
mkdir $fact_dir &>/dev/null
output_exe=souffle-compiled-$program
cat scheme-mcfa.slog >$program.slog
racket scheme-fact-gen.rkt $2 $fact_dir >>$program.slog
echo "Compiling Souffle source using $3 cores"
souffle -c -j$3 -o $output_exe scheme_mcfa.dl -F $fact_dir &>/dev/null
echo "Compiling Slog source to C++ (output db in $slog_dir)"
rm -rf ../../data/scheme-mcfa-input
cd ../../compiler; racket slog.rkt -o -c --output-db $slog_dir $here/$program.slog
cd $here
cp -r ../../data/$program-input $slog_input
cp CMakeLists.txt $slog_dir
sed 's/..\/src\/parallel_RA_inc.h/..\/..\/..\/backend\/src\/parallel_RA_inc.h/g' $slog_dir/$program.cpp >$slog_dir/target.cpp
mv $slog_dir/target.cpp $slog_dir/$program.cpp
cd $slog_dir
echo "Compiling Slog C++ -> Binary"
cmake . &>/dev/null
make &>/dev/null
