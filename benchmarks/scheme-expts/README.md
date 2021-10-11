# Scheme CFA experiments

- This directory contains a context-sensitive m-CFA analysis of a core
  subset of Scheme, supporting arbitrary-fixed-arity lambdas, let
  (with multiple bindings), primitive operations, set!, and call/cc.

- The Scheme analysis is written in scheme_mca_<N>.dl for N = 3-m-CFA
  and 5-m-CFA (differing only in analysis precision, which will
  concomitantly increase analysis complexity by a polynomial factor).

- The corresponding Slog analysis is written in scheme-mca-<N>.slog
  for N = 3-m-CFA and 5-m-CFA.

- Sample programs are included in the `scheme-programs/` folder, which
  includes several term generators (written in Racket) for a variety
  of terms that will elicit worst-case space complexity of the
  analysis.

- The Slog and Souffle versions of experiments must agree on all key
  relation sizes (and contents) to be considered correct (though the
  Slog version will, in general, include other intermediate
  relations).

# Generating Data

Once terms of interest are identified in scheme-programs/, an analysis
of the program may be complied to both Souffle and Slog:

    ./gen-scheme-expt.sh <expt-name> <expt-src> <core-count>

# Running experiments

A single Souffle run must specify the souffle fact input directory:

    ./souffle-compiled-<expt-name> -F souffle-facts-<expt-name>

Slog output is written in <expt-name>-output, and a built version is
included as `./target`. Running this will run slog. mpirun will use
MPI:

    mpirun -n <core-count> <expt-name>-output/target

