# Slog vs. Souffle benchmarking code

This directory allows us to benchmark slog vs. souffle for terms of
various sizes. The main entry point is `gen-worstcase-terms.sh`, which
accepts two command-line arguments, n (the term size) and m (the
polyvariance for m-CFA). This generates two "k-CFA tiny" (really
m-CFA) terms for both slog and souffle with the specified
configuration.

## Building an individual term size (n) / polyvariance (m) combo

Done via the following bash script:

    bash gen-worstcase-terms.sh 1 2 # generates worstcase-1-terms-2-m.{dl|slog}

## Building all benchmarks

Done via the following Racket program:

    racket build-experiments.rkt
    