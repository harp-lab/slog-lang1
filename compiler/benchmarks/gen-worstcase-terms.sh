# generate slog and souffle kcfa-tiny for the same term size (n) and polyvariance (m)
if (( $# != 2 )); then
    echo "Expects two arguments: number of lambdas to generate for term, and polyvariance m"
    exit 1
fi

SIZE=$1
M=$2
OUT=worstcase-$SIZE-terms-$M-m.slog
OUTDL=worstcase-$SIZE-terms-$M-m.dl

# slog
racket kcfa-tiny-slog-generator.rkt $M >$OUT
racket worstcase-slog.rkt $SIZE >>$OUT

# souffle
racket kcfa-tiny-souffle-generator.rkt $M >$OUTDL
racket worstcase-souffle.rkt $SIZE >>$OUTDL
