### A Language or Something

![CI](https://github.com/harp-lab/slog-lang/workflows/CI/badge.svg)



## Installation ##

Slog runs on Racket.

* Need binaryio, graph, rparallel, pmap

```sh
raco pkg install binaryio
raco pkg install graph
raco pkg install rparallel
raco pkg install pmap
```

(This is incomplete, these instructions are just for running the interpreter.)

## Running Slog ##

To run the (local) interpreter on a slog file:

```sh
racket slog.rkt -i /path/to/slog-file.slog
```
