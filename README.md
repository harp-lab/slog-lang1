### Slog: Data-Parallel Deductive Programming

Slog is a language, compiler, and runtime system for implementing
data-parallel deductive programming.

## Components

- `compiler/`    -- Compiler from Slog source code to a parallel RA plan
- `protobufs/`   -- Wire-level protocol
- `parallel-ra/` -- MPI-based parallel relational algebra plan (RA) runtime
- `daemon/`      -- Server for managing, queuing, and storing Slog jobs
- `repl/`        -- Interactive CLI for Slog
