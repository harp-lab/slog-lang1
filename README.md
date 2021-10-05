### Slog: Data-Parallel Deductive Programming

![CI](https://github.com/harp-lab/slog-lang/workflows/CI/badge.svg)

Slog is a language, compiler, and runtime system for implementing
data-parallel deductive programming.

## Components

- `compiler/`         -- Compiler from Slog source code to a parallel RA plan
- `backend/`          -- MPI-based parallel relational algebra plan (RA) runtime
- `slog/`             -- Python code for interacting with Slog:
- `slog/protobufs/`   -- Wire-level protocol
- `slog/daemon/`      -- Server for managing, queuing, and storing Slog jobs
- `slog/repl/`        -- Interactive CLI for Slog

## Building and Executing

![cli](./cli.gif)

## Using Docker ##

```sh
docker build -t slog-daemon .
docker run -d --name slog-server slog-daemon
## To run a script on the server
# IP was found by `docker network inspect bridge` and getting the IPV4 address
./runslog 172.17.0.2 tests/tc.slog
## To go onto the REPL
docker exec -it slog-server bash
./slog-repl
```
