### Slog: Data-Parallel Deductive Programming

<!-- ![CI](https://github.com/harp-lab/slog-lang/workflows/CI/badge.svg) -->

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

## CSPA test (using Docker)
CSPA test program is located under `analysis/cspa.slog`
1. dowload dataset from google drive to root dir of slog. https://drive.google.com/file/d/1meoqulLRXMqLsb9S66NqUqP5mFUuDpzv/view?usp=sharing
2. unzip it
```sh
unzip ./linux.zip
```
3. build docker image, run the container and enter test container
```sh
docker build -t slog .
docker run -it --rm --shm-size=32g slog bash
docker exec -it slog bash
```
4. run test program
```sh
./runslog -v -R -j 8 -f ./linux ./analysis/cspa.slog out
```
5. in the generated REPL, use `relations` command to check size of computed relations

## Common issues on setup & how to solve
- Windows
       - <strong>WSL</strong>:
              - Docker has issues running on WSL if you are using a Linux Distribution running on WSL 1. To check your distribution version run
              - ``wsl -l -v``
              - If your main distribution (denoted by the asterisk) is version 1, upgrade it using 
              - ``wsl --set-version <distro name> 2``
              - Restart your terminal and try Docker again
