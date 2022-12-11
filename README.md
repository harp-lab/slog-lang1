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

## Using Docker

First, build the Docker image and start up the server:

```sh
docker build --tag slog .
docker run --name=slog --entrypoint=/slog/slog-server --rm --detach slog
```

You may want to run with `--network=host` to expose all the Slog ports to your
host.

### Using the REPL from inside the container

In a separate terminal:
```sh
docker exec --interactive --tty slog bash
./slog-repl
```

### Running scripts from outside the container

First, install the Python dependencies:

```sh
pip install -r requirements.txt
```

Then:

```sh
./runslog 127.0.0.1 tests/tc.slog
```

If you ran without `--network=host`, you can find the container's IP like so:

```sh
docker container inspect slog --format '{{.NetworkSettings.IPAddress}}'
```

## Another way to setup slog

Open Terminal -> Download slog repo using the git repo (https://github.com/harp-lab/slog.git)

Using Docker desktop application:

         1.    Install Docker desktopapplication using https://docs.docker.com/desktop/install/mac-install/
         
         2.    Place the downloadedfile In Applications folder. Double-click on the the downloaded .dmg file toget started.
         
         3.    Go to Terminal -> cd to slog directory
         
         # Create docker image

         docker build -t slog-daemon .

         docker run -d --name slog-server slog-daemon
         
         # keep two terminals open and change the current directory to the directory where slog is installed.

         One terminal to run the slog-server and another to run the slog-repl
          
         # To Up the slog-server

         docker exec -it slog-server bash

         ./slog-server
         
         # To go onto the REPL

         docker exec -it slog-server bash

         ./slog-repl

         4.   Work on the programs / slog-repl as required




         
