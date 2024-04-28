### Slog: Data-Parallel Deductive Programming

![CI](https://github.com/harp-lab/slog-lang/workflows/CI/badge.svg)

Slog is a language, compiler, and runtime system for implementing
data-parallel deductive programming.

## Components

- `compiler/`         -- Compiler from Slog source code to a parallel RA plan
- `backend/`          -- MPI-based parallel relational algebra plan (RA) runtime
- `runslog`           -- Script to build and run slog program with given input data

## Building and Executing

### Prepare
- input slog program file: "<filename>.slog"
- an input database dir "<in_dir_name>", all relation stored as "<dir_name>/<rel_name>.csv"
- an output database dir "<out_dir_name>"

### Build From Scratch
Please check the command used in Dockerfile.


### Using Docker
```sh
docker build -t <image_name> .
# mount current dir  into workspace, please make sure input facts and slog program
# are presented.
docker run --rm -it -v $(pwd):/workspace stargazermiao/slog bash
# please specify the core number
./runslog -v -R -j <cores> -f <in_dir_name>  <filename>.slog <out_dir_name>
```


## Another way to setup slog

Open Terminal -> Download slog repo using the git repo (https://github.com/harp-lab/slog.git)

Using Docker desktop application:

         1.    Install Docker desktop application using https://docs.docker.com/desktop/
         
         2.    Place the downloaded file in Applications folder(if on mac). Double-click on the the downloaded .dmg file to get started.
         
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

  ## Common issues on setup & how to solve
  - Windows
	  - <strong>WSL</strong>:
		  - Docker has issues running on WSL if you are using a Linux Distribution running on WSL 1. To check your distribution version run
		  - ``wsl -l -v``
		  - If your main distribution (denoted by the asterisk) is version 1, upgrade it using 
		  - ``wsl --set-version <distro name> 2``
		  - Restart your terminal and try Docker again


         
