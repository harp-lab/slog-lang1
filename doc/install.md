# Install Slog
Installation of Slog is not special based on the machine/OS of use. Running basic Python, Racket, and C could suffice. There are two ways to install slog. 
1. [Docker](####Building-and-Running-the-Docker-Image)
2. [Terminal]()

## Installing using Docker
#### Pre-Reqs:
1. Install [Docker Desktop](https://www.docker.com/products/docker-desktop/)
2. Start Docker Desktop

#### Building and Running the Docker Image
```bash
1. git clone https://github.com/harp-lab/slog.git
2. cd slog
# Creating the docker image
3. docker build -t slog-daemon .
4. docker run -d --name slog-server slog-daemon
# Open two terminals
# First Terminal (to up the slog-server)
5. docker exec -it slog-server bash
6. ./slog-server
# Second Terminal (to up the repl)
7. docker exec -it slog-server bash
8. ./slog-repl
```

#### Common issues with Docker on setup & how to solve
  - Windows
	  - <strong>WSL</strong>:
		  - Docker has issues running on WSL if you are using a Linux Distribution running on WSL 1. To check your distribution version run
		  - ``wsl -l -v``
		  - If your main distribution (denoted by the asterisk) is version 1, upgrade it using 
		  - ``wsl --set-version <distro name> 2``
		  - Restart your terminal and try Docker again


## Installing without Docker
#### Pre-Reqs:
1. Install [Python](https://www.python.org/downloads/)
2. Install [Racket](https://racket-lang.org/)

#### Downloading Slog and installing dependencies
```bash
1. git clone https://github.com/harp-lab/slog.git
2. cd slog
# Feel free to create an environment here
3. pip install -r requirements.txt
4. raco pkg install graph pmap csv-reading rparrallel binaryio
```