## Setting up Slog
The easiest and reliable way to get slog running is using Docker, the repo has a Docker file that sets up the container. To start;

- You need Docker installed and set-up on your system, Refer to [docker-docs](https://docs.docker.com/get-docker/)
- Once you have docker installed, get the repo from [harp-lab/slog](https://github.com/harp-lab/slog)
- Inside the repo directory, run  
```bash
docker build --tag=slog .
```
- Then start the container for the built image using
```bash
docker run --name=slog --detach slog
``` 
- To mount a local working directory into the container, change the command to 
```bash
docker run --name=slog -v <local_dir>:/workspace slog
```
- This starts the container and keeps it running the back ground.
- You can drop into the container shell by running, 
```bash
docker exec -it slog bash
```
-  In docker, the slog repo is at `/slog` , `./runslog` inside the folder lets you run slog.
- Jump to [A simple Example](./TC.md) to run `TC` using slog.
