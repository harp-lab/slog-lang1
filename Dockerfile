FROM ubuntu:latest

RUN apt-get update && apt-get install -y wget gnupg software-properties-common

RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN add-apt-repository ppa:plt/racket
RUN apt-get update -y && apt-get install -y clang-format clang-tidy clang-tools clang clangd libc++-dev libc++1 libc++abi-dev \
            libc++abi1 libclang-dev libclang1 liblldb-dev libomp-dev libomp5 lld lldb \
            llvm-dev llvm-runtime llvm python-clang mcpp cmake racket build-essential mpich z3 \
            git python3-pip sqlite3 ninja-build valgrind apt-utils
RUN raco setup --doc-index --force-user-docs
RUN raco pkg install --batch --deps search-auto binaryio graph

ENV OMPI_ALLOW_RUN_AS_ROOT=1
ENV OMPI_ALLOW_RUN_AS_ROOT_CONFIRM=1
ENV CC=mpicc
ENV CXX=mpicxx

COPY . /slog

# build backend
RUN cd /slog/backend ; rm -rf build ; \ 
    cmake --no-warn-unused-cli -DCMAKE_EXPORT_COMPILE_COMMANDS:BOOL=TRUE -DCMAKE_BUILD_TYPE:STRING=Release -DCMAKE_C_COMPILER:FILEPATH=/usr/bin/mpicc -H/slog/backend -B/slog/backend/build -G Ninja ; \
    cmake --build /slog/backend/build --config Release --target all -j --

WORKDIR /slog
RUN pip3 install -r requirements.txt
EXPOSE 5108

ENTRYPOINT ["tail", "-f", "/dev/null"]
