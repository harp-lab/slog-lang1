FROM ubuntu:latest

RUN apt-get update && apt-get install -y wget gnupg software-properties-common

RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN add-apt-repository ppa:plt/racket
RUN apt-get update -y
RUN apt-get install -y clang-format clang-tidy clang-tools clang clangd libc++-dev libc++1 libc++abi-dev \
            libc++abi1 libclang-dev libclang1 liblldb-dev libomp-dev libomp5 lld lldb \
            llvm-dev llvm-runtime llvm python-clang mcpp cmake racket build-essential mpich z3 \
            git python3-pip sqlite3

RUN raco setup --doc-index --force-user-docs
RUN raco pkg install --batch --deps search-auto binaryio graph

ENV OMPI_ALLOW_RUN_AS_ROOT=1
ENV OMPI_ALLOW_RUN_AS_ROOT_CONFIRM=1

COPY . /slog

# build backend
RUN rm -rf /slog/backend/build
RUN cd /slog/backend && cmake -Bbuild .
RUN cd /slog/backend/build && make -j8

WORKDIR /slog
RUN pip3 install -r requirements.txt
EXPOSE 5108

ENV CC=mpicc
ENV CXX=mpicxx
# ENTRYPOINT [ "./slog-server" ]
