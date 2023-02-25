FROM ubuntu:jammy

RUN apt-get update && apt-get install -y wget gnupg software-properties-common

RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN add-apt-repository ppa:plt/racket
RUN apt-get update && apt-get install -y clang-format clang-tidy clang-tools clang clangd libc++-dev libc++1 libc++abi-dev \
            libc++abi1 libclang-dev libclang1 liblldb-dev libomp-dev libomp5 lld lldb \
            llvm-dev llvm-runtime llvm python3-clang mcpp cmake racket build-essential openmpi-bin libopenmpi-dev z3 \
            git python3-pip sqlite3 ninja-build valgrind apt-utils libssl-dev vim valgrind apt-utils
RUN raco setup --doc-index --force-user-docs
RUN raco pkg install --batch --deps search-auto binaryio graph rparallel pmap csv-reading

ENV OMPI_ALLOW_RUN_AS_ROOT=1
ENV OMPI_MCA_btl_vader_single_copy_mechanism=none
ENV OMPI_ALLOW_RUN_AS_ROOT_CONFIRM=1
ENV OMPI_MCA_btl_vader_single_copy_mechanism=none
ENV CC=mpicc
ENV CXX=mpicxx

COPY . /slog

# build backend
RUN cd /slog/backend ; rm -rf build ; \ 
    cmake --no-warn-unused-cli -DCMAKE_EXPORT_COMPILE_COMMANDS:BOOL=TRUE -DCMAKE_BUILD_TYPE:STRING=RelWithDebInfo -DCMAKE_C_COMPILER:FILEPATH=/usr/bin/clang -H/slog/backend -B/slog/backend/build -G Ninja ; \
    cmake --build /slog/backend/build --config Release --target all -j --

WORKDIR /slog
RUN pip3 install -r requirements.txt
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3 100
EXPOSE 5108

# ENTRYPOINT ["tail", "-f", "/dev/null"]
