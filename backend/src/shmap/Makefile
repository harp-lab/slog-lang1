
all:
	mpicxx -std=c++14 -I/usr/lib/x86_64-linux-gnu/openmpi/include/openmpi -I/usr/lib/x86_64-linux-gnu/openmpi/include -pthread -L/usr/lib/x86_64-linux-gnu/openmpi/lib -lmpi -g -O3 -o test test.cpp

clean:
	rm ./test *.o
