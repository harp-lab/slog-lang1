#include <stdint.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <fcntl.h>
#include <unistd.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_set>
#include <queue>
#include <unordered_map>
#include <tuple>

int main(int argc, char **argv)
{
    int col_count=0;
    int global_row_count=0;

    char meta_data_filename[1024];
    sprintf(meta_data_filename, "%s.size", argv[1]);

    FILE *fp_in;
    fp_in = fopen(meta_data_filename, "r");
    if (fscanf (fp_in, "%d\n%d", &global_row_count, &col_count) != 2)
        printf("Wrong input format (Meta Data)\n");
    fclose(fp_in);


    std::cout << "Filename " << meta_data_filename << " Row Count " << global_row_count << " Column count " << col_count << std::endl;


    if (global_row_count == 0)
        return -1;

    char data_filename[1024];
    sprintf(data_filename, "%s", argv[1]);
    int fp = open(data_filename, O_RDONLY);


    unsigned long long* input_buffer = new unsigned long long[global_row_count * col_count];
    unsigned int rb_size = pread(fp, input_buffer, global_row_count * col_count * sizeof(unsigned long long), 0);
    if (rb_size != global_row_count * col_count * sizeof(unsigned long long))
        std::cout << "Wrong IO: rank: " << " " << rb_size << " " <<  global_row_count << " " << col_count << std::endl;

    close(fp);

    for (int u = 0; u < global_row_count * col_count; u = u+col_count)
    {
        for (int v = 0; v < col_count; v++)
            std::cout << input_buffer[u+v] << " " ;
        std::cout << std::endl;
    }
}
