/*
 * Buffer struct (similar to std::vector)
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */


#include "../parallel_RA_inc.h"

#define Max2ab(a,b)      (((a)> (b))?(a):(b))

vector_buffer::vector_buffer()
{
    buffer = NULL;
    size = 0;
    capacity = 0;
}



void vector_buffer::vector_buffer_create_empty()
{
    buffer = NULL;
    size = 0;
    capacity = 0;
}



void vector_buffer::vector_buffer_create_with_capacity(uint64_t cap)
{
    buffer = (unsigned char*)malloc(capacity);
    size = 0;
    capacity = cap;
}



void vector_buffer::vector_buffer_free()
{
    free(buffer);
    size = 0;
    capacity = 0;
}



void vector_buffer::vector_buffer_append(const unsigned char *data, const uint64_t si)
{
    if (capacity - size < si)
    {
        capacity = Max2ab(capacity + si, capacity * 1.5);
        buffer = (unsigned char*)realloc(buffer, capacity);
    }
    memcpy(buffer + size, data, si);
    size += si;
}



void vector_buffer::vector_buffer_resize(const uint64_t si)
{
    if (capacity < si)
    {
        capacity = si;
        buffer = (unsigned char*)realloc(buffer, capacity);
    }
    size = size;
}
