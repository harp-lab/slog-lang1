/*
 * Buffer struct (similar to std::vector)
 * Copyright (c) Sidharth Kumar, et al, see License.md
 */

#pragma once


/* A resizeable buffer, similar to std::vector */
struct vector_buffer
{
  unsigned char *buffer=NULL;
  uint64_t size=0;
  uint64_t capacity=0;

  vector_buffer();
  void vector_buffer_create_empty();
  void vector_buffer_create_with_capacity(uint64_t capacity);
  void vector_buffer_free(void);
  void vector_buffer_append(const unsigned char *data, const uint64_t size);
  void vector_buffer_resize(const uint64_t size);
};
