#ifndef __MEM_H__
#define __MEM_H__

#include <unistd.h>

// A drop-in replacement for malloc
void *rts_malloc(size_t size);

// A drop-in replacement for free
void rts_free(void *ptr);

void rts_memcpy(void *dest, const void *src, size_t size);

#endif