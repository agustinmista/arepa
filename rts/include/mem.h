#ifndef __MEM_H__
#define __MEM_H__

#include <unistd.h>

// A drop-in replacement for malloc
void *rts_malloc(size_t size);

// A drop-in replacement for free
void rts_free(void *ptr);

#endif