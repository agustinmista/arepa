#ifndef __GC_H__
#define __GC_H__

#include "tim.h"

#ifndef NO_GC

typedef int Mark;

typedef struct gc_list {
  void* location;
  struct gc_list* next;
} *gc_list;

typedef struct gc_data {
  long size;
  gc_list locations;
} gc_data;

#define GC_THRESHOLD  100
#define GC_MARK_BOUND 100

#endif

// Startup
void gc_init();

// GC call
void gc();

// Allocation
void* gc_malloc(size_t size);

#endif