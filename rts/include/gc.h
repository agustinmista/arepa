#ifndef __GC_H__
#define __GC_H__

#include "tim.h"

#ifdef GC

typedef enum gc_data_t {
  CLOSURE,
  FRAME,
  META,
  END
} gc_data_t;

typedef struct gc_list {
  gc_data_t type;
  void* location;
  struct gc_list* next;
} *gc_list;

typedef struct gc_data {
  long size;
  gc_list locations;
} gc_data;

#endif

// Startup
void gc_init();

// GC types for closures
#ifdef GC

#define set_closure_gc_value(closure)   closure->type = VALUE;
#define set_closure_gc_regular(closure) closure->type = REGULAR;
#define set_closure_gc_nil(closure)     closure->type = NIL;
#define copy_closure_type(a,b)          b->type = a->type;

#else

#define set_closure_gc_value(closure) ;
#define set_closure_gc_regular(closure) ;
#define set_closure_gc_nil(closure)    ;
#define copy_closure_type(closure,type) ;

#endif

// Allocation
closure_t*     malloc_closure();
frame_t        malloc_frame();
tim_metadata_t malloc_tim_metadata();

#endif