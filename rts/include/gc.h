#ifndef __GC_H__
#define __GC_H__

#include "tim.h"

#ifdef GC

typedef enum gc_data_t {
  CLOSURE,
  FRAME
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
void gc_startup();

// GC types for closures
#ifdef GC

#define set_closure_gc_int(closure)    closure->type = INT;
#define set_closure_gc_double(closure) closure->type = DOUBLE;
#define set_closure_gc_string(closure) closure->type = STRING;
#define set_closure_gc_bool(closure)   closure->type = BOOL;
#define set_closure_gc_unit(closure)   closure->type = UNIT;
#define set_closure_gc_none(closure)   closure->type = NONE;
#define copy_closure_type(a,b)         a->type = b->type;

#else

#define set_closure_gc_int(closure)     ;
#define set_closure_gc_double(closure)  ;
#define set_closure_gc_string(closure)  ;
#define set_closure_gc_bool(closure)    ;
#define set_closure_gc_unit(closure)    ;
#define set_closure_gc_none(closure)    ;
#define copy_closure_type(closure,type) ;

#endif

// Allocation
closure_t* malloc_closure();
frame_t    malloc_frame();
closure_t* malloc_closure_array(long size);

#endif