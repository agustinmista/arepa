#include <assert.h>
#include <stdlib.h>

#include "tim.h"
#include "mem.h"
#include "gc.h"

#ifdef GC
gc_data data_locations;
#endif

void gc_startup() {
  #ifdef GC
  data_locations.size = 0;
  data_locations.locations = rts_malloc(sizeof(struct gc_list));
  #endif
}

#ifdef GC
void add_location(gc_data_t type, void* location) {
  gc_list new_location = rts_malloc(sizeof(struct gc_list));
  new_location->type = type;
  new_location->location = location;
  new_location->next = data_locations.locations;
  data_locations.locations = new_location;
}

void add_frame_location(frame_t frame) {
  add_location(FRAME,(void*) frame);
}

void add_closure_location(closure_t* closure) {
  add_location(CLOSURE,(void*) closure);
}
#endif

frame_t malloc_frame() {
  frame_t frame = rts_malloc(sizeof(struct frame_t));
  #ifdef GC
  add_frame_location(frame);
  #endif
  return frame;
}

closure_t* malloc_closure() {
  closure_t* closure = rts_malloc(sizeof(closure_t));
  #ifdef GC
  add_closure_location(closure);
  #endif
  return closure;
}

closure_t* malloc_closure_array(long size){
  closure_t* closure_array = rts_malloc(size*sizeof(closure_t));
  #ifdef GC
  for (long i = 0; i < size; i++){
    add_closure_location(&closure_array[i]);
  }
  #endif
  return closure_array;
}