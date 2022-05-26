#include <assert.h>
#include <stdlib.h>

#include "tim.h"
#include "mem.h"
#include "dump.h"
#include "gc.h"

#ifdef GC

/*****************/
/* Mark handling */
/*****************/
int gc_mark=1;

void mark_refresh() {
  gc_mark = (gc_mark + 1) % 100;
  if (gc_mark <= 0) {gc_mark = 1;}
}

/**********************/
/* location recording */
/**********************/

gc_data data_locations;

void add_location(gc_data_t type, void* location) {
  gc_list new_location = rts_malloc(sizeof(struct gc_list));
  new_location->type = type;
  new_location->location = location;
  new_location->next = data_locations.locations;
  data_locations.locations = new_location;
  data_locations.size++;
}

void add_frame_location(frame_t frame) {
  add_location(FRAME,(void*) frame);
}

void add_closure_location(closure_t* closure) {
  add_location(CLOSURE,(void*) closure);
}

void add_metadata_location(tim_metadata_t metadata) {
  add_location(META,(void*) metadata);
}

/**********************/
/* Deallocation (free)*/
/**********************/

void free_value_ptr_as_frame(gc_closure_t type, frame_t frame) {
  switch (type)
  {
  case INT:
    rts_free((Int*) frame);
    break;
  case DOUBLE:
    rts_free((Double*) frame);
    break;
  case STRING:
    //TODO: differentiate between static strings and dynamically allocated ones
    rts_free((String*) frame);
    break;
  case BOOL:
    rts_free((Bool*) frame);
    break;
  case UNIT:
    rts_free((Unit*) frame);
    break;
  default:
    break;
  }
}

void free_closure(closure_t* closure) {
  gc_closure_t closure_type = closure->type;
  if (closure_type != REGULAR) {
    free_value_ptr_as_frame(closure_type,closure->frame);
  }
  rts_free(closure);
}

void free_frame(frame_t frame) {
  rts_free(frame);
}

void free_metadata(tim_metadata_t metadata) {
  rts_free(metadata);
}

/***********/
/* Marking */
/***********/

// Prototypes
void mark_closure(closure_t* closure);
void mark_frame(frame_t frame);

int is_marked_closure(closure_t* closure) {
  return closure->marked == gc_mark;
}

void set_mark_closure(closure_t* closure) {
  closure->marked=gc_mark;
}

void mark_closure(closure_t* closure) {
  if (is_marked_closure(closure)) {return;}
  set_mark_closure(closure);
  if (closure->type==REGULAR) {
    mark_frame(closure->frame);
  }
}

void set_mark_frame(frame_t frame) {
  frame->marked=gc_mark;
}

int is_marked_frame(frame_t frame) {
  return frame->marked == gc_mark;
}

void mark_frame(frame_t frame) {
  if (is_marked_frame(frame)) {return;}
  set_mark_frame(frame);
  for (long i = 0; i < frame->length; i++) {
    mark_closure(&frame->arguments[i]);
  }
}

int is_marked_tim_metadata(tim_metadata_t metadata) {
  return metadata->marked == gc_mark;
}

void set_mark_metadata(tim_metadata_t metadata) {
  metadata->marked=gc_mark;
}

void mark_tim_metadata(tim_metadata_t metadata) {
  if (is_marked_tim_metadata(metadata)) {return;}
  set_mark_metadata(metadata);
  mark_frame(metadata->frame);
}

void mark_closure_stack (long size,stack_t stack) {
  if (size <= 0) { return; }
  assert(stack);
  mark_closure(stack->data);
  mark_closure_stack(size-1,stack->next);
}

void mark_closure_dump(dump_t dump) {
  assert(dump);
  stack_t current_stack = dump->current;
  long stack_size = dump->current_size;
  mark_closure_stack(stack_size,current_stack);
  mark_tim_metadata(dump->metadata);
  dump_t rest_of_the_dump = dump->parent;
  if (rest_of_the_dump != NULL) {
    mark_closure_dump(rest_of_the_dump);
  }
}

void mark_argument_stack() {
  mark_closure_dump(argument_stack);
}

void mark() {
  mark_frame(current_frame);
  mark_frame(current_data_frame);
  mark_argument_stack();
}

/************/
/* Sweeping */
/************/

int is_marked_location(gc_list locations) {
  void* location = locations->location;
  switch (locations->type) {
    case CLOSURE:
      return is_marked_closure(location);
    case FRAME:
      return is_marked_frame(location);
    case META:
      return is_marked_tim_metadata(location);
    default:
      return 0;
  }
}

void swap_with_next_location(gc_list location) {
  assert(location);
  gc_list next = location->next;
  *location = *next;
  rts_free(next);
}

void free_location(gc_list locations) {
  void* location = locations->location;
  switch (locations->type) {
    case FRAME:
      return free_frame(location);
    case CLOSURE:
      return free_closure(location);
    case META:
      return free_metadata(location);
    default:
      return;
  }
}

long sweep_locations(gc_list location, long size) {
  if (location == NULL) {return size;}
  assert(location);

  if (is_marked_location(location)) {
    size--;
    free_location(location);
    swap_with_next_location(location);
    return sweep_locations(location,size);
  }

  return sweep_locations(location->next,size);
}

void sweep(){
  if (data_locations.size == 0) {return;}
  long size = data_locations.size;
  gc_list locations = data_locations.locations;
  data_locations.size = sweep_locations(locations,size);
}
#endif
/*****************/
/* Trigger code */
/****************/
void run_gc() {
  #ifdef GC
  if (data_locations.size <= 100) {return;}
  mark();
  sweep();
  mark_refresh();
  #endif
}



/***********************/
/* Allocation wrappers */
/***********************/

frame_t malloc_frame() {
  run_gc();
  frame_t frame = rts_malloc(sizeof(struct frame_t));
  #ifdef GC
  add_frame_location(frame);
  #endif
  return frame;
}

closure_t* malloc_closure() {
  run_gc();
  closure_t* closure = rts_malloc(sizeof(closure_t));
  #ifdef GC
  add_closure_location(closure);
  #endif
  return closure;
}

tim_metadata_t malloc_tim_metadata() {
  run_gc();
  tim_metadata_t metadata = rts_malloc(sizeof(struct tim_metadata_t));
  #ifdef GC
  add_metadata_location(metadata);
  #endif
  return metadata;
}

closure_t* malloc_closure_array(long size){
  run_gc();
  closure_t* closure_array = rts_malloc(size*sizeof(closure_t));
  #ifdef GC
  for (long i = 0; i < size; i++){
    add_closure_location(&closure_array[i]);
  }
  #endif
  return closure_array;
}

void gc_init() {
  #ifdef GC
  data_locations.size = 0;
  data_locations.locations = NULL;
  #endif
}
