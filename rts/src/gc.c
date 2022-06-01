#include <assert.h>
#include <stdlib.h>

#include "tim.h"
#include "mem.h"
#include "dump.h"
#include "gc.h"

#ifndef NO_GC

/*****************/
/* Mark handling */
/*****************/
Mark gc_mark=1;

void mark_refresh() {
  gc_mark = (gc_mark + 1) % 100;
  if (gc_mark <= 0) {gc_mark = 1;}
}

/**********************/
/* location recording */
/**********************/

gc_data data_locations;

void add_location(void* location) {
  gc_list new_location = rts_malloc(sizeof(struct gc_list));
  new_location->location = location;
  new_location->next = data_locations.locations;
  data_locations.locations = new_location;
  data_locations.size++;
}

/**********************/
/* Deallocation (free)*/
/**********************/

void free_location(void* ptr) {
  Mark* location = (Mark*) ptr;
  location--;
  rts_free(location);
}

/***********/
/* Marking */
/***********/

// Prototypes
void mark_frame(frame_t frame);

void mark_location(void* ptr) {
  if (ptr == NULL) { return; }
  Mark* location = (Mark*) ptr;
  location--;
  *location = gc_mark;
}

int is_marked_location(void* ptr) {
  Mark* location = (Mark*) ptr;
  if (location == NULL) { return 1; }
  location--;
  return *location == gc_mark;
}

int is_regular_closure(closure_t* closure) {
  return closure->code != *tim_value_code &&
         closure->code != *tim_nil_code;
}

void mark_frame_in_closure(closure_t* closure) {
  if (is_regular_closure(closure)) {
      mark_frame(closure->frame);
    } else {
      mark_location(closure->frame);
    }
}

void mark_frame(frame_t frame) {
  if (is_marked_location(frame)) { return; }
  mark_location(frame);
  mark_location(frame->arguments);
  for (long i = 0; i < frame->length; i++) {
    mark_frame_in_closure(&frame->arguments[i]);
  }
}

void mark_closure(closure_t* closure) {
  if (is_marked_location(closure)) { return; }
  mark_location(closure);
  mark_frame_in_closure(closure);
}

void mark_tim_metadata(tim_metadata_t metadata) {
  if (is_marked_location(metadata)) { return; }
  mark_location(metadata);
  mark_frame(metadata->frame);
}

void mark_closure_stack(long size,stack_t stack) {
  // Stack size correctness
  assert( 0 < size || stack == NULL);
  if (size <= 0) { return; }
  assert(stack);
  mark_closure(stack->data);
  mark_closure_stack(size-1,stack->next);
}

void mark_closure_dump(dump_t dump) {
  if (dump == NULL) { return; }
  assert(dump);
  stack_t current_stack = dump->current;
  long stack_size = dump->current_size;
  mark_closure_stack(stack_size,current_stack);
  mark_tim_metadata(dump->metadata);
  return mark_closure_dump(dump->parent);
}

void mark_argument_stack() {
  mark_closure_dump(argument_stack);
}

void mark_value_stack (long size, stack_t stack) {
  // Stack size correctness
  assert( 0 < size || stack == NULL);
  if (size <= 0) { return; }
  assert(stack);
  mark_location(stack->data);
  mark_value_stack(size-1,stack->next);
}

void mark_value_dump(dump_t dump) {
  if (dump == NULL) { return; }
  assert(dump);
  stack_t current_stack = dump->current;
  long stack_size = dump->current_size;
  mark_value_stack(stack_size,current_stack);
  return mark_value_dump(dump->parent);
}

void mark_current_value_stack() {
  mark_value_dump(value_stack);
}

void mark() {
  mark_frame(current_frame);
  mark_frame(current_data_frame);
  mark_current_value_stack();
  mark_argument_stack();
}

/************/
/* Sweeping */
/************/

int is_marked_list_location(gc_list locations) {
  return is_marked_location(locations->location);
}

void free_list_location(gc_list locations) {
  free_location(locations->location);
}

void sweep_locations(gc_list parent, gc_list location) {
  if (location == NULL) {return;}
  assert(location);

  if (is_marked_list_location(location)) {
    return sweep_locations(location,location->next);
  }

  free_list_location(location);
  parent->next = location->next;
  rts_free(location);
  return sweep_locations(parent,parent->next);
}

void sweep(){
  if (data_locations.size == 0) {return;}
  gc_list locations = data_locations.locations;
  sweep_locations(locations,locations->next);
  data_locations.size = 0;
}
#endif

/*****************/
/* Trigger code */
/****************/
void run_gc() {
  #ifndef NO_GC
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
  frame_t frame = gc_malloc(sizeof(struct frame_t));
  return frame;
}

closure_t* malloc_closure() {
  closure_t* closure = gc_malloc(sizeof(closure_t));
  return closure;
}

tim_metadata_t malloc_tim_metadata() {
  tim_metadata_t metadata = gc_malloc(sizeof(struct tim_metadata_t));
  return metadata;
}

void* gc_malloc(size_t size) {
  run_gc();
  #ifndef NO_GC
  Mark* pointer = rts_malloc(sizeof(Mark)+size);
  pointer++;
  add_location(pointer);
  #else
  void* pointer = rts_malloc(size);
  #endif
  return pointer;
}

void gc_init() {
  #ifndef NO_GC
  data_locations.size = 0;
  data_locations.locations = NULL;
  #endif
}
