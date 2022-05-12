#ifndef __TIM_H__
#define __TIM_H__

#include "value.h"
#include "prim.h"

struct closure_t;

typedef struct frame_t {
  long length;
  int is_partial;
  struct closure_t* arguments;
} *frame_t;

typedef struct closure_t {
  frame_t frame;
  void (*code)();
} closure_t;

typedef struct tim_metadata_t {
  int offset;
  frame_t frame;
} *tim_metadata_t;

void tim_start();

void tim_take(long total, long n);

void tim_push_argument_argument(long offset);

void tim_push_argument_int(Int value);

void tim_push_argument_double(Double value);

void tim_push_argument_string(String value);

void tim_push_argument_label(void (*code)());

void tim_push_value_int(Int value);

void tim_push_value_double(Double value);

void tim_push_value_string(String value);

Int* tim_pop_value_int();

Double* tim_pop_value_double();

String* tim_pop_value_string();

void tim_enter_argument(long argument);

void tim_enter_int(Int value);

void tim_enter_double(Double value);

void tim_enter_string(String value);

void tim_enter_label(void (*code)());

void tim_move_argument(long offset, long argument);

void tim_move_int(long offset, Int value);

void tim_move_double(long offset, Double value);

void tim_move_string(long offset, String value);

void tim_move_label(long offset, void (*code)());

void tim_marker_push(long offset);

void tim_markers_update(long nargs);

void tim_return();

Int get_int_result();

Double get_double_result();

String get_string_result();

#endif
