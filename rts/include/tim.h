#ifndef __TIM_H__
#define __TIM_H__

#include "value.h"
#include "prim.h"

struct closure_t;

typedef struct frame_t {
  long length;
  struct closure_t* arguments;
} *frame_t;

typedef struct closure_t {
  frame_t frame;
  void (*code)();
} closure_t;

void tim_start();

void tim_take(long range);

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

void tim_return();

Int get_int_result();

Double get_double_result();

String get_string_result();

#endif
