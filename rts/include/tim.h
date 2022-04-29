#ifndef __TIM_H__
#define __TIM_H__

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

void tim_take(Int range);

void tim_push_argument_argument(Int offset);

void tim_push_argument_int(Int literal);

void tim_push_argument_double(Double literal);

void tim_push_argument_label(void (*code)());

void tim_push_value_int(Int literal);

void tim_push_value_double(Double literal);

void * tim_pop_value();

void tim_enter_argument(Int argument);

void tim_enter_int(Int literal);

void tim_enter_double(Double literal);

void tim_enter_label(void (*code)());

void tim_int_code();

void tim_double_code();

void tim_return();

Int get_int_result();

Double get_double_result();

#endif
