#ifndef __TIM_H__
#define __TIM_H__

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

void tim_push_argument(long argument);

void tim_push_literal_long(long literal);

void tim_push_label(void (*code)());

void tim_enter_argument(long argument);

void tim_enter_literal(int literal);

void tim_enter_label(void (*code)());

void tim_long_code();

void tim_float_code();

#endif
