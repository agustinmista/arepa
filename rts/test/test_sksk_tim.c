#include <stdio.h>
#include <assert.h>
#include "tim.h"

void i(){
  tim_take(1, 1);
  tim_enter_argument(0);
}

void k(){
  tim_take(2, 2);
  tim_enter_argument(0);
}

void fun0(){
  tim_push_argument_argument(2);
  tim_enter_argument(1);
}

void s(){
  tim_take(3, 3);
  tim_push_argument_label(*fun0);
  tim_push_argument_argument(2);
  tim_enter_argument(0);
}

int main(){
  tim_start();
  tim_take(0, 0);
  tim_push_argument_int(0);
  tim_push_argument_int(5);
  tim_push_argument_label(*k);
  tim_push_argument_label(*s);
  tim_push_argument_label(*k);
  tim_enter_label(*s);

  int result = get_int_result();
  printf("The result is: %i\n", result);
  assert(result == 5);
}