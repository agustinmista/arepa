#include <stdio.h>
#include <assert.h>
#include "tim.h"

void k(){
  tim_take(2);
  tim_enter_argument(0);
}

int main(){
  tim_start();
  tim_take(0);
  tim_push_argument_int(0);
  tim_push_argument_int(5);
  tim_enter_label(*k);

  int result = get_int_result();
  printf("The result is: %i\n", result);
  assert(result == 5);
}