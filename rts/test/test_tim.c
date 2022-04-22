#include <stdio.h>
#include "tim.h"

void i(){
  tim_take(1);
  tim_enter_argument(0);
}

int main(){
  tim_start();
  tim_take(0);
  tim_push_literal_int(5);
  tim_enter_label(*i);
  printf("result %i", get_result());
}