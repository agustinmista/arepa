#include "tim.h"

void plus();

void minus();

void call_arg_cont10();

void call_arg_cont11();

void call_arg_cont7();

void call_arg_cont8();

void call_arg_cont9();

void fun_arg0();

void fun_arg1();

void fun_arg2();

void fun_arg3();

void fun_arg4();

void fun_arg5();

void fun_arg6();

int main();

void mymain();

void print_int();

void twice();

void plus(){
  tim_take(2);
  tim_push_argument_label(*call_arg_cont9);
  tim_enter_argument(1);
}

void minus(){
  tim_take(2);
  tim_push_argument_label(*call_arg_cont11);
  tim_enter_argument(1);
}

void call_arg_cont10(){
  Int a = * ((Int*) tim_pop_value());
  Int b = * ((Int*) tim_pop_value());
  Int res = __prim_sub_int__(a,b);
  tim_push_value_int(res);
  tim_return();
}

void call_arg_cont11(){
  tim_push_argument_label(*call_arg_cont10);
  tim_enter_argument(0);
}

void call_arg_cont7(){
  Int a = * ((Int*) tim_pop_value());
  __prim_print_int__(a);
  tim_return();
}

void call_arg_cont8(){
  Int a = * ((Int*) tim_pop_value());
  Int b = * ((Int*) tim_pop_value());
  Int res = __prim_add_int__(a,b);
  tim_push_value_int(res);
  tim_return();
}

void call_arg_cont9(){
  tim_push_argument_label(*call_arg_cont8);
  tim_enter_argument(0);
}

void fun_arg0(){
  tim_push_argument_label(*fun_arg2);
  tim_push_argument_label(*fun_arg1);
  tim_enter_label(*twice);
}

void fun_arg1(){
  tim_push_argument_int(3);
  tim_enter_label(plus);
}

void fun_arg2(){
  tim_push_argument_int(5);
  tim_push_argument_int(30);
  tim_enter_label(*minus);
}

void fun_arg3(){
  tim_push_argument_label(*fun_arg5);
  tim_push_argument_label(*fun_arg4);
  tim_enter_label(*twice);
}

void fun_arg4(){
  tim_push_argument_argument(0);
  tim_enter_label(*plus);
}

void fun_arg5(){
  tim_push_argument_int(3);
  tim_push_argument_int(10);
  tim_enter_label(*minus);
}

void fun_arg6(){
  tim_push_argument_argument(1);
  tim_enter_argument(0);
}

int main(){
  tim_start();
  tim_take(0);
  tim_push_argument_label(*fun_arg0);
  tim_enter_label(*print_int);
}

void mymain(){
  tim_take(1);
  tim_push_argument_label(*fun_arg3);
  tim_enter_label(*print_int);
}

void print_int(){
  tim_take(1);
  tim_push_argument_label(*call_arg_cont7);
  tim_enter_argument(0);
}

void twice(){
  tim_take(2);
  tim_push_argument_label(*fun_arg6);
  tim_enter_argument(0);
}
