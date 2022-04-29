#include <stdio.h>
#include "prim.h"

Int __prim_add_int__(Int x, Int y) { return x + y; }
Int __prim_sub_int__(Int x, Int y) { return x - y; }
Void __prim_print_int__(Int x) { printf("%ld\n", x); };
Double __prim_pi__() { return 3.141592653589793; };
