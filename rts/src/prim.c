#include "prim.h"

IntV __arepa_rts_add_int__(IntV x, IntV y) { return x + y; }
IntV __arepa_rts_sub_int__(IntV x, IntV y) { return x - y; }
VoidV __arepa_rts_print_int__(IntV x) { printf("%ld\n", x); };
DoubleV __arepa_rts_pi__() { return 3.141592653589793; };
