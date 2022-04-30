#include <stdio.h>
#include "prim.h"


// Int arithmetic
Int __prim_add_int__(Int x, Int y) { return x + y; }
Int __prim_sub_int__(Int x, Int y) { return x - y; }

// Double arithmetic
Double __prim_add_double__(Double x, Double y) { return x + y; }
Double __prim_sub_double__(Double x, Double y) { return x - y; }

// Constants
Double __prim_pi__() { return 3.141592653589793; };

// IO
Void __prim_print_int__(Int x)       { printf("%ld\n", x); };
Void __prim_print_double__(Double x) { printf("%f\n", x);  };
Void __prim_print_string__(String x) { printf("%s\n", x);  };
