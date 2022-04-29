#ifndef __PRIM_H__
#define __PRIM_H__

#include "value.h"

/* --------------------------------- */
/* Primitive operations              */
/* --------------------------------- */

/*
 * Do not modify the BEGIN and END comments below since they are used to mine
 * primitive operations by the compiler. Put the prototype of any new primitive
 * operation declaration between these two comments.
 */

// BEGIN PROTOTYPES
Int __prim_add_int__(Int, Int);
Int __prim_sub_int__(Int, Int);
Void __prim_print_int__(Int);
Double __prim_pi__();
// END PROTOTYPES

#endif