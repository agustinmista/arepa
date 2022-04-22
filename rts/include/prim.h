#ifndef __PRIM_H__
#define __PRIM_H__

#include <stdio.h>

/* --------------------------------- */
/* Primitive operations              */
/* --------------------------------- */

typedef long   Int;
typedef double Double;
typedef char   Char;
typedef char*  String;
typedef void   Void;

/*
 * Do not modify the BEGIN and END comments below since they are used to mine
 * primitive operations by the compiler. Put the prototype of any new primitive
 * operation declaration between these two comments.
 */

// BEGIN PROTOTYPES
Int __arepa_rts_add_int__(Int, Int);
Int __arepa_rts_sub_int__(Int, Int);
Void __arepa_rts_print_int__(Int);
Double __arepa_rts_pi__();
// END PROTOTYPES

#endif