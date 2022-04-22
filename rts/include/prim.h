#ifndef __PRIM_H__
#define __PRIM_H__

#include <stdio.h>

/* --------------------------------- */
/* Primitive operations              */
/* --------------------------------- */

typedef long   IntV;
typedef double DoubleV;
typedef char   CharV;
typedef char*  StringV;
typedef void   VoidV;

IntV __arepa_rts_add_int__(IntV x, IntV y);
IntV __arepa_rts_sub_int__(IntV x, IntV y);
VoidV __arepa_rts_print_int__(IntV x);
DoubleV __arepa_rts_pi__();

#endif