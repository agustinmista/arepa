#ifndef __PRIM_H__
#define __PRIM_H__

/* --------------------------------- */
/* Primitive operations              */
/* --------------------------------- */

typedef long   IntV;
typedef double DoubleV;
typedef char   CharV;
typedef char*  StringV;

IntV __arepa_rts_add_int__(IntV x, IntV y);
IntV __arepa_rts_sub_int__(IntV x, IntV y);
DoubleV __arepa_rts_pi__();

#endif