#ifndef __VALUE_H__
#define __VALUE_H__

/* --------------------------------- */
/* Primitive value types             */
/* --------------------------------- */

typedef long   Int;
typedef double Double;
typedef char*  String;
typedef long   Bool;
typedef long   Unit;

String bool_str(Bool b);

extern Unit unit;

#endif