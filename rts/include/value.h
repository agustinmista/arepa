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

extern Bool true;
extern Bool false;
String bool_str(Bool b);

extern Unit   unit;
extern String unit_str;

#endif