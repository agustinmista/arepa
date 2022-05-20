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

// Int arithmetic
Int __prim_add_int__(Int, Int);
Int __prim_sub_int__(Int, Int);
Int __prim_mul_int__(Int, Int);
Int __prim_div_int__(Int, Int);
Int __prim_mod_int__(Int, Int);

// Double arithmetic
Double __prim_add_double__(Double, Double);
Double __prim_sub_double__(Double, Double);
Double __prim_mul_double__(Double, Double);
Double __prim_div_double__(Double, Double);

// String operations
String __prim_concat_string__(String, String);
Int __prim_length_string__(String);
String __prim_int_to_string__(Int);
String __prim_double_to_string__(Double);
String __prim_bool_to_string__(Bool);

// Int comparison operators
Bool __prim_eq_int__(Int, Int);
Bool __prim_le_int__(Int, Int);
Bool __prim_lt_int__(Int, Int);

// Double comparison operators
Bool __prim_eq_double__(Double, Double);
Bool __prim_le_double__(Double, Double);
Bool __prim_lt_double__(Double, Double);

// String comparison operators
Bool __prim_eq_string__(String, String);

// Constants
Double __prim_pi__();

// IO
String __prim_read_line__();
String __prim_read_file__(String);
Unit __prim_write_file__(String, String);

// Printing
Unit __prim_print_int__(Int);
Unit __prim_print_double__(Double);
Unit __prim_print_string__(String);
Unit __prim_print_bool__(Bool);
Unit __prim_print_unit__(Unit);

// Debugging
Int    __prim_trace_int__(Int);
Double __prim_trace_double__(Double);
String __prim_trace_string__(String);
Bool   __prim_trace_bool__(Bool);
Unit   __prim_trace_unit__(Unit);

// END PROTOTYPES

#endif