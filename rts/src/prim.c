#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "prim.h"
#include "mem.h"
#include "io.h"


// Int arithmetic
Int __prim_add_int__(Int x, Int y) { return x + y; }
Int __prim_sub_int__(Int x, Int y) { return x - y; }
Int __prim_mul_int__(Int x, Int y) { return x * y; }
Int __prim_div_int__(Int x, Int y) { return x / y; }
Int __prim_mod_int__(Int x, Int y) { return x % y; }

// Double arithmetic
Double __prim_add_double__(Double x, Double y) { return x + y; }
Double __prim_sub_double__(Double x, Double y) { return x - y; }
Double __prim_mul_double__(Double x, Double y) { return x * y; }
Double __prim_div_double__(Double x, Double y) { return x / y; }

// String operations
String __prim_concat_string__(String x, String y) {
    int len_x = strlen(x);
    int len_y = strlen(y);
    int len_z = len_x + len_y + 1;
    String z = rts_malloc(len_z*sizeof(char));
    rts_memcpy(z, x, len_x);
    rts_memcpy(z+len_x, y, len_y+1);
    return z;
}

Int __prim_length_string__(String x) {
    return strlen(x);
}

String __prim_int_to_string__(Int x) {
    size_t needed = snprintf(NULL, 0, "%lu", x);
    String str = rts_malloc(needed);
    sprintf(str, "%lu", x);
    return str;
}

String __prim_double_to_string__(Double x) {
    size_t needed = snprintf(NULL, 0, "%f", x);
    String str = rts_malloc(needed);
    sprintf(str, "%f", x);
    return str;
}

String __prim_bool_to_string__(Bool x) {
    return (String) bool_str(x);
}

String __prim_unit_to_string__(Unit x) {
    return (String) unit_str;
}

// Int comparison operators
Bool __prim_eq_int__(Int x, Int y) { return x == y; }
Bool __prim_le_int__(Int x, Int y) { return x <= y; }
Bool __prim_lt_int__(Int x, Int y) { return x <  y; }

// Double comparison operators
Bool __prim_eq_double__(Double x, Double y) { return x == y; }
Bool __prim_le_double__(Double x, Double y) { return x <= y; }
Bool __prim_lt_double__(Double x, Double y) { return x <  y; }

// String comparison operators
Bool __prim_eq_string__(String x, String y) { return strcmp(x, y) == 0; }

// Constants
Double __prim_pi__() { return 3.141592653589793; };

// IO
String __prim_read_line__() {
    size_t buffer_len = 0;
    size_t buffer_step = 8;

    String buffer = rts_malloc(buffer_step);
    assert(buffer);
    buffer[buffer_len] = '\0';

    char c;
    while ((c = rts_getchar()) != '\n' && c != EOF) {
        buffer_len++;
        if (buffer_len % buffer_step == 0) {
            buffer = rts_realloc(buffer, buffer_len + buffer_step);
            assert(buffer);
        }
        buffer[buffer_len-1] = c;
        buffer[buffer_len]   = '\0';
    }
  return buffer;
}

String __prim_read_file__(String path) {
    FILE* f = fopen(path, "rb");
    assert(f);

    fseek(f, 0, SEEK_END);
    size_t length = ftell(f);
    fseek(f, 0, SEEK_SET);

    String data = rts_malloc(length);
    assert(data);
    size_t read = fread(data, 1, length, f);
    assert(read == length);

    fclose(f);
    return data;
}

Unit __prim_write_file__(String path, String data) {
    FILE *f = fopen(path, "ab");
    assert(f);
    fputs(data, f);
    fclose(f);
    return unit;
}

// Printing
Unit __prim_print_int__(Int x)       { rts_printf("%li\n", x);           return unit; }
Unit __prim_print_double__(Double x) { rts_printf("%f\n",  x);           return unit; }
Unit __prim_print_string__(String x) { rts_printf("%s\n",  x);           return unit; }
Unit __prim_print_bool__(Bool x)     { rts_printf("%s\n",  bool_str(x)); return unit; }
Unit __prim_print_unit__(Unit x)     { rts_printf("unit(%ld)\n", x);     return unit; }

// Tracing
Int    __prim_trace_int__(Int x)       { rts_printf("<%li>\n", x);           return x; }
Double __prim_trace_double__(Double x) { rts_printf("<%f>\n",  x);           return x; }
String __prim_trace_string__(String x) { rts_printf("<%s>\n",  x);           return x; }
Bool   __prim_trace_bool__(Bool x)     { rts_printf("<%s>\n",  bool_str(x)); return x; }
Unit   __prim_trace_unit__(Unit x)     { rts_printf("<unit(%ld)>\n", x);     return x; }