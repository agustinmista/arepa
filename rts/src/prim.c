#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
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

Void __prim_write_file__(String path, String data) {
    FILE *f = fopen(path, "ab");
    assert(f);
    fputs(data, f);
    fclose(f);
}

// Printing
Void __prim_print_int__(Int x)       { rts_printf("%ld\n", x); }
Void __prim_print_double__(Double x) { rts_printf("%f\n",  x); }
Void __prim_print_string__(String x) { rts_printf("%s\n",  x); }

// Tracing
Int    __prim_trace_int__(Int x)       { rts_printf("<%ld>\n", x); return x; }
Double __prim_trace_double__(Double x) { rts_printf("<%f>\n",  x); return x; }
String __prim_trace_string__(String x) { rts_printf("<%s>\n",  x); return x; }