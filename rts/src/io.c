#include <stdio.h>
#include <stdarg.h>

#include "io.h"
#include "debug.h"

FILE* rts_stdin;
FILE* rts_stdout;

void set_rts_stdin(FILE* stream) {
    debug_msg("Setting RTS stdin stream to %p", stream);
    rts_stdin = stream;
}

void set_rts_stdout(FILE* stream) {
    debug_msg("Setting RTS stdout stream to %p", stream);
    rts_stdout = stream;
}

void open_rts_stdin(char* path) {
    debug_msg("Opening file %s as stdin", path);
    rts_stdin = fopen(path, "r");
}

void open_rts_stdout(char* path) {
    debug_msg("Opening file %s as stdout", path);
    rts_stdout = fopen(path, "w");
}

void close_rts_stdin() {
    debug_msg("Closing the RTS stdin stream");
    fclose(rts_stdin);
}

void close_rts_stdout() {
    debug_msg("Closing the RTS stdout stream");
    fclose(rts_stdout);
}

void rts_printf(const char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vfprintf(rts_stdout, fmt, args);
    va_end(args);
}

char rts_getchar() {
    return getc(rts_stdin);
}