#ifndef __IO_H__
#define __IO_H__

#include <stdio.h>

/* --------------------------------- */
/* Redirecting stdout                */
/* --------------------------------- */

void set_rts_stdout(FILE* stream);

void set_rts_stdin(FILE* stream);

void open_rts_stdin(char* path);

void open_rts_stdout(char* path);

void close_rts_stdin();

void close_rts_stdout();

void rts_printf(const char* fmt, ...);

char rts_getchar();

#endif