#ifndef __IO_H__
#define __IO_H__

#include <stdio.h>

/* --------------------------------- */
/* Redirecting stdout                */
/* --------------------------------- */

FILE* rts_stdout;

void set_rts_stdout(FILE* stream);

void new_rts_stdout(char* path);

void close_rts_stdout();

void rts_printf(const char* fmt, ...);

#endif