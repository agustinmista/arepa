#include <stdio.h>

#include "io.h"
#include "debug.h"

void set_rts_stdout(FILE* stream) {
    debug_msg("Setting RTS stdout stream to %p", stream);
    rts_stdout = stream;
}

void new_rts_stdout(char* path) {
    debug_msg("Creating new RTS stdout file stream in %s", path);
    rts_stdout = fopen(path, "w");
}

void close_rts_stdout() {
    debug_msg("Closing the RTS stdout stream");
    fclose(rts_stdout);
}