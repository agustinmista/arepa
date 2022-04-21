#include <stdlib.h>

#include "mem.h"
#include "debug.h"


void *rts_malloc(size_t size) {
    void *ptr = malloc(size);
    debug_msg("allocated %zu bytes at %p", size, ptr);
    return ptr;
}

void rts_free(void *ptr) {
    debug_msg("freed pointer at %p", ptr);
    free(ptr);
}

void rts_memcpy(void *dest, const void *src, size_t size){
    debug_msg("copied %zu from %p to %p", src, dest, size);
    memcpy(dest,src,size);
}