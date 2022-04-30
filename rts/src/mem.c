#include <stdlib.h>
#include <string.h>

#include "mem.h"
#include "debug.h"


void *rts_malloc(size_t size) {
    void *ptr = malloc(size);
    debug_msg("Allocated %zu bytes at %p", size, ptr);
    return ptr;
}

void rts_free(void *ptr) {
    debug_msg("Freed pointer at %p", ptr);
    free(ptr);
}

void* rts_realloc(void *ptr, size_t new_size) {
    void* new_ptr = realloc(ptr, new_size);
    debug_msg("Relocated pointer %p to %p with new size of %zu", ptr, new_ptr, new_size);
    return new_ptr;
};

void rts_memcpy(void *dest, const void *src, size_t size){
    debug_msg("Copied %zu bytes from %p to %p", size, src, dest);
    memcpy(dest,src,size);
}