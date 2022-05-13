#include <stdio.h>
#include "dump.h"

int main(int argc, char **argv) {

    (void) argc;
    (void) argv;

    int empty;
    void *top;

    printf("Create some values to put in the dump\n");
    int x1=1, x2=2, x3=3, x4=4, x5=5, x6=6;
    printf("x1=%d, &x1=%p\n", x1, &x1);
    printf("x2=%d, &x2=%p\n", x2, &x2);
    printf("x3=%d, &x3=%p\n", x3, &x3);
    printf("x4=%d, &x4=%p\n", x4, &x4);
    printf("x5=%d, &x5=%p\n", x5, &x5);
    printf("x6=%d, &x6=%p\n", x6, &x6);
    printf("\n");

    printf("Create a new dump\n");
    printf("> dump<-dump_new()\n");
    dump_t dump = dump_new(NULL);
    printf("> dump=%p\n", dump);
    printf("\n");

    printf("Check if it is empty\n");
    printf("> empty<-dump_is_empty(%p)\n", dump);
    empty = dump_is_empty(dump);
    printf("> empty=%d, (current_size=%lu)\n", empty, dump->current_size);
    printf("\n");

    printf("Push some values\n");
    printf("> dump_push(%p, %p)\n", dump, &x1);
    dump_push(dump, &x1);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("> dump_push(%p, %p)\n", dump, &x2);
    dump_push(dump, &x2);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("> dump_push(%p, %p)\n", dump, &x3);
    dump_push(dump, &x3);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("\n");

    printf("Check if it is empty\n");
    printf("> empty<-dump_is_empty(%p)\n", dump);
    empty = dump_is_empty(dump);
    printf("> empty=%d, (current_size=%lu)\n", empty, dump->current_size);
    printf("\n");

    printf("Peek the top element\n");
    printf("> top<-dump_peek(%p)\n", dump);
    top = dump_peek(dump);
    printf("> top=%p, *top=%d\n", top, *((int *) top));
    printf("\n");

    printf("Pop two elements\n");
    printf("> dump_pop(%p)\n", dump);
    dump_pop(dump);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("> dump_pop(%p)\n", dump);
    dump_pop(dump);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("\n");

    printf("Peek the top element\n");
    printf("> top<-dump_peek(%p)\n", dump);
    top = dump_peek(dump);
    printf("> top=%p, *top=%d\n", top, *((int *) top));
    printf("\n");

    printf("> Freeze the current stack\n");
    printf("> dump_freeze(%p)\n", dump);
    dump_freeze(dump, NULL);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);

    printf("Check if it is empty\n");
    printf("> empty<-dump_is_empty(%p)\n", dump);
    empty = dump_is_empty(dump);
    printf("> empty=%d, (current_size=%lu)\n", empty, dump->current_size);

    printf("Push some more values\n");
    printf("> dump_push(%p, %p)\n", dump, &x4);
    dump_push(dump, &x4);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("> dump_push(%p, %p)\n", dump, &x5);
    dump_push(dump, &x5);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("\n");

    printf("Check if it is empty\n");
    printf("> empty<-dump_is_empty(%p)\n", dump);
    empty = dump_is_empty(dump);
    printf("> empty=%d, (current_size=%lu)\n", empty, dump->current_size);
    printf("\n");

    printf("Peek the top element\n");
    printf("> top<-dump_peek(%p)\n", dump);
    top = dump_peek(dump);
    printf("> top=%p, *top=%d\n", top, *((int *) top));
    printf("\n");

    printf("Restore the previous stack\n");
    printf("> dump_restore(%p)\n", dump);
    dump_restore(dump);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("\n");

    printf("Check if it is empty\n");
    printf("> empty<-dump_is_empty(%p)\n", dump);
    empty = dump_is_empty(dump);
    printf("> empty=%d, (current_size=%lu)\n", empty, dump->current_size);
    printf("\n");

    printf("Peek the top element\n");
    printf("> top<-dump_peek(%p)\n", dump);
    top = dump_peek(dump);
    printf("> top=%p, *top=%d\n", top, *((int *) top));
    printf("\n");

    printf("Push some more values\n");
    printf("> dump_push(%p, %p)\n", dump, &x4);
    dump_push(dump, &x4);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("> dump_push(%p, %p)\n", dump, &x3);
    dump_push(dump, &x3);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("\n");

    printf("> Freeze the current stack\n");
    printf("> dump_freeze(%p)\n", dump);
    dump_freeze(dump, NULL);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("\n");

    printf("Push some more values\n");
    printf("> dump_push(%p, %p)\n", dump, &x2);
    dump_push(dump, &x2);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("> dump_push(%p, %p)\n", dump, &x1);
    dump_push(dump, &x1);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("\n");

    printf("Dump the previous values\n");
    printf("> dump_overlay_previous(%p)\n", dump);
    dump_overlay_previous(dump);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("Print values in the stack\n");

    printf("Expect: %p Got: %p\n", &x1, dump_peek(dump));
    printf("> dump_pop(%p)\n", dump);
    dump_pop(dump);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("Expect: %p Got: %p\n", &x2, dump_peek(dump));
    printf("> dump_pop(%p)\n", dump);
    dump_pop(dump);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("Expect: %p Got: %p\n", &x3, dump_peek(dump));
    printf("> dump_pop(%p)\n", dump);
    dump_pop(dump);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("Expect: %p Got: %p\n", &x4, dump_peek(dump));
    printf("> dump_pop(%p)\n", dump);
    dump_pop(dump);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("Expect: %p Got: %p\n", &x1, dump_peek(dump));
    printf("> dump_pop(%p)\n", dump);
    dump_pop(dump);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("\n");

    printf("Push some more values\n");
    printf("> dump_push(%p, %p)\n", dump, &x4);
    dump_push(dump, &x4);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("> dump_push(%p, %p)\n", dump, &x3);
    dump_push(dump, &x3);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("\n");

    printf("> Freeze the current stack\n");
    printf("> dump_freeze(%p)\n", dump);
    dump_freeze(dump, NULL);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("\n");

    printf("Push some more values\n");
    printf("> dump_push(%p, %p)\n", dump, &x2);
    dump_push(dump, &x2);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("> dump_push(%p, %p)\n", dump, &x1);
    dump_push(dump, &x1);
    printf("> (%p)->current_size=%lu\n", dump, dump->current_size);
    printf("\n");

    printf("Destroy the dump\n");
    printf("> dump_destroy(%p)\n", dump);
    dump_destroy(dump);
    printf("\n");

    return 0;
}