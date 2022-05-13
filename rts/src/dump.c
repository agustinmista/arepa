#include <assert.h>

#include "mem.h"
#include "dump.h"

/* --------------------------------- */
/* Dumps (stacks of stacks)          */
/* --------------------------------- */

dump_t dump_new(void* metadata) {
    // Allocate memory for a new dump
    dump_t dump = rts_malloc(sizeof(struct dump_t));

    // Initialize the fields
    dump->current = NULL;
    dump->parent = NULL;
    dump->current_size = 0;
    dump->metadata = metadata;

    return dump;
}

void dump_destroy(dump_t dump) {
    // Sanity checks
    assert(dump);

    // The next dump to be destroyed
    dump_t current = dump;

    // Start destroying stuff
    do {

        // Free the stack nodes in the current dump
        while (!dump_is_empty(current)) {
            dump_pop(current);
        }

        // Save the current dump for later
        dump_t tmp = current;

        // Set the parent dump to be destroyed next
        current = current->parent;

        // Free the current dump struct and move on
        rts_free(tmp);

    } while (current);

}

int dump_is_empty(dump_t dump) {
    // Sanity checks
    assert(dump);

    return (dump->current == NULL);
}

void dump_push(dump_t dump, void* data) {
    // Sanity checks
    assert(dump);

    // Allocate memory for a new stack node
    stack_t node = rts_malloc(sizeof(struct stack_t));
    assert(node);

    // Initialize the new node
    node->next = dump->current;
    node->data = data;

    // Set the new node a the top of the current stack and increase size
    dump->current = node;
    dump->current_size++;
}

void* dump_pop(dump_t dump) {
    // Sanity checks
    assert(dump);
    assert(dump->current);

    // Link the current stack to the next element in line and decrease size
    stack_t node = dump->current;
    dump->current = dump->current->next;
    dump->current_size--;

    // Save the popped data for later
    void* data = node->data;

    // Delete the unlinked stack node
    rts_free(node);

    return data;
}

void* dump_peek(dump_t dump) {
    // Sanity checks
    assert(dump);
    assert(dump->current);

    return dump->current->data;
}

void* stack_peek(stack_t stack) {
    // Sanity checks
    assert(stack);
    return stack->data;
}

void dump_freeze(dump_t dump, void* metadata) {
    // Sanity checks
    assert(dump);

    // Allocate memory for a new dump
    dump_t new_dump = rts_malloc(sizeof(struct dump_t));
    assert(new_dump);

    // Copy the current dump onto the new one
    new_dump->current      = dump->current;
    new_dump->parent       = dump->parent;
    new_dump->current_size = dump->current_size;
    new_dump->metadata     = metadata;

    // Rewire the current dump to be the new one
    dump->current = NULL;
    dump->parent = new_dump;
    dump->current_size = 0;
    dump->metadata = NULL;
}

void dump_restore_parent(dump_t dump) {
    // Sanity checks
    assert(dump);
    assert(dump->parent);

    // Save the parent dump for later
    dump_t parent = dump->parent;

    // Restore the parent dump onto the current one
    dump->current      = parent->current;
    dump->parent       = parent->parent;
    dump->current_size = parent->current_size;
    dump->metadata     = parent->metadata;

    // Free the (now old) parent dump
    rts_free(parent);
}

void dump_append(dump_t a, dump_t b){
    assert(a);
    assert(b);

    if (dump_is_empty(a)) return;

    void* data = dump_peek(a);
    dump_pop(a);
    dump_append(a, b);
    dump_push(b, data);
}

void dump_overlay_previous(dump_t dump) {
    // Sanity checks
    assert(dump);
    assert(dump->parent);

    // Append the current stack on top of the previous one
    dump_append(dump, dump->parent);
    assert(dump_is_empty(dump));

    // Restore parent
    dump_restore_parent(dump);
}

void dump_restore(dump_t dump) {
    // Sanity checks
    assert(dump);
    assert(dump->parent);

    // Free nodes in the current stack
    while (!dump_is_empty(dump)) {
        dump_pop(dump);
    }

    // Restore parent
    dump_restore_parent(dump);
}
