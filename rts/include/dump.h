#ifndef __DUMP_H__
#define __DUMP_H__

/* --------------------------------- */ 
/* Dumps (stacks of stacks)          */  
/* --------------------------------- */ 

typedef struct stack_t {
    struct stack_t *next;
    void *data;
} *stack_t;

typedef struct dump_t {    
    stack_t current;
    struct dump_t *parent;
} *dump_t;

// Create a new dump
dump_t dump_new();

// Destroy a dump
void dump_destroy(dump_t dump);

// Is the current stack empty?
int dump_is_empty(dump_t dump);

// Push an element to the dump (on top of the current stack)
void dump_push(dump_t dump, void *data); 

// Pop (and free) an element from the current stack
void dump_pop(dump_t dump);

// Peek the top element on the current stack (without freeing it)
void *dump_peek(dump_t dump);

// Create a new stack on top of a given one
void dump_freeze(dump_t dump);

// Restore the previous stack, freeing everything in the current one
void dump_restore(dump_t dump);

#endif