#include <stdlib.h>
#include <assert.h>

/* --------------------------------- */ 
/* Stacks as linked lists            */  
/* --------------------------------- */ 

typedef struct stack_node_t {
    void                *data;
    struct stack_node_t *next;
} stack_node_t;

/* --------------------------------- */ 
/* Nested stacks                     */  
/* --------------------------------- */ 

typedef struct stack_t {    
    struct stack_t *parent;
    stack_node_t   *current;
} stack_t;

// Empty check
int stack_is_empty(stack_t *stack) {
    assert(stack);
    return (stack->current == NULL); 
}

// Push an element on top of the current stack
void stack_push(stack_t *stack, void *data) {
    assert(stack);
    stack_node_t *node = malloc(sizeof(stack_node_t));
    assert(node);
    node->data = data;
    node->next = stack->current->next;
    stack->current = node;
} 

// Pop an element from the current stack
void *stack_pop(stack_t *stack) {
    assert(stack);
    stack_node_t *node = stack->current;
    stack->current = stack->current->next;
    free(node);
}

// Peek the top element on the current stack
void *stack_peek(stack_t *stack) {
    assert(stack);
    assert(stack->current);
    return stack->current->data;
}

// Create a new stack on top of a given one
stack_t *stack_new(stack_t *parent) {
    stack_t *stack = malloc(sizeof(stack_t));
    assert(stack);
    stack_node_t *node = malloc(sizeof(stack_node_t));
    assert(node);
    stack->current = node;
    stack->parent = parent;  
    return stack; 
}

// Drop the current stack, returning the address to the parent one
stack_t *stack_drop(stack_t *stack) {
    assert(stack);
}
