#include <assert.h>

#include "debug.h"
#include "dump.h"
#include "mem.h"
#include "tim.h"
#include "value.h"

frame_t current_frame;
dump_t argument_stack;
dump_t value_stack;

/*********************/
/* Utility functions */
/*********************/

frame_t new_frame(long size){
    frame_t frame = rts_malloc(sizeof(struct frame_t));
    debug_msg("New frame at %p with size %li", frame, size);
    frame->length    = size;
    frame->arguments = NULL;
    debug_msg("New frame has %li arguments at %p"
             , frame->length
             , frame->arguments);
    return frame;
}

closure_t* take_n_closures_from_stack(long n){
    debug_msg("Copying %li arguments from frame %p into argument array"
             , n
             , current_frame);

    if (n == 0) { return NULL; }

    closure_t* closures = rts_malloc(n*sizeof(closure_t));

    for (int i = 0; i < n; i++){
        closure_t* argument = (closure_t*) dump_peek(argument_stack);
        rts_memcpy(&closures[i], argument, sizeof(closure_t));
        dump_pop(argument_stack);
    }

    debug_msg("New closure array at %p with size %li", closures, n);

    return closures;
}

void tim_enter_closure(closure_t *closure){
    debug_msg("Entering closure %p with code %p and frame %p"
             , &closure
             , closure->code
             , closure->frame);
    current_frame = closure->frame;
    return closure->code();
}

/*****************************/
/* Predefined code/functions */
/*****************************/

void tim_nil_code(){
    return;
}

void tim_int_code(){
    debug_msg("Running int code");
    Int* int_ptr_as_frame = (Int*) current_frame;
    debug_msg("Pushing int value %li at %p into the value stack"
             , *int_ptr_as_frame
             ,  int_ptr_as_frame);
    dump_push(value_stack, int_ptr_as_frame);
    return tim_return();
}

void tim_double_code(){
    debug_msg("Running double code");
    Double* double_ptr_as_frame = (Double*) current_frame;
    debug_msg("Pushing double value %f at %p into the value stack"
             , *double_ptr_as_frame
             ,  double_ptr_as_frame);
    dump_push(value_stack, double_ptr_as_frame);
    return tim_return();
}

void tim_string_code(){
    debug_msg("Running string code");
    String* string_ptr_as_frame = (String*) current_frame;
    debug_msg("Pushing string value \"%s\" at %p into the value stack"
             , *string_ptr_as_frame
             ,  string_ptr_as_frame);
    dump_push(value_stack, string_ptr_as_frame);
    return tim_return();
}

/************************/
/* Closure construction */
/************************/

closure_t* make_closure(void (*code)(), void* frame){
    debug_msg("Creating new closure with code at %p and frame %p", code, frame);
    closure_t* closure = rts_malloc(sizeof(closure_t));
    closure->code  = code;
    closure->frame = (frame_t) frame;
    debug_msg("New closure at %p with code %p and frame %p"
             , closure
             , closure->code
             , closure->frame);
    return closure;
}

closure_t* tim_nil_closure(){
    return make_closure(*tim_nil_code, NULL);
}

closure_t* int_closure(Int value){
    debug_msg("Creating new int value closure for %li", value);
    int* int_ptr_as_frame = rts_malloc(sizeof(int));
    *int_ptr_as_frame = value;
    return make_closure(*tim_int_code, int_ptr_as_frame);
}

closure_t* double_closure(Double value){
    debug_msg("Creating new double value closure for %f", value);
    Double* double_ptr_as_frame = rts_malloc(sizeof(Double));
    *double_ptr_as_frame = value;
    return make_closure(*tim_double_code, double_ptr_as_frame);
}

closure_t* string_closure(String value){
    debug_msg("Creating new string value closure for \"%s\"", value);
    String* string_ptr_as_frame = rts_malloc(sizeof(String));
    *string_ptr_as_frame = value;
    return make_closure(*tim_double_code, string_ptr_as_frame);
}

/*****************/
/* Start-up code */
/*****************/

void tim_start_argument_stack(){
    debug_msg("Initializing argument stack");
    argument_stack = dump_new();
    debug_msg("Creating a nil closure for main to land to");
    return dump_push(argument_stack, tim_nil_closure());
}

void tim_start(){
    debug_msg("Initializing frame");
    current_frame = new_frame(0);
    tim_start_argument_stack();
    debug_msg("Initializing value stack");
    value_stack = dump_new();
    debug_msg("Initialization finished");
}

/*******************/
/* Instruction API */
/*******************/

void tim_take (long range){
    debug_msg("Taking %li arguments from frame %p", range, current_frame);
    assert(range <= argument_stack->current_size);
    frame_t frame = new_frame(range);
    frame->arguments = take_n_closures_from_stack(range);
    debug_msg("New frame created at %p with %li argument and closure array %p"
             , frame
             , frame->length
             , frame->arguments);
    current_frame = frame;
}

void tim_push_argument_argument(long offset){
    debug_msg("Pushing argument %li from frame %p", offset, current_frame);
    assert(offset < current_frame->length);
    return dump_push(argument_stack, &current_frame->arguments[offset]);
}

void tim_push_argument_int(Int value){
    debug_msg("Pushing int value %li into the stack", value);
    return dump_push(argument_stack, int_closure(value));
}

void tim_push_argument_double(Double value){
    debug_msg("Pushing double value %f into the stack", value);
    return dump_push(argument_stack, double_closure(value));
}

void tim_push_argument_string(String value){
    debug_msg("Pushing string value \"%s\" into the stack", value);
    return dump_push(argument_stack, string_closure(value));
}

void tim_push_argument_label(void (* code)()){
    debug_msg("Pushing code %p", code);
    return dump_push(argument_stack, make_closure(code, current_frame));
}

void tim_push_value_int(Int value){
    debug_msg("Pushing int %li into the value stack", value);
    Int* p = rts_malloc(sizeof(Int));
    *p = value;
    return dump_push(value_stack, p);
}

void tim_push_value_double(Double value){
    debug_msg("Pushing double %f into the value stack", value);
    Double* p = rts_malloc(sizeof(Double));
    *p = value;
    return dump_push(value_stack, p);
}

void tim_push_value_string(String value){
    debug_msg("Pushing string \"%s\" into the value stack", value);
    String* p = rts_malloc(sizeof(String));
    *p = value;
    return dump_push(value_stack, p);
}

void tim_enter_argument(long argument){
    debug_msg("Entering argument %li from frame %p", argument, current_frame);
    assert(argument<current_frame->length);
    return tim_enter_closure(&(current_frame->arguments[argument]));
}

void tim_enter_int(Int value){
    debug_msg("Entering int value closure for %li", value);
    return tim_enter_closure(int_closure(value));
}

void tim_enter_double(Double value){
    debug_msg("Entering double value closure for %f", value);
    return tim_enter_closure(double_closure(value));
}

void tim_enter_string(String value){
    debug_msg("Entering string value closure for \"%s\"", value);
    return tim_enter_closure(string_closure(value));
}

void tim_enter_label(void (*code)()){
    debug_msg("Entering function closure %p", code);
    return tim_enter_closure(make_closure(code, current_frame));
}

Int get_int_result(){
    debug_msg("Getting an int result from the top of the value stack");
    Int int_result = *((Int*) dump_peek(value_stack));
    debug_msg("The result is %li", int_result);
    return int_result;
}

Double get_double_result(){
    debug_msg("Getting a double result from the top of the value stack");
    Double double_result = *((Double*) dump_peek(value_stack));
    debug_msg("The result is %f", double_result);
    return double_result;
}

String get_string_result(){
    debug_msg("Getting a string result from the top of the value stack");
    String string_result = *((String*) dump_peek(value_stack));
    debug_msg("The result is \"%s\"", string_result);
    return string_result;
}

void tim_return(){
    debug_msg("Returning the topmost closure in the argument stack");
    closure_t* top_closure = (closure_t*) dump_peek(argument_stack);
    dump_pop(argument_stack);
    current_frame = top_closure->frame;
    return top_closure->code();
}

void* tim_pop_value(){
    void* value = dump_peek(value_stack);
    dump_pop(value_stack);
    return value;
}

Int* tim_pop_value_int(){
    return (Int*) tim_pop_value();
};

Double* tim_pop_value_double(){
    return (Double*) tim_pop_value();
};

String* tim_pop_value_string(){
    return (String*) tim_pop_value();
};
