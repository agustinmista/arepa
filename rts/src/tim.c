#include <assert.h>

#include "debug.h"
#include "dump.h"
#include "mem.h"
#include "tim.h"

frame_t current_frame;
dump_t current_stack;
int current_value;

frame_t new_frame(long size){
  frame_t frame = rts_malloc(sizeof(struct frame_t));
  debug_msg("New frame at %p with size %li", frame, size);
  frame->length    = size;
  frame->arguments = NULL;
  debug_msg("New frame has %li arguments at %p", frame->length, frame->arguments);
  return frame;
}

void tim_start(){
  debug_msg("Initializing frame and dump");
  current_frame = new_frame(0);
  current_stack = dump_new();
  debug_msg("Initialization finished");
}

closure_t* take_n_closures_from_stack(long n){
    debug_msg("Copying %li arguments from frame %p into argument array"
             , n
             , current_frame);

    closure_t* closures = rts_malloc(sizeof(n*sizeof(closure_t)));

    for (int i = 0; i < n; i++){
        closure_t* argument = (closure_t*) dump_peek(current_stack);
        rts_memcpy(&closures[i],argument,sizeof(closure_t));
        dump_pop(current_stack);
    }

    debug_msg("New closure array at %p with size %li", closures, n);

    return closures;
}

void tim_take (long range){
    debug_msg("Taking %li arguments from frame %p",range,current_frame);
    assert(range <= current_frame->length);
    frame_t frame = new_frame(range);
    frame->arguments = take_n_closures_from_stack(range);
    current_frame = frame;
}

void tim_push_argument(long argument){
    debug_msg("Pushing argument %li from frame %p",argument,current_frame);
    assert(argument < current_frame->length);
    return dump_push(current_stack,&current_frame->arguments[argument]);
}

void tim_int_code(){
    debug_msg("Running int code");
    int literal = *((int*) current_frame);
    current_value = literal;
}

void tim_float_code(){
    debug_msg("Running float code");
    float literal = *((float*) current_frame);
    current_value = (int) literal;
}

closure_t* make_closure(void (*code)(),void* frame){
    debug_msg("Creating new closure with code at %p and frame %p",code,frame);
    closure_t* closure = rts_malloc(sizeof(closure_t));
    closure->code  = code;
    closure->frame = (frame_t) frame;
    debug_msg("New closure at %p with code %p and frame %p"
             , closure
             , closure->code
             , closure->frame);
    return closure;
}

closure_t* int_closure(int literal){
    debug_msg("Creating new int literal closure for %i", literal);
    int* int_ptr_as_frame = rts_malloc(sizeof(int));
    *int_ptr_as_frame = literal;
    return make_closure(*tim_int_code,int_ptr_as_frame);
}

void tim_push_literal_int(int literal){
    debug_msg("Pushing int literal %i into the stack", literal);
    return dump_push(current_stack,int_closure(literal));
}

closure_t* float_closure(float literal){
    debug_msg("Creating new int literal closure for %f", literal);
    float* float_ptr_as_frame = rts_malloc(sizeof(float));
    *float_ptr_as_frame = literal;
    return make_closure(*tim_float_code,float_ptr_as_frame);
}
void tim_push_literal_float(float literal){
    debug_msg("Pushing float literal %f into the stack", literal);
    return dump_push(current_stack,float_closure(literal));
}

void tim_push_label(void (* code)()){
    debug_msg("Pushing code %p", code);
    return dump_push(current_stack,make_closure(code,current_frame));
}

void tim_enter_closure(closure_t closure){
    debug_msg("Entering closure %p with code %p and frame %p"
             , &closure
             , closure.code
             , closure.frame);
    current_frame = closure.frame;
    return closure.code();
}

void tim_enter_argument(long argument){
    debug_msg("Entering argument %li from frame %p",argument,current_frame);
    assert(argument<current_frame->length);
    return tim_enter_closure(current_frame->arguments[argument]);
}

void tim_enter_literal_int(int literal){
    debug_msg("Entering int literal closure for %i", literal);
    return tim_enter_closure(*int_closure(literal));
}

void tim_enter_literal_float(float literal){
    debug_msg("Entering float literal closure for %f", literal);
    return tim_enter_closure(*float_closure(literal));
}

void tim_enter_label(void (*code)()){
    debug_msg("Entering function closure %p", code);
    return tim_enter_closure(*make_closure(code,current_frame));
}

int get_result(){
    debug_msg("The curren result is %i", current_value);
    return current_value;
}