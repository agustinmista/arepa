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
  frame->length    = size;
  frame->arguments = NULL;
  return frame;
}

void tim_start(){
  current_frame = new_frame(0);
  current_stack = dump_new();
}

closure_t* take_n_closures_from_stack(long n){

    closure_t* closures = rts_malloc(sizeof(n*sizeof(closure_t)));

    for (int i = 0; i < n; i++){
        closure_t* argument = (closure_t*) dump_peek(current_stack);
        rts_memcpy(&closures[i],argument,sizeof(closure_t));
        dump_pop(current_stack);
    }

  return closures;
}

void tim_take (long range){
    assert(range <= current_frame->length);
    frame_t frame = new_frame(range);
    frame->arguments = take_n_closures_from_stack(range);
    current_frame = frame;
}

void tim_push_argument(long argument){
    assert(argument < current_frame->length);
    return dump_push(current_stack,&current_frame->arguments[argument]);
}

void tim_int_code(){
    int literal = *((int*) current_frame);
    current_value = literal;
}

void tim_float_code(){
    float literal = *((float*) current_frame);
    current_value = (int) literal;
}

closure_t* make_closure(void (*code)(),void* frame){
    closure_t* closure = rts_malloc(sizeof(closure_t));
    closure->code  = code;
    closure->frame = (frame_t) frame;
    return closure;
}

closure_t* int_closure(int literal){
    int* int_ptr_as_frame = rts_malloc(sizeof(int));
    *int_ptr_as_frame = literal;
    return make_closure(*tim_int_code,int_ptr_as_frame);
}

void tim_push_literal_int(int literal){
    return dump_push(current_stack,int_closure(literal));
}

closure_t* float_closure(float literal){
    float* float_ptr_as_frame = rts_malloc(sizeof(float));
    *float_ptr_as_frame = literal;
    return make_closure(*tim_float_code,float_ptr_as_frame);
}
void tim_push_literal_float(float literal){
    return dump_push(current_stack,float_closure(literal));
}

void tim_push_label(void (* code)()){
    return dump_push(current_stack,make_closure(code,current_frame));
}

void tim_enter_closure(closure_t closure){
    current_frame = closure.frame;
    return closure.code();
}

void tim_enter_argument(long argument){
    assert(argument<current_frame->length);
    return tim_enter_closure(current_frame->arguments[argument]);
}

void tim_enter_literal_int(int literal){
    return tim_enter_closure(*int_closure(literal));
}

void tim_enter_literal_float(float literal){
    return tim_enter_closure(*float_closure(literal));
}

void tim_enter_label(void (*code)()){
    return tim_enter_closure(*make_closure(code,current_frame));
}

int get_result(){
    return current_value;
}