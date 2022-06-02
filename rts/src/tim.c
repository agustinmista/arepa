#include <assert.h>
#include <stdlib.h>

#include "debug.h"
#include "dump.h"
#include "mem.h"
#include "tim.h"
#include "value.h"
#include "io.h"
#include "gc.h"

frame_t current_frame;
frame_t current_data_frame;

dump_t argument_stack;
dump_t value_stack;

/****************/
/* Declarations */
/****************/

closure_t* argument_closure(long argument);

/*********************/
/* Utility functions */
/*********************/

void initialize_arguments(long size, closure_t arguments[]){
    for(long i = 0; i < size; i++) {
        arguments[i].frame = NULL;
        arguments[i].code  = *tim_nil_code;
    }
}

frame_t new_frame(long size) {
    debug_msg("Creating new frame of size %li", size);
    frame_t frame = gc_malloc(sizeof(struct frame_t));
    frame->length = size;
    frame->is_partial = 0;
    closure_t* arguments = gc_malloc(size*sizeof(closure_t));
    initialize_arguments(size,arguments);
    frame->arguments = arguments;
    return frame;
}

frame_t new_partial_frame(long size) {
    debug_msg("Creating new partial frame of size %li", size);
    frame_t frame = new_frame(size);
    frame->is_partial = 1;
    return frame;
}

void copy_n_stack_arguments_to_frame(long n, frame_t frame, stack_t stack) {
    debug_msg("Copying %lu closures from the stack at %p into the frame at %p", n, stack, frame);
    assert(frame != current_frame); // Sanity check!
    for (int i = 0; i < n; i++) {
        assert(stack);
        closure_t* closure = (closure_t*) stack_peek(stack);
        rts_memcpy(&frame->arguments[i], closure, sizeof(closure_t));
        stack = stack->next;
    }
}

void move_n_stack_arguments_to_frame(long n, frame_t frame) {
    debug_msg("Moving %lu closures from the current stack into the frame at %p", n, frame);
    assert(n <= argument_stack->current_size);
    assert(frame != current_frame); // Sanity check!
    for (int i = 0; i < n; i++){
        closure_t* closure = (closure_t*) dump_pop(argument_stack);
        rts_memcpy(&frame->arguments[i], closure, sizeof(closure_t));
    }
}

void tim_enter_closure(closure_t *closure) {
    debug_msg("Entering closure %p with code at %p and frame at %p", &closure, closure->code, closure->frame);
    current_frame = closure->frame;
    return closure->code();
}

void tim_update_closure(long offset, closure_t *closure) {
    debug_msg("Copying closure %p to current frame slot $%li at %p", closure, offset, &current_frame->arguments[offset]);
    rts_memcpy(&current_frame->arguments[offset], closure, sizeof(closure_t));
}

void tim_populate_partial_arguments() {
    debug_msg("Filling the arguments stack with arguments of a partial frame");
    if (!current_frame->is_partial) return;
    for (int i=0; i < current_frame->length; i++){
        dump_push(argument_stack, argument_closure(i));
    }
}

void update_closure_frame_in_metadata_frame(tim_metadata_t metadata, frame_t frame) {
    debug_msg("Updating the corresponding closure in the frame retrived from the dump");
    frame_t target_frame = metadata->frame;
    long offset          = metadata->offset;
    target_frame->arguments[offset].frame = frame;
}

void update_closure_code_in_metadata_frame(tim_metadata_t metadata, void (*code)()) {
    debug_msg("Updating the corresponding code in the frame retrived from the dump");
    frame_t target_frame = metadata->frame;
    long offset          = metadata->offset;
    target_frame->arguments[offset].code = code;
}

void tim_handle_partial_application() {
    debug_msg("Restoring previous stack from the dump");
    dump_overlay_previous(argument_stack);
    long argn = argument_stack->current_size;
    frame_t frame = new_partial_frame(argn);
    copy_n_stack_arguments_to_frame(argn, frame, argument_stack->current);
    tim_metadata_t metadata = (tim_metadata_t) argument_stack->metadata;
    update_closure_frame_in_metadata_frame(metadata, frame);
}

void return_to_continuation() {
    debug_msg("Returning the topmost closure in the argument stack");
    closure_t* top_closure = (closure_t*) dump_pop(argument_stack);
    current_frame = top_closure->frame;
    return top_closure->code();
}

void restore_dump_and_update(void (*code)(), void* frame) {
    debug_msg("Returning from an empty stack into the previous stack in the dump");
    dump_overlay_previous(argument_stack);
    tim_metadata_t metadata = (tim_metadata_t) argument_stack->metadata;
    update_closure_code_in_metadata_frame(metadata, code);
    update_closure_frame_in_metadata_frame(metadata, frame);
}

/*****************************/
/* Predefined code/functions */
/*****************************/

void tim_nil_code() {
    return;
}

void tim_value_code() {
    debug_msg("Running value code");
    void* value_ptr_as_frame = (void*) current_frame;
    debug_msg("Pushing value at %p into the value stack", value_ptr_as_frame);
    dump_push(value_stack, value_ptr_as_frame);
    return tim_return();
}

/************************/
/* Closure construction */
/************************/

closure_t* make_closure(void (*code)(), void* frame) {
    closure_t* closure = gc_malloc(sizeof(closure_t));
    closure->code  = code;
    closure->frame = (frame_t) frame;
    debug_msg("New closure at %p with code %p and frame %p", closure, closure->code, closure->frame);
    return closure;
}

closure_t* tim_nil_closure() {
    return make_closure(*tim_nil_code, NULL);
}

closure_t* argument_closure(long argument) {
    debug_msg("Creating new argument closure from current frame slot $%li", argument);
    assert(argument < current_frame->length);
    closure_t* closure = &current_frame->arguments[argument];
    return make_closure(closure->code, closure->frame);
}

closure_t* int_closure(Int value) {
    debug_msg("Creating new int value closure for %li", value);
    Int* int_ptr_as_frame = gc_malloc(sizeof(Int));
    *int_ptr_as_frame = value;
    return make_closure(*tim_value_code, int_ptr_as_frame);
}

closure_t* double_closure(Double value) {
    debug_msg("Creating new double value closure for %f", value);
    Double* double_ptr_as_frame = gc_malloc(sizeof(Double));
    *double_ptr_as_frame = value;
    return make_closure(*tim_value_code, double_ptr_as_frame);
}

closure_t* string_closure(String value) {
    debug_msg("Creating new string value closure for \"%s\"", value);
    String* string_ptr_as_frame = gc_malloc(sizeof(String));
    *string_ptr_as_frame = value;
    return make_closure(*tim_value_code, string_ptr_as_frame);
}

closure_t* bool_closure(Bool value) {
    debug_msg("Creating new bool value closure for %s", bool_str(value));
    Bool* bool_ptr_as_frame = gc_malloc(sizeof(Bool));
    *bool_ptr_as_frame = value;
    return make_closure(*tim_value_code, bool_ptr_as_frame);
}

closure_t* unit_closure(Unit value) {
    debug_msg("Creating new unit value closure for %li", value);
    Unit* unit_ptr_as_frame = gc_malloc(sizeof(Unit));
    *unit_ptr_as_frame = value;
    return make_closure(*tim_value_code, unit_ptr_as_frame);
}

closure_t* label_closure(void (*code)()) {
    debug_msg("Creating new label closure for code at %p", code);
    return make_closure(code, current_frame);
}

closure_t* con_closure(long field) {
    debug_msg("Creating new closure for current constructor field %lu", field);
    assert(field < current_data_frame->length);
    closure_t* closure = &current_data_frame->arguments[field];
    return make_closure(closure->code, closure->frame);
}

/*****************/
/* Start-up code */
/*****************/

void tim_init_current_frame() {
    debug_msg("Initializing frame");
    current_frame = new_frame(0);
}

void tim_init_current_data_frame() {
    debug_msg("Initializing data frame");
    current_data_frame = new_frame(0);
}

void tim_init_argument_stack() {
    debug_msg("Initializing argument stack");
    argument_stack = dump_new(NULL);
    debug_msg("Creating a nil closure for main to land to");
    return dump_push(argument_stack, tim_nil_closure());
}

void tim_init_value_stack() {
    debug_msg("Initializing value stack");
    value_stack = dump_new(NULL);
}

void tim_init_io_streams() {
    debug_msg("Initializing IO streams");
    set_rts_stdin(stdin);
    set_rts_stdout(stdout);
}

void tim_start() {
    debug_msg("Initialization started");
    gc_init();
    tim_init_current_frame();
    tim_init_current_data_frame();
    tim_init_argument_stack();
    tim_init_value_stack();
    tim_init_io_streams();
    debug_msg("Initialization finished");
}

/*******************/
/* Instruction API */
/*******************/

void tim_take(long total, long n) {
    debug_msg("Taking %li arguments from stack into new frame of size %li", n, total);
    assert(total >= n);
    frame_t frame = new_frame(total);
    move_n_stack_arguments_to_frame(n, frame);
    current_frame = frame;
}

void tim_push_argument_argument(long argument) {
    debug_msg("Pushing argument $%li from frame %p", argument, current_frame);
    return dump_push(argument_stack, argument_closure(argument));
}

void tim_push_argument_int(Int value) {
    debug_msg("Pushing int value %li into the argument stack", value);
    return dump_push(argument_stack, int_closure(value));
}

void tim_push_argument_double(Double value) {
    debug_msg("Pushing double value %f into the argument stack", value);
    return dump_push(argument_stack, double_closure(value));
}

void tim_push_argument_string(String value) {
    debug_msg("Pushing string value \"%s\" into the argument stack", value);
    return dump_push(argument_stack, string_closure(value));
}

void tim_push_argument_bool(Bool value) {
    debug_msg("Pushing bool value %s into the argument stack", bool_str(value));
    return dump_push(argument_stack, bool_closure(value));
}

void tim_push_argument_unit(Unit value) {
    debug_msg("Pushing unit value %li into the argument stack", value);
    return dump_push(argument_stack, unit_closure(value));
}

void tim_push_argument_label(void (*code)()) {
    debug_msg("Pushing function at %p into the argument stack", code);
    return dump_push(argument_stack, label_closure(code));
}

void tim_push_argument_data(long field) {
    debug_msg("Pushing data constructor field %lu into the argument stack", field);
    return dump_push(argument_stack, con_closure(field));
}

void tim_push_value_int(Int value) {
    debug_msg("Pushing int %li into the value stack", value);
    Int* ptr = gc_malloc(sizeof(Int));
    *ptr = value;
    return dump_push(value_stack, ptr);
}

void tim_push_value_double(Double value) {
    debug_msg("Pushing double %f into the value stack", value);
    Double* ptr = gc_malloc(sizeof(Double));
    *ptr = value;
    return dump_push(value_stack, ptr);
}

void tim_push_value_string(String value) {
    debug_msg("Pushing string \"%s\" into the value stack", value);
    String* ptr = gc_malloc(sizeof(String));
    *ptr = value;
    return dump_push(value_stack, ptr);
}

void tim_push_value_bool(Bool value) {
    debug_msg("Pushing bool %s into the value stack", bool_str(value));
    Bool* ptr = gc_malloc(sizeof(Bool));
    *ptr = value;
    return dump_push(value_stack, ptr);
}

void tim_push_value_unit(Unit value) {
    debug_msg("Pushing unit %li into the value stack", value);
    Unit* ptr = gc_malloc(sizeof(Unit));
    *ptr = value;
    return dump_push(value_stack, ptr);
}

void tim_push_value_data(tag_t tag) {
    debug_msg("Pushing data constructor tag %lu into the value stack", tag);
    tag_t* ptr = gc_malloc(sizeof(tag_t));
    *ptr = tag;
    return dump_push(value_stack, ptr);
}

void* tim_pop_value() {
    debug_msg("Popping value from value stack");
    void* value = dump_pop(value_stack);
    return value;
}

Int* tim_pop_value_int() {
    Int* ptr = tim_pop_value();
    debug_msg("Popped int value %li from the value stack", *ptr);
    return ptr;
}

Double* tim_pop_value_double() {
    Double* ptr = tim_pop_value();
    debug_msg("Popped double value %f from the value stack", *ptr);
    return ptr;
}

String* tim_pop_value_string() {
    String* ptr = tim_pop_value();
    debug_msg("Popped string value \"%s\" from the value stack", *ptr);
    return ptr;
}

Bool* tim_pop_value_bool() {
    Bool* ptr = tim_pop_value();
    debug_msg("Popped bool value %s from the value stack", bool_str(*ptr));
    return ptr;
}

Unit* tim_pop_value_unit() {
    Unit* ptr = tim_pop_value();
    debug_msg("Popped unit value %li from the value stack", *ptr);
    return ptr;
}

tag_t* tim_pop_value_data() {
    tag_t* ptr = tim_pop_value();
    debug_msg("Popped data constructor tag %lu from the value stack", *ptr);
    return ptr;
}

void tim_enter_argument(long argument) {
    debug_msg("Entering argument %li from current frame %p", argument, current_frame);
    assert(argument < current_frame->length);
    return tim_enter_closure(&current_frame->arguments[argument]);
}

void tim_enter_int(Int value) {
    debug_msg("Entering int value closure for %li", value);
    return tim_enter_closure(int_closure(value));
}

void tim_enter_double(Double value) {
    debug_msg("Entering double value closure for %f", value);
    return tim_enter_closure(double_closure(value));
}

void tim_enter_string(String value) {
    debug_msg("Entering string value closure for \"%s\"", value);
    return tim_enter_closure(string_closure(value));
}

void tim_enter_bool(Bool value) {
    debug_msg("Entering bool value closure for %s", bool_str(value));
    return tim_enter_closure(bool_closure(value));
}

void tim_enter_unit(Unit value) {
    debug_msg("Entering unit value closure for %li", value);
    return tim_enter_closure(unit_closure(value));
}

void tim_enter_label(void (*code)()) {
    debug_msg("Entering function at %p", code);
    return tim_enter_closure(label_closure(code));
}

void tim_enter_data(long field) {
    debug_msg("Entering data constructor field %lu", field);
    return tim_enter_closure(con_closure(field));
}

void tim_move_argument(long offset, long argument) {
    debug_msg("Moving argument %li to frame current slot $%li", argument, offset);
    return tim_update_closure(offset, argument_closure(argument));
}

void tim_move_int(long offset, Int value) {
    debug_msg("Moving int value %li to current frame slot $%li", value, offset);
    return tim_update_closure(offset, int_closure(value));
}

void tim_move_double(long offset, Double value) {
    debug_msg("Moving double value %f to current frame slot $%li", value, offset);
    return tim_update_closure(offset, double_closure(value));
}

void tim_move_string(long offset, String value) {
    debug_msg("Moving string value \"%s\" to current frame slot $%li", value, offset);
    return tim_update_closure(offset, string_closure(value));
}

void tim_move_bool(long offset, Bool value) {
    debug_msg("Moving bool value %s to current frame slot $%li", bool_str(value), offset);
    return tim_update_closure(offset, bool_closure(value));
}

void tim_move_unit(long offset, Unit value) {
    debug_msg("Moving unit value %li to current frame slot $%li", value, offset);
    return tim_update_closure(offset, unit_closure(value));
}

void tim_move_label(long offset, void (*code)()) {
    debug_msg("Moving function at %p to current frame slot $%li", code, offset);
    return tim_update_closure(offset, label_closure(code));
}

void tim_move_data(long offset, long field) {
    debug_msg("Moving data constructor field %lu to current frame slot $%li", field, offset);
    return tim_update_closure(offset, con_closure(field));
}

void tim_marker_push(long offset) {
    tim_metadata_t metadata = gc_malloc(sizeof(struct tim_metadata_t));
    metadata->offset = offset;
    metadata->frame  = current_frame;
    dump_freeze(argument_stack, metadata);
}

void tim_markers_update(long nargs) {
    if (nargs == 0) return;
    tim_populate_partial_arguments();
    if (nargs <= argument_stack->current_size) return;
    tim_handle_partial_application();
    return tim_markers_update(nargs);
}

void tim_return() {
    if (dump_is_empty(argument_stack)) {
        void* value = dump_peek(value_stack);
        restore_dump_and_update(*tim_value_code, value);
        return tim_return();
    } else {
        return return_to_continuation();
    }
}

void tim_data(tag_t tag, void (*code)()) {
    if (dump_is_empty(argument_stack)) {
        restore_dump_and_update(code, current_frame);
        return tim_data(tag, code);
    } else {
        current_data_frame = current_frame;
        tim_push_value_data(tag);
        return return_to_continuation();
    }
}

void tim_freeze() {
    debug_msg("Freezing the value stack");
    dump_freeze(value_stack, NULL);
}

void tim_restore() {
    debug_msg("Restoring the previous value stack");
    dump_restore(value_stack);
}

void tim_switch_error(tag_t tag) {
    rts_printf("non-exhaustive alternatives for tag %lu\n", tag);
    exit(EXIT_FAILURE);
}

Int get_int_result() {
    Int int_result = *((Int*) dump_peek(value_stack));
    debug_msg("Peeked int %li from the value stack", int_result);
    return int_result;
}

Double get_double_result() {
    Double double_result = *((Double*) dump_peek(value_stack));
    debug_msg("Peeked double %f from the value stack", double_result);
    return double_result;
}

String get_string_result() {
    String string_result = *((String*) dump_peek(value_stack));
    debug_msg("Peeked string \"%s\" from the value stack", string_result);
    return string_result;
}

Bool get_bool_result() {
    Bool bool_result = *((Bool*) dump_peek(value_stack));
    debug_msg("Peeked bool %s from the value stack", bool_result ? "true" : "false");
    return bool_result;
}

Bool get_unit_result() {
    Unit unit_result = *((Unit*) dump_peek(value_stack));
    debug_msg("Peeked unit %li from the value stack", unit_result);
    return unit_result;
}

