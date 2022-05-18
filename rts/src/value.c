#include "value.h"

const char* true_str  = "true";
const char* false_str = "false";

const char* bool_str(Bool b) { return b ? true_str : false_str; }