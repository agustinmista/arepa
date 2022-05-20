#include "value.h"

const String true_str  = "true";
const String false_str = "false";

const String bool_str(Bool b) { return b ? true_str : false_str; }

const Unit unit = 0;