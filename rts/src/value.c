#include "value.h"

String true_str  = "true";
String false_str = "false";

String bool_str(Bool b) { return b ? true_str : false_str; }

Unit unit = 0;