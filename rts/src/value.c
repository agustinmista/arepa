#include "value.h"

Bool   true  = 1;
Bool   false = 0;

String true_str  = "true";
String false_str = "false";
String bool_str(Bool b) { return b ? true_str : false_str; }

Unit   unit     = 0;

String unit_str = "unit";