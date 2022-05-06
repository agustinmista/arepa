#ifndef __DEBUG_H__
#define __DEBUG_H__

#include <stdio.h>

#ifdef DEBUG
#define DEBUG_ENABLED 1
#else
#define DEBUG_ENABLED 0
#endif

#define debug_msg(fmt, ...) \
    do { if (DEBUG_ENABLED) fprintf(stderr, "[DEBUG] %-9s | %3d | %-30s | " fmt "\n", __FILE__, __LINE__, __func__, ##__VA_ARGS__); } while (0)

#endif