#ifndef REGALLOC_H
#define REGALLOC_H

/* function prototype from regalloc.c */
#include "color.h"
typedef struct RA_result_ *RA_result;
struct RA_result_ {Temp_map coloring; AS_instrList il;};
RA_result RA_regAlloc(F_frame f, AS_instrList il);

#endif
