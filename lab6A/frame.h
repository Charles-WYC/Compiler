
/*Lab5: This header file is not complete. Please finish it with more definition.*/

#include "temp.h"
#include "tree.h"
#ifndef FRAME_H
#define FRAME_H

typedef struct F_frame_ *F_frame;
typedef struct F_access_ * F_access;/*format-params & local-vat info in register or stack*/
typedef struct F_accessList_ * F_accessList;

struct F_accessList_ {F_access head; F_accessList tail;};

F_frame F_newFrame(Temp_label name, U_boolList formals); /*construct-func -> stack-frame*/
Temp_label F_name(F_frame f);
F_accessList F_formals(F_frame f);
F_access F_allocLocal(F_frame f, bool escape);/*alloc a new local-var*/

Temp_map F_tempMap;

/* declaration for fragments */
typedef struct F_frag_ *F_frag;
struct F_frag_ {enum {F_stringFrag, F_procFrag} kind;
			union {
				struct {Temp_label label; string str;} stringg;
				struct {T_stm body; F_frame frame;} proc;
			} u;
};

F_frag F_StringFrag(Temp_label label, string str);
F_frag F_ProcFrag(T_stm body, F_frame frame);

typedef struct F_fragList_ *F_fragList;
struct F_fragList_ {F_frag head; F_fragList tail;};
F_fragList F_FragList(F_frag head, F_fragList tail);


extern const int F_WORD_SIZE;

Temp_temp F_FP();
T_exp F_Exp(F_access, T_exp);
T_exp F_externalCall(string, T_expList);
T_stm F_procEntryExit1(F_frame, T_stm);
Temp_temp F_RV();

bool F_isEscape(F_access);
Temp_tempList F_calldefs();

#endif
