#ifndef SEMANT_H
#define SEMANT_H

#include "env.h"
#include "errormsg.h"
#include "frame.h"

F_fragList SEM_transProg(A_exp exp);

struct expty {Tr_exp exp; Ty_ty ty; } ;
struct expty expTy(Tr_exp exp, Ty_ty ty);

#endif
