#ifndef SEMANT_H
#define SEMANT_H

#include "env.h"
#include "errormsg.h"

void SEM_transProg(A_exp exp);

typedef void *Tr_exp;

struct expty {Tr_exp exp; Ty_ty ty; } ;
struct expty expTy(Tr_exp exp, Ty_ty ty);

#endif
