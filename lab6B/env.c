#include "env.h"

/*Lab4: Your implementation of lab4*/

E_enventry E_VarEntry(Tr_access a, Ty_ty ty) {
	E_enventry final;
	final = checked_malloc(sizeof(*final));
	final->kind = E_varEntry;
	final->u.var.access = a;
	final->u.var.ty = ty;
	return final;
}

E_enventry E_FunEntry(Tr_level level, Temp_label label, Ty_tyList fms, Ty_ty resl) {
	E_enventry final = checked_malloc(sizeof(*final));
	final->kind = E_funEntry;
	final->u.fun.formals = fms;
	final->u.fun.level = level;
	final->u.fun.label = label;
	final->u.fun.result  = resl;
	return final;
}

S_table E_base_tenv(void){
	S_table s_table = S_empty();
	S_enter(s_table,S_Symbol("int"),Ty_Int());
	S_enter(s_table,S_Symbol("string"),Ty_String());
	S_enter(s_table,S_Symbol("nil"),Ty_Nil());
	S_enter(s_table,S_Symbol("void"),Ty_Void());
	return s_table;
}

S_table E_base_venv(void) {
  S_table t = S_empty();
  
  S_enter(
    t,
    S_Symbol("print"),
    E_FunEntry(Tr_outermost(), Temp_namedlabel("print"), Ty_TyList(Ty_String(), NULL), Ty_Void())
  	);
  S_enter(
    t,
    S_Symbol("initArray"),
    E_FunEntry(Tr_outermost(), Temp_namedlabel("initArray"), Ty_TyList(Ty_Int(), Ty_TyList(Ty_Int(), NULL)), Ty_Int())
  	);
  S_enter(
    t,
    S_Symbol("allocRecord"),
    E_FunEntry(Tr_outermost(), Temp_namedlabel("allocRecord"), Ty_TyList(Ty_Int(), NULL), Ty_Int())
  	);
  S_enter(
    t,
    S_Symbol("stringEqual"),
    E_FunEntry(Tr_outermost(), Temp_namedlabel("stringEqual"), Ty_TyList(Ty_String(), Ty_TyList(Ty_String(), NULL)), Ty_Int())
  	);
  S_enter(
    t,
    S_Symbol("printi"),
    E_FunEntry(Tr_outermost(), Temp_namedlabel("printi"), Ty_TyList(Ty_Int(), NULL), Ty_Void())
  	);
  S_enter(
    t,
    S_Symbol("flush"),
    E_FunEntry(Tr_outermost(), Temp_namedlabel("flush"), NULL, Ty_Void())
  	);
  S_enter(
    t,
    S_Symbol("getchar"),
    E_FunEntry(Tr_outermost(), Temp_namedlabel("getchar"), NULL, Ty_String())
  	);
  S_enter(
    t,
    S_Symbol("__wrap_getchar"),
    E_FunEntry(Tr_outermost(), Temp_namedlabel("__wrap_getchar"), NULL, Ty_String())
  	);
  S_enter(
    t,
    S_Symbol("ord"),
    E_FunEntry(Tr_outermost(), Temp_namedlabel("ord"), Ty_TyList(Ty_String(), NULL), Ty_Int())
  	);
  S_enter(
    t,
    S_Symbol("chr"),
    E_FunEntry(Tr_outermost(), Temp_namedlabel("chr"), Ty_TyList(Ty_Int(), NULL), Ty_String())
  	);
  S_enter(
    t,
    S_Symbol("size"),
    E_FunEntry(Tr_outermost(), Temp_namedlabel("size"), Ty_TyList(Ty_String(), NULL), Ty_Int())
  	);
  S_enter(
    t,
    S_Symbol("substring"),
    E_FunEntry(
		Tr_outermost(), 
		Temp_namedlabel("substring"), 
		Ty_TyList(Ty_String(), Ty_TyList(Ty_Int(), Ty_TyList(Ty_Int(), NULL))),
		Ty_String())
  	);
  S_enter(
    t,
    S_Symbol("concat"),
    E_FunEntry(
		Tr_outermost(), 
		Temp_namedlabel("concat"), 
		Ty_TyList(Ty_String(), Ty_TyList(Ty_String(), NULL)),
        Ty_String())
  	);
  S_enter(
    t,
    S_Symbol("not"),
    E_FunEntry(Tr_outermost(), Temp_namedlabel("not"), Ty_TyList(Ty_Int(), NULL), Ty_Int())
  	);
  S_enter(
    t,
    S_Symbol("exit"),
    E_FunEntry(Tr_outermost(), Temp_namedlabel("exit"), Ty_TyList(Ty_Int(), NULL), Ty_Void())
  	);
	return t;
}

void E_push(E_stack* stack,void *object){
  E_stack node = checked_malloc(sizeof(*node));
  node->object = object;
  node->next = *stack;
  *stack = node;
}
void E_pop(E_stack* stack){
  if(stack==NULL){
    fprintf(stdout,"stack already empty\n");
    assert(0);
  }
  if(*stack!=NULL){
    *stack =(*stack)->next;
  }
}
void* E_top(E_stack stack){
  if(stack==NULL){
    fprintf(stdout,"stack already empty\n");
    assert(0);
  }
  return stack->object;
}
E_stack E_newStack(){
  return NULL;
}
