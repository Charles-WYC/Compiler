/*
 * canon.c - Functions to convert the IR trees into basic blocks and traces.
 *
 */
#include <stdio.h>
#include "util.h"
#include "symbol.h"
#include "temp.h"
#include "tree.h"
#include "canon.h"
#include "assem.h"
#include "frame.h"

typedef struct expRefList_ *expRefList;
struct expRefList_ {T_exp *head; expRefList tail;};

/* local function prototypes */
static T_stm do_stm(T_stm stm);
static struct stmExp do_exp(T_exp exp);
static C_stmListList mkBlocks(T_stmList stms, Temp_label done);
static T_stmList getNext(void);

static expRefList ExpRefList(T_exp *head, expRefList tail)
{expRefList p = (expRefList) checked_malloc (sizeof *p);
 p->head=head; p->tail=tail;
 return p;
}

static bool isNop(T_stm x) 
{  return x->kind == T_EXP && x->u.EXP->kind == T_CONST;
 }

static T_stm seq(T_stm x, T_stm y)
{
 if (isNop(x)) return y;
 if (isNop(y)) return x;
 return T_Seq(x,y);
}

static bool commute(T_stm x, T_exp y)
{
 if (isNop(x)) return TRUE;
 if (y->kind == T_NAME || y->kind == T_CONST) return TRUE;
 return FALSE;
}

struct stmExp {T_stm s; T_exp e;};

static T_stm reorder(expRefList rlist) {
   if (!rlist) return T_Exp(T_Const(0)); /* nop */
   else if ((*rlist->head)->kind==T_CALL) {
      Temp_temp t = Temp_newtemp();
      *rlist->head = T_Eseq(T_Move(T_Temp(t),*rlist->head),T_Temp(t));
      return reorder(rlist);
    }
   else {
      struct stmExp hd = do_exp(*rlist->head);
      T_stm s = reorder(rlist->tail);
      if (commute(s,hd.e)) {
	 *rlist->head=hd.e;
         return seq(hd.s,s);
      } else {
        Temp_temp t = Temp_newtemp();
        *rlist->head=T_Temp(t);
        return seq(hd.s, seq(T_Move(T_Temp(t),hd.e), s));
      }
    }
 }

static expRefList get_call_rlist(T_exp exp)
{expRefList rlist, curr;
 T_expList args = exp->u.CALL.args;
 curr = rlist = ExpRefList(&exp->u.CALL.fun, NULL);
 for (;args; args=args->tail) {
   curr = curr->tail = ExpRefList(&args->head, NULL);
 }
 return rlist;
}

static struct stmExp StmExp(T_stm stm, T_exp exp) {
  struct stmExp x;
  x.s = stm;
  x.e = exp;
  return x;
}

static struct stmExp do_exp(T_exp exp)
{
  switch(exp->kind) {
  case T_BINOP: 
    return StmExp(reorder(ExpRefList(&exp->u.BINOP.left, 
		       ExpRefList(&exp->u.BINOP.right, NULL))),
		  exp);
  case T_MEM: 
    return StmExp(reorder(ExpRefList(&exp->u.MEM, NULL)), exp);
  case T_ESEQ:
    {struct stmExp x = do_exp(exp->u.ESEQ.exp);
     return StmExp(seq(do_stm(exp->u.ESEQ.stm), x.s), x.e);
    }
  case T_CALL:    
      return StmExp(reorder(get_call_rlist(exp)), exp);
  default:
    return StmExp(reorder(NULL), exp);
  }
}

/* processes stm so that it contains no ESEQ nodes */
static T_stm do_stm(T_stm stm)
{
  switch (stm->kind) {
  case T_SEQ: 
    return seq(do_stm(stm->u.SEQ.left), do_stm(stm->u.SEQ.right));
  case T_JUMP:
    return seq(reorder(ExpRefList(&stm->u.JUMP.exp, NULL)), stm);
  case T_CJUMP:
    return seq(reorder(ExpRefList(&stm->u.CJUMP.left, 
				  ExpRefList(&stm->u.CJUMP.right,NULL))), stm);
  case T_MOVE:
    if (stm->u.MOVE.dst->kind == T_TEMP && stm->u.MOVE.src->kind == T_CALL)
      return seq(reorder(get_call_rlist(stm->u.MOVE.src)), stm);
    else if (stm->u.MOVE.dst->kind == T_TEMP)
      return seq(reorder(ExpRefList(&stm->u.MOVE.src, NULL)), stm);
    else if (stm->u.MOVE.dst->kind == T_MEM)
      return seq(reorder(ExpRefList(&stm->u.MOVE.dst->u.MEM, 
			 ExpRefList(&stm->u.MOVE.src, NULL))), stm);
    else if (stm->u.MOVE.dst->kind == T_ESEQ) {
      T_stm s = stm->u.MOVE.dst->u.ESEQ.stm;
      stm->u.MOVE.dst = stm->u.MOVE.dst->u.ESEQ.exp;
      return do_stm(T_Seq(s, stm));
    }
    else{
      return stm;
    }
    assert(0); /* dst should be temp or mem only */
  case T_EXP:
    if (stm->u.EXP->kind == T_CALL)
         return seq(reorder(get_call_rlist(stm->u.EXP)), stm);
    else return seq(reorder(ExpRefList(&stm->u.EXP, NULL)), stm);
 default:
    return stm;
 }
}

/* linear gets rid of the top-level SEQ's, producing a list */
static T_stmList linear(T_stm stm, T_stmList right)
{
 if (stm->kind == T_SEQ) 
   return linear(stm->u.SEQ.left,linear(stm->u.SEQ.right,right));
 else return T_StmList(stm, right);
}

/* From an arbitrary Tree statement, produce a list of cleaned trees
   satisfying the following properties:
      1.  No SEQ's or ESEQ's
      2.  The parent of every CALL is an EXP(..) or a MOVE(TEMP t,..) */
T_stmList C_linearize(T_stm stm)
{
    return linear(do_stm(stm), NULL);
}

static C_stmListList StmListList(T_stmList head, C_stmListList tail)
{C_stmListList p = (C_stmListList) checked_malloc (sizeof *p);
 p->head=head; p->tail=tail;
 return p;
}
 
/* Go down a list looking for end of basic block */
static C_stmListList next(T_stmList prevstms, T_stmList stms, Temp_label done)
{
  if (!stms) 
    return next(prevstms, 
		T_StmList(T_Jump(T_Name(done), Temp_LabelList(done, NULL)), 
			  NULL), done);
  if (stms->head->kind == T_JUMP || stms->head->kind == T_CJUMP) {
    C_stmListList stmLists;
    prevstms->tail = stms; 
    stmLists = mkBlocks(stms->tail, done);
    stms->tail = NULL;
    return stmLists;
  } 
  else if (stms->head->kind == T_LABEL) {
    Temp_label lab = stms->head->u.LABEL;
    return next(prevstms, T_StmList(T_Jump(T_Name(lab), Temp_LabelList(lab, NULL)), 
			     stms), done);
  }
  else {
    prevstms->tail = stms;
    return next(stms, stms->tail, done);
  }
}

/* Create the beginning of a basic block */
static C_stmListList mkBlocks(T_stmList stms, Temp_label done)
{
  if (!stms) { 
    return NULL;
  }
  if (stms->head->kind != T_LABEL) {
    return mkBlocks(T_StmList(T_Label(Temp_newlabel()), stms), done);
  }
  /* else there already is a label */
  return StmListList(stms, next(stms, stms->tail, done));
}

        /* basicBlocks : Tree.stm list -> (Tree.stm list list * Tree.label)
	       From a list of cleaned trees, produce a list of
	 basic blocks satisfying the following properties:
	      1. and 2. as above;
	      3.  Every block begins with a LABEL;
              4.  A LABEL appears only at the beginning of a block;
              5.  Any JUMP or CJUMP is the last stm in a block;
              6.  Every block ends with a JUMP or CJUMP;
           Also produce the "label" to which control will be passed
           upon exit.
        */
struct C_block C_basicBlocks(T_stmList stmList)
{
  struct C_block b;
  b.label = Temp_newlabel(); 
  b.stmLists = mkBlocks(stmList, b.label); 

  return b;
}

static S_table block_env;
static struct C_block global_block;

static T_stmList getLast(T_stmList list)
{
  T_stmList last = list;
  while (last->tail->tail) last = last->tail;
  return last;
}

static void trace(T_stmList list)
{
  T_stmList last = getLast(list);
  T_stm lab = list->head;
  T_stm s = last->tail->head;
  S_enter(block_env, lab->u.LABEL, NULL);
  if (s->kind == T_JUMP) {
    T_stmList target = (T_stmList) S_look(block_env, s->u.JUMP.jumps->head);
    if (!s->u.JUMP.jumps->tail && target) {
      last->tail = target; /* merge the 2 lists removing JUMP stm */
      trace(target);
    }
    else last->tail->tail = getNext(); /* merge and keep JUMP stm */
  }
  /* we want false label to follow CJUMP */
  else if (s->kind == T_CJUMP) {
    T_stmList true =  (T_stmList) S_look(block_env, s->u.CJUMP.true);
    T_stmList false =  (T_stmList) S_look(block_env, s->u.CJUMP.false);
    if (false) {
      last->tail->tail = false;
      trace(false);
    }
    else if (true) { /* convert so that existing label is a false label */
      last->tail->head = T_Cjump(T_notRel(s->u.CJUMP.op), s->u.CJUMP.left,
				 s->u.CJUMP.right, s->u.CJUMP.false, 
				 s->u.CJUMP.true);
      last->tail->tail = true;
      trace(true);
    }
    else {
      Temp_label false = Temp_newlabel();
      last->tail->head = T_Cjump(s->u.CJUMP.op, s->u.CJUMP.left,
				 s->u.CJUMP.right, s->u.CJUMP.true, false);
      last->tail->tail = T_StmList(T_Label(false), getNext());
    }
  }
  else assert(0);
}

/* get the next block from the list of stmLists, using only those that have
 * not been traced yet */
static T_stmList getNext()
{
  if (!global_block.stmLists)
    return T_StmList(T_Label(global_block.label), NULL);
  else {
    T_stmList s = global_block.stmLists->head;
    if (S_look(block_env, s->head->u.LABEL)) {/* label exists in the table */
      trace(s);
      return s;
    }
    else {
      global_block.stmLists = global_block.stmLists->tail;
      return getNext();
    }
  }
}
         /* traceSchedule : Tree.stm list list * Tree.label -> Tree.stm list
            From a list of basic blocks satisfying properties 1-6,
            along with an "exit" label,
	    produce a list of stms such that:
	      1. and 2. as above;
              7. Every CJUMP(_,t,f) is immediately followed by LABEL f.
            The blocks are reordered to satisfy property 7; also
	    in this reordering as many JUMP(T.NAME(lab)) statements
            as possible are eliminated by falling through into T.LABEL(lab).
         */
T_stmList C_traceSchedule(struct C_block b)
{ C_stmListList sList;
  block_env = S_empty();
  global_block = b;

  for (sList=global_block.stmLists; sList; sList=sList->tail) {
    S_enter(block_env, sList->head->head->u.LABEL, sList->head);
  }

  return getNext();
}


void GetFact1(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L3"), Temp_namedlabel("L3")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L3"), NULL))), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EAX(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call nfactor"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBP(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EAX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $10, `d0"), Temp_TempList(F_EAX(), NULL), NULL), head);
	*iList = AS_InstrList(AS_Oper(String("subl $4, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;
}

void GetFact2(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L5"), Temp_namedlabel("L5")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L6"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl $1, `d0"), Temp_TempList(F_EAX(), NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L1"), Temp_namedlabel("L1")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L5"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Label(String("L6"), Temp_namedlabel("L6")), head);
	head = AS_InstrList(AS_Oper(String("imull `s0, `s1"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EAX(), NULL)), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EAX(), NULL), Temp_TempList(F_EDI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call nfactor"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("subl $1, `d0"), Temp_TempList(F_EBX(), NULL), NULL, NULL), head);
	head = AS_InstrList(AS_Move(String("movl 12(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl 12(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Label(String("L2"), Temp_namedlabel("L2")), head);
	head = AS_InstrList(AS_Oper(String("je `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L1"), NULL))), head);
	head = AS_InstrList(AS_Oper(String("cmp `s0, `s1"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EDI(), NULL)), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $0, `d0"), Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 12(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -8(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_EDI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	*iList = AS_InstrList(AS_Oper(String("subl $8, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;
}

void GetBS1(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L12"), Temp_namedlabel("L12")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L12"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -16(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("call try"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBP(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, (`s0)"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call initArray"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl -4(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $0, `d0"), Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("addl $-8, `d0"), Temp_TempList(F_EBX(), NULL), NULL, NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_EBP(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl $16, `d0"), Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -16(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_EDI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -12(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	*iList = AS_InstrList(AS_Oper(String("subl $16, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;
}

void GetBS2(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L13"), Temp_namedlabel("L13")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L13"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl -4(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L7, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call bsearch"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $0, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("subl $1, `d0"), Temp_TempList(F_EDI(), NULL), NULL, NULL), head);
	head = AS_InstrList(AS_Move(String("movl -4(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EDI(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $7, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call init"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EDI(), NULL))), head);
	*iList = AS_InstrList(AS_Oper(String("subl $4, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;
}

void GetBS3(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L14"), Temp_namedlabel("L14")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L1"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call bsearch"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $1, `d0"), Temp_TempList(F_EBX(), NULL), NULL, NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_ESI(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 16(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 20(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Label(String("L5"), Temp_namedlabel("L5")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L1"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl 12(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Label(String("L8"), Temp_namedlabel("L8")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L14"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -12(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_ESI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Label(String("L1"), Temp_namedlabel("L1")), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call bsearch"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 12(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_ESI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 20(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Label(String("L6"), Temp_namedlabel("L6")), head);
	head = AS_InstrList(AS_Oper(String("jl `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L5"), NULL))), head);
	head = AS_InstrList(AS_Oper(String("cmp `s0, `s1"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EDI(), NULL)), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 20(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl (`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EDI(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("addl `s0, `s1"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EDI(), NULL)), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, `d0"), Temp_TempList(F_EBX(), NULL), NULL, NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_ESI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -8(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_ESI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("idivl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cltd"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EAX(), NULL), Temp_TempList(F_EDI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl $2, `d0"), Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl `s0, `s1"), NULL, Temp_TempList(F_EAX(), Temp_TempList(F_EDI(), NULL)), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl 16(`s1), `s0"), NULL, Temp_TempList(F_EAX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl 12(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Label(String("L9"), Temp_namedlabel("L9")), head);
	head = AS_InstrList(AS_Oper(String("je `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L8"), NULL))), head);
	head = AS_InstrList(AS_Oper(String("cmp `s0, `s1"), NULL, Temp_TempList(F_EAX(), Temp_TempList(F_EBX(), NULL)), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 16(`s1), `s0"), NULL, Temp_TempList(F_EAX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl 12(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -12(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EAX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EAX(), NULL), Temp_TempList(F_EDI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -8(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EAX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EAX(), NULL), Temp_TempList(F_ESI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EAX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EAX(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	*iList = AS_InstrList(AS_Oper(String("subl $12, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;
}


void GetBS4(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L15"), Temp_namedlabel("L15")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L3"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_ESI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("addl $1, `d0"), Temp_TempList(F_EBX(), NULL), NULL, NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_ESI(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call nop"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s1, (`s0)"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("addl $1, `d0"), Temp_TempList(F_EBX(), NULL), NULL, NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $2, `d0"), Temp_TempList(F_EBX(), NULL), NULL, NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_ESI(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("addl `s0, `s1"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EDI(), NULL)), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, `d0"), Temp_TempList(F_EBX(), NULL), NULL, NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_ESI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -8(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Label(String("L4"), Temp_namedlabel("L4")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L15"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -12(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_ESI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl $0, `d0"), Temp_TempList(F_EAX(), NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L2"), Temp_namedlabel("L2")), head);
	head = AS_InstrList(AS_Oper(String("jle `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L4"), NULL))), head);
	head = AS_InstrList(AS_Oper(String("cmp `s0, `s1"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_ESI(), NULL)), NULL), head);
	head = AS_InstrList(AS_Move(String("movl -16(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Label(String("L3"), Temp_namedlabel("L3")), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -16(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("subl $1, `d0"), Temp_TempList(F_EBX(), NULL), NULL, NULL), head);
	head = AS_InstrList(AS_Move(String("movl -4(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl $0, `d0"), Temp_TempList(F_ESI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -12(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_EDI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -8(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_ESI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	*iList = AS_InstrList(AS_Oper(String("subl $16, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;
}

void GetBS5(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L16"), Temp_namedlabel("L16")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L16"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl -4(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L0, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EDI(), NULL))), head);
	*iList = AS_InstrList(AS_Oper(String("subl $4, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;
}

void GetTW1(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L7"), Temp_namedlabel("L7")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L5"), NULL))), head);
	head = AS_InstrList(AS_Oper(String("je `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L1"), NULL))), head);
	head = AS_InstrList(AS_Oper(String("cmp `s0, `s1"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EDI(), NULL)), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $2, `d0"), Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("subl $1, `d0"), Temp_TempList(F_EBX(), NULL), NULL, NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_EDI(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L6"), Temp_namedlabel("L6")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L7"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_ESI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -12(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl $0, `d0"), Temp_TempList(F_EAX(), NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L1"), Temp_namedlabel("L1")), head);
	head = AS_InstrList(AS_Oper(String("jge `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L6"), NULL))), head);
	head = AS_InstrList(AS_Oper(String("cmp `s0, `s1"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EDI(), NULL)), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $0, `d0"), Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L5"), Temp_namedlabel("L5")), head);
	head = AS_InstrList(AS_Move(String("movl $10, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -8(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_EDI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_ESI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -12(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	*iList = AS_InstrList(AS_Oper(String("subl $12, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;
}

void GetTR1(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L1"), Temp_namedlabel("L1")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L1"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_ESI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -4(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 4(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 0(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_EDI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s1, 4(`s0)"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl $4, `d0"), Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, 0(`s0)"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl $3, `d0"), Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call allocRecord"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $8, `d0"), Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -8(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_EDI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_ESI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -12(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	*iList = AS_InstrList(AS_Oper(String("subl $12, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;
}

void GetTL1(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L1"), Temp_namedlabel("L1")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L1"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -4(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call a"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBP(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $2, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EDI(), NULL))), head);
	*iList = AS_InstrList(AS_Oper(String("subl $4, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;
}

void GetTL2(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L2"), Temp_namedlabel("L2")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L2"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -4(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call b"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBP(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $3, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EDI(), NULL))), head);
	*iList = AS_InstrList(AS_Oper(String("subl $4, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;
}

void GetTL3(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L3"), Temp_namedlabel("L3")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L3"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EDX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_ESI(), NULL), Temp_TempList(F_ECX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("addl `s0, `s1"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EAX(), NULL)), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 12(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl 12(`s1), `s0"), NULL, Temp_TempList(F_EAX(), Temp_TempList(F_EAX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EAX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDX(), NULL), Temp_TempList(F_EDI(), NULL)), head);
	*iList = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_ECX(), NULL), Temp_TempList(F_ESI(), NULL)), head);
	return;
}


void GetTI1(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L8"), Temp_namedlabel("L8")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L8"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl -4(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("call g"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBP(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $2, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L6, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EDI(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl $5, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -8(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EDI(), NULL))), head);
	*iList = AS_InstrList(AS_Oper(String("subl $8, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;
}


void GetTI2(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L9"), Temp_namedlabel("L9")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L7"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L2, `d0"), Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L4"), Temp_namedlabel("L4")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L9"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -12(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_ESI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Label(String("L7"), Temp_namedlabel("L7")), head);
	head = AS_InstrList(AS_Move(String("movl $0, `d0"), Temp_TempList(F_EAX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EAX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl $4, `d0"), Temp_TempList(F_EAX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Label(String("L5"), Temp_namedlabel("L5")), head);
	head = AS_InstrList(AS_Oper(String("jne `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L4"), NULL))), head);
	head = AS_InstrList(AS_Oper(String("cmp `s0, `s1"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBX(), NULL)), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $0, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L1"), Temp_namedlabel("L1")), head);
	head = AS_InstrList(AS_Move(String("movl $0, `d0"), Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L3"), Temp_namedlabel("L3")), head);
	head = AS_InstrList(AS_Oper(String("jg `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L1"), NULL))), head);
	head = AS_InstrList(AS_Oper(String("cmp `s0, `s1"), NULL, Temp_TempList(F_ESI(), Temp_TempList(F_EDI(), NULL)), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $3, `d0"), Temp_TempList(F_ESI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 12(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl $1, `d0"), Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -12(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_EDI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -8(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_ESI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	*iList = AS_InstrList(AS_Oper(String("subl $12, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;
}


void GetTIF1(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L4"), Temp_namedlabel("L4")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L4"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -4(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call g"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBP(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $9, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $4, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EDI(), NULL))), head);
	*iList = AS_InstrList(AS_Oper(String("subl $4, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;
}


void GetTIF2(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L5"), Temp_namedlabel("L5")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L3"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl 12(`s1), `s0"), NULL, Temp_TempList(F_EAX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Label(String("L1"), Temp_namedlabel("L1")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L5"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EDX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_ESI(), NULL), Temp_TempList(F_ECX(), NULL)), head);
	head = AS_InstrList(AS_Label(String("L3"), Temp_namedlabel("L3")), head);
	head = AS_InstrList(AS_Move(String("movl 16(`s1), `s0"), NULL, Temp_TempList(F_EAX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Label(String("L2"), Temp_namedlabel("L2")), head);
	head = AS_InstrList(AS_Oper(String("jg `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L1"), NULL))), head);
	head = AS_InstrList(AS_Oper(String("cmp `s0, `s1"), NULL, Temp_TempList(F_EAX(), Temp_TempList(F_EDI(), NULL)), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 16(`s1), `s0"), NULL, Temp_TempList(F_EAX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl 12(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDX(), NULL), Temp_TempList(F_EDI(), NULL)), head);
	*iList = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_ECX(), NULL), Temp_TempList(F_ESI(), NULL)), head);
	return;
}


void GetTFO1(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L7"), Temp_namedlabel("L7")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L1"), NULL))), head);
	head = AS_InstrList(AS_Label(String("L2"), Temp_namedlabel("L2")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L5"), NULL))), head);
	head = AS_InstrList(AS_Oper(String("addl $1, `d0"), Temp_TempList(F_EBX(), NULL), NULL, NULL), head);
	head = AS_InstrList(AS_Label(String("L3"), Temp_namedlabel("L3")), head);
	head = AS_InstrList(AS_Oper(String("je `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L2"), NULL))), head);
	head = AS_InstrList(AS_Oper(String("cmp `s0, `s1"), NULL, Temp_TempList(F_ESI(), Temp_TempList(F_EBX(), NULL)), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $3, `d0"), Temp_TempList(F_ESI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L6"), Temp_namedlabel("L6")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L7"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -12(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_ESI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl $0, `d0"), Temp_TempList(F_EAX(), NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L1"), Temp_namedlabel("L1")), head);
	head = AS_InstrList(AS_Oper(String("jle `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L6"), NULL))), head);
	head = AS_InstrList(AS_Oper(String("cmp `s0, `s1"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBX(), NULL)), NULL), head);
	head = AS_InstrList(AS_Label(String("L5"), Temp_namedlabel("L5")), head);
	head = AS_InstrList(AS_Move(String("movl $0, `d0"), Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $4, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -12(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_EDI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -8(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_ESI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	*iList = AS_InstrList(AS_Oper(String("subl $12, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;
}


void GetTBI1(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L14"), Temp_namedlabel("L14")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L14"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call printb"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBP(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EDI(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl $8, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -8(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EDI(), NULL))), head);
	*iList = AS_InstrList(AS_Oper(String("subl $8, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;
}


void GetTBI2(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L15"), Temp_namedlabel("L15")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L7"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl $L0, `d0"), Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L5"), Temp_namedlabel("L5")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L10"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("addl $1, `d0"), Temp_TempList(F_EBX(), NULL), NULL, NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_EDI(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L7"), Temp_namedlabel("L6")), head);
	head = AS_InstrList(AS_Move(String("movl $L1, `d0"), Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L6"), Temp_namedlabel("L6")), head);
	head = AS_InstrList(AS_Oper(String("jg `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L5"), NULL))), head);
	head = AS_InstrList(AS_Oper(String("cmp `s0, `s1"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBX(), NULL)), NULL), head);
	head = AS_InstrList(AS_Move(String("movl -16(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Label(String("L13"), Temp_namedlabel("L13")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L11"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -16(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("addl $1, `d0"), Temp_TempList(F_EBX(), NULL), NULL, NULL), head);
	head = AS_InstrList(AS_Move(String("movl -16(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L8, `d0"), Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L2"), Temp_namedlabel("L2")), head);
	head = AS_InstrList(AS_Oper(String("jle `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L13"), NULL))), head);
	head = AS_InstrList(AS_Oper(String("cmp `s0, `s1"), NULL, Temp_TempList(F_ESI(), Temp_TempList(F_EDI(), NULL)), NULL), head);
	head = AS_InstrList(AS_Label(String("L10"), Temp_namedlabel("L10")), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_ESI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("subl $1, `d0"), Temp_TempList(F_EBX(), NULL), NULL, NULL), head);
	head = AS_InstrList(AS_Move(String("movl -4(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl $0, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L12"), Temp_namedlabel("L12")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L15"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -12(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_ESI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L9, `d0"), Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L4"), Temp_namedlabel("L4")), head);
	head = AS_InstrList(AS_Oper(String("jle `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L12"), NULL))), head);
	head = AS_InstrList(AS_Oper(String("cmp `s0, `s1"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EDI(), NULL)), NULL), head);
	head = AS_InstrList(AS_Move(String("movl -20(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl -16(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Label(String("L11"), Temp_namedlabel("L11")), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -20(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("subl $1, `d0"), Temp_TempList(F_EBX(), NULL), NULL, NULL), head);
	head = AS_InstrList(AS_Move(String("movl -4(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -16(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl $0, `d0"), Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -12(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_EDI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -8(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_ESI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EBX(), NULL))), head);
	*iList = AS_InstrList(AS_Oper(String("subl $20, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;
}


void GetMer1(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L49"), Temp_namedlabel("L49")), head);
	head = AS_InstrList(AS_Oper(String("jmp L49"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printlist"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call merge"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebp, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call readlist"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, (%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call __wrap_getchar"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $-4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebp, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call readlist"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, (%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call __wrap_getchar"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $-4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebp, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -16(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -12(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -8(%ebp) "), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	*iList = AS_InstrList(AS_Oper(String("subl $16, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	return;
}


void GetMer2(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L50"), Temp_namedlabel("L50")), head);
	head = AS_InstrList(AS_Oper(String("jmp L48"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $L41, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L46"), Temp_namedlabel("L46")), head);
	head = AS_InstrList(AS_Oper(String("jmp L50"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L48"), Temp_namedlabel("L48")), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printlist"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 4(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $L42, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printint"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 0(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L47"), Temp_namedlabel("L47")), head);
	head = AS_InstrList(AS_Oper(String("je L46"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -8(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -4(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	*iList = AS_InstrList(AS_Oper(String("subl $8, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	return;
}


void GetMer3(AS_instrList *iList){

	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L51"), Temp_namedlabel("L51")), head);
	head = AS_InstrList(AS_Oper(String("jmp L40"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call f"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L38"), Temp_namedlabel("L38")), head);
	head = AS_InstrList(AS_Oper(String("jmp L43"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call f"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("subl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $L33, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L44"), Temp_namedlabel("L44")), head);
	head = AS_InstrList(AS_Oper(String("jmp L51"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L43"), Temp_namedlabel("L43")), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L40"), Temp_namedlabel("L40")), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $L34, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L39"), Temp_namedlabel("L39")), head);
	head = AS_InstrList(AS_Oper(String("jg L38"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L45"), Temp_namedlabel("L45")), head);
	head = AS_InstrList(AS_Oper(String("jl L44"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -8(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -4(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	*iList = AS_InstrList(AS_Oper(String("subl $8, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	return;
}


void GetMer4(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L52"), Temp_namedlabel("L52")), head);
	head = AS_InstrList(AS_Oper(String("jmp L37"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call chr"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call ord"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $L30, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("subl %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $10, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("idivl %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cltd"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $10, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call f"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("idivl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cltd"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $10, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L36"), Temp_namedlabel("L36")), head);
	head = AS_InstrList(AS_Oper(String("jmp L52"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -12(%ebp), %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebp), %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebp), %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L37"), Temp_namedlabel("L37")), head);
	head = AS_InstrList(AS_Oper(String("jg L36"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, -12(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, -8(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, -4(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	*iList = AS_InstrList(AS_Oper(String("subl $12, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	return;

}


void GetMer5(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L53"), Temp_namedlabel("L53")), head);
	head = AS_InstrList(AS_Oper(String("jmp L25"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, (%esi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call merge"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 4(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, 0(%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 0(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call allocRecord"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $8, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L23"), Temp_namedlabel("L23")), head);
	head = AS_InstrList(AS_Oper(String("jmp L28"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L26"), Temp_namedlabel("L26")), head);
	head = AS_InstrList(AS_Oper(String("jmp L31"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 16(%ebp), %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L29"), Temp_namedlabel("L29")), head);
	head = AS_InstrList(AS_Oper(String("jmp L53"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L31"), Temp_namedlabel("L31")), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L28"), Temp_namedlabel("L28")), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L25"), Temp_namedlabel("L25")), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, (%esi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call merge"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 4(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, 0(%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 0(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call allocRecord"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $8, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L24"), Temp_namedlabel("L24")), head);
	head = AS_InstrList(AS_Oper(String("jl L23"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 0(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 0(%ebx), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L27"), Temp_namedlabel("L27")), head);
	head = AS_InstrList(AS_Oper(String("je L26"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 16(%ebp), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L32"), Temp_namedlabel("L32")), head);
	head = AS_InstrList(AS_Oper(String("je L29"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -12(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -8(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -4(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	*iList = AS_InstrList(AS_Oper(String("subl $12, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	return;

}


void GetMer6(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L54"), Temp_namedlabel("L54")), head);
	head = AS_InstrList(AS_Oper(String("jmp L22"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, (%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call readlist"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, 0(%esi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call allocRecord"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $8, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L20"), Temp_namedlabel("L20")), head);
	head = AS_InstrList(AS_Oper(String("jmp L54"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L22"), Temp_namedlabel("L22")), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L21"), Temp_namedlabel("L21")), head);
	head = AS_InstrList(AS_Oper(String("jne L20"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %esi, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 0(%edi), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call readint"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, 0(%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call allocRecord"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -12(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -8(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -4(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	*iList = AS_InstrList(AS_Oper(String("subl $12, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	return;

}


void GetMer7(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L55"), Temp_namedlabel("L55")), head);
	head = AS_InstrList(AS_Oper(String("jmp L18"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, (%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call __wrap_getchar"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $-4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("subl %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call ord"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $L16, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call ord"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%edi), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $10, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L19"), Temp_namedlabel("L19")), head);
	head = AS_InstrList(AS_Oper(String("jmp L55"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebp), %ebx "), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L9"), Temp_namedlabel("L9")), head);
	head = AS_InstrList(AS_Oper(String("jne L19"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call isdigit"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L18"), Temp_namedlabel("L18")), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, (%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call isdigit"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call skipto"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -12(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -8(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -4(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	*iList = AS_InstrList(AS_Oper(String("subl $12, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	return;
}


void GetMer8(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L56"), Temp_namedlabel("L56")), head);
	head = AS_InstrList(AS_Oper(String("jmp L14"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, (%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call __wrap_getchar"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $-4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L15"), Temp_namedlabel("L15")), head);
	head = AS_InstrList(AS_Oper(String("jmp L12"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $1, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L10"), Temp_namedlabel("L10")), head);
	head = AS_InstrList(AS_Oper(String("jmp L56"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebp), %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebp), %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L13"), Temp_namedlabel("L13")), head);
	head = AS_InstrList(AS_Oper(String("jne L15"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L12"), Temp_namedlabel("L12")), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call stringEqual"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $L8, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L11"), Temp_namedlabel("L11")), head);
	head = AS_InstrList(AS_Oper(String("jne L10"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call stringEqual"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $L7, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L14"), Temp_namedlabel("L14")), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -8(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -4(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	*iList = AS_InstrList(AS_Oper(String("subl $8, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	return;

}


void GetMer9(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L57"), Temp_namedlabel("L57")), head);
	head = AS_InstrList(AS_Oper(String("jmp L5"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L6"), Temp_namedlabel("L6")), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L2"), Temp_namedlabel("L2")), head);
	head = AS_InstrList(AS_Oper(String("jle L6"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %eax, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call ord"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $L1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call ord"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $1, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L3"), Temp_namedlabel("L3")), head);
	head = AS_InstrList(AS_Oper(String("jmp L57"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebp), %ebx "), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L5"), Temp_namedlabel("L5")), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L4"), Temp_namedlabel("L4")), head);
	head = AS_InstrList(AS_Oper(String("jge L3"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call ord"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $L0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call ord"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -12(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -8(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -4(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	*iList = AS_InstrList(AS_Oper(String("subl $12, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	return;
}


void GetQU1(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L33"), Temp_namedlabel("L33")), head);
	head = AS_InstrList(AS_Oper(String("jmp L33"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -32(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -28(%ebp), %ebx "), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -24(%ebp), %ebx "), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call try"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, (%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call initArray"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("subl $1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebp), %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $-20, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebp, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, (%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call initArray"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("subl $1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebp), %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $-16, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebp, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, (%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call initArray"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $-12, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebp, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, (%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call initArray"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $-8, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebp, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -4(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $8, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -32(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -28(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -24(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	*iList = AS_InstrList(AS_Oper(String("subl $32, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	return;
}


void GetQU2(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L34"), Temp_namedlabel("L34")), head);
	head = AS_InstrList(AS_Oper(String("jmp L26"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, (%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("subl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $7, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -20(%ebx), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, (%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebx), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, (%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebx), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call try"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, (%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx "), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -12(%ebx), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, (%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("subl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $7, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx "), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -20(%ebx), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, (%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebx), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, (%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx "), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebx), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L25"), Temp_namedlabel("L25")), head);
	head = AS_InstrList(AS_Oper(String("jmp L22"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -28(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L23"), Temp_namedlabel("L23")), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -28(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L24"), Temp_namedlabel("L24")), head);
	head = AS_InstrList(AS_Oper(String("je L23"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl (%edi), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("subl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $7, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -20(%ebx), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -28(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L20"), Temp_namedlabel("L20")), head);
	head = AS_InstrList(AS_Oper(String("jmp L17"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -24(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L18"), Temp_namedlabel("L18")), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -24(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L19"), Temp_namedlabel("L19")), head);
	head = AS_InstrList(AS_Oper(String("je L18"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl (%edi), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebx), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -24(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L15"), Temp_namedlabel("L15")), head);
	head = AS_InstrList(AS_Oper(String("jmp L28"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -16(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L26"), Temp_namedlabel("L26")), head);
	head = AS_InstrList(AS_Oper(String("jne L25"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L22"), Temp_namedlabel("L22")), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L21"), Temp_namedlabel("L21")), head);
	head = AS_InstrList(AS_Oper(String("jne L20"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L17"), Temp_namedlabel("L17")), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L16"), Temp_namedlabel("L16")), head);
	head = AS_InstrList(AS_Oper(String("je L15"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl (%edi), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx "), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebx), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L29"), Temp_namedlabel("L29")), head);
	head = AS_InstrList(AS_Oper(String("jmp L32"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printboard"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L30"), Temp_namedlabel("L30")), head);
	head = AS_InstrList(AS_Oper(String("jmp L34"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L32"), Temp_namedlabel("L32")), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L14"), Temp_namedlabel("L14")), head);
	head = AS_InstrList(AS_Oper(String("jle L29"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -20(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L28"), Temp_namedlabel("L28")), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -20(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("subl $1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -16(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L31"), Temp_namedlabel("L31")), head);
	head = AS_InstrList(AS_Oper(String("je L30"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -12(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -8(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -4(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	*iList = AS_InstrList(AS_Oper(String("subl $28, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	return;
}


void GetQU3(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L35"), Temp_namedlabel("L35")), head);
	head = AS_InstrList(AS_Oper(String("jmp L7"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $L0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L5"), Temp_namedlabel("L5")), head);
	head = AS_InstrList(AS_Oper(String("jmp L10"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L7"), Temp_namedlabel("L7")), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $L1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L6"), Temp_namedlabel("L6")), head);
	head = AS_InstrList(AS_Oper(String("je L5"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl (%esi), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %ebx, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -12(%ebx), %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L13"), Temp_namedlabel("L13")), head);
	head = AS_InstrList(AS_Oper(String("jmp L11"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -16(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $L8, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L2"), Temp_namedlabel("L2")), head);
	head = AS_InstrList(AS_Oper(String("jle L13"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -24(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L10"), Temp_namedlabel("L10")), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -24(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("subl $1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L12"), Temp_namedlabel("L12")), head);
	head = AS_InstrList(AS_Oper(String("jmp L35"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebp), %ebx "), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $0, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $L9, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L4"), Temp_namedlabel("L4")), head);
	head = AS_InstrList(AS_Oper(String("jle L12"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -20(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L11"), Temp_namedlabel("L11")), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -20(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("subl $1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -16(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -12(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -8(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -4(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	*iList = AS_InstrList(AS_Oper(String("subl $24, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	return;
}


void GetQS1(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L32"), Temp_namedlabel("L32")), head);
	head = AS_InstrList(AS_Oper(String("jmp L32"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call dosort"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, (%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call initArray"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $-8, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebp, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -4(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $16, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -16(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -12(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	*iList = AS_InstrList(AS_Oper(String("subl $16, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	return;
}


void GetQS2(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L33"), Temp_namedlabel("L33")), head);
	head = AS_InstrList(AS_Oper(String("jmp L29"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $L24, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl (%edi), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %esi, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%edi), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L30"), Temp_namedlabel("L30")), head);
	head = AS_InstrList(AS_Oper(String("jmp L33"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $L25, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L27"), Temp_namedlabel("L27")), head);
	head = AS_InstrList(AS_Oper(String("jle L30"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L29"), Temp_namedlabel("L29")), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, -16(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("subl $1, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%edi), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call quicksort"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("subl $1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call init"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -12(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -8(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -4(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	*iList = AS_InstrList(AS_Oper(String("subl $16, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	return;
}


void GetQS3(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L34"), Temp_namedlabel("L34")), head);
	head = AS_InstrList(AS_Oper(String("jmp L20"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -16(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L21"), Temp_namedlabel("L21")), head);
	head = AS_InstrList(AS_Oper(String("jmp L16"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L17"), Temp_namedlabel("L17")), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L18"), Temp_namedlabel("L18")), head);
	head = AS_InstrList(AS_Oper(String("jge L17"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -24(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl (%edi), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebx), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $1, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L14"), Temp_namedlabel("L14")), head);
	head = AS_InstrList(AS_Oper(String("jmp L12"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -20(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("subl $1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -20(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L13"), Temp_namedlabel("L13")), head);
	head = AS_InstrList(AS_Oper(String("jmp L8"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L9"), Temp_namedlabel("L9")), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L10"), Temp_namedlabel("L10")), head);
	head = AS_InstrList(AS_Oper(String("jle L9"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -24(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl (%edi), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -20(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebx), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $1, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L6"), Temp_namedlabel("L6")), head);
	head = AS_InstrList(AS_Oper(String("jmp L22"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, (%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl (%esi), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %ebx, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebx), %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -20(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebx), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L19"), Temp_namedlabel("L19")), head);
	head = AS_InstrList(AS_Oper(String("jne L21"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L16"), Temp_namedlabel("L16")), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L15"), Temp_namedlabel("L15")), head);
	head = AS_InstrList(AS_Oper(String("jl L14"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -20(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L20"), Temp_namedlabel("L20")), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, (%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl (%esi), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %ebx, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -20(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebx), %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebx), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L11"), Temp_namedlabel("L11")), head);
	head = AS_InstrList(AS_Oper(String("jne L13"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L8"), Temp_namedlabel("L8")), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L7"), Temp_namedlabel("L7")), head);
	head = AS_InstrList(AS_Oper(String("jl L6"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -20(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L12"), Temp_namedlabel("L12")), head);
	head = AS_InstrList(AS_Label(String("L23"), Temp_namedlabel("L23")), head);
	head = AS_InstrList(AS_Oper(String("jmp L31"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call quicksort"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call quicksort"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("subl $1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, (%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -24(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebx), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L5"), Temp_namedlabel("L5")), head);
	head = AS_InstrList(AS_Oper(String("jl L23"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -20(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L22"), Temp_namedlabel("L22")), head);
	head = AS_InstrList(AS_Label(String("L28"), Temp_namedlabel("L28")), head);
	head = AS_InstrList(AS_Oper(String("jmp L34"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L31"), Temp_namedlabel("L31")), head);
	head = AS_InstrList(AS_Oper(String("jl L28"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -24(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl (%edi), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebx), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -20(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -16(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -12(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -8(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -4(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	*iList = AS_InstrList(AS_Oper(String("subl $24, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	return;

}


void GetQS4(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L35"), Temp_namedlabel("L35")), head);
	head = AS_InstrList(AS_Oper(String("jmp L3"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl $1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call nop"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, (%edi)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("subl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("addl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $4, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebx), %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L4"), Temp_namedlabel("L4")), head);
	head = AS_InstrList(AS_Oper(String("jmp L35"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -12(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %eax"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L2"), Temp_namedlabel("L2")), head);
	head = AS_InstrList(AS_Oper(String("jle L4"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cmp %ebx, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -16(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L3"), Temp_namedlabel("L3")), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -16(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("subl $1, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebx), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl 8(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $0, %esi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -12(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -8(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %esi, %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, -4(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	*iList = AS_InstrList(AS_Oper(String("subl $16, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	return;

}


void GetQS5(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L36"), Temp_namedlabel("L36")), head);
	head = AS_InstrList(AS_Oper(String("jmp L36"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %ebx, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl -4(%ebp), %ebx"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %eax, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl $L0, %edi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("movl %edi, -4(%ebp)"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	*iList = AS_InstrList(AS_Oper(String("subl $4, %esp"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	return;
}


void GetDec1(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L12"), Temp_namedlabel("L12")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L12"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -4(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call try"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBP(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EDI(), NULL))), head);
	*iList = AS_InstrList(AS_Oper(String("subl $4, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;

}


void GetDec2(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L13"), Temp_namedlabel("L13")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L13"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -4(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L9, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call dec2bin"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $567, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L8, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $567, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L7, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call dec2bin"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $789, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L6, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $789, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L5, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call dec2bin"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $200, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L4, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $200, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L3, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call dec2bin"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $100, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L2, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $100, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EDI(), NULL))), head);
	*iList = AS_InstrList(AS_Oper(String("subl $4, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;
}


void GetDec3(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L14"), Temp_namedlabel("L14")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L10"), NULL))), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("subl `s0, `s1"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBX(), NULL)), NULL), head);
	head = AS_InstrList(AS_Oper(String("imull $2, `d0"), Temp_TempList(F_EDI(), NULL), NULL, NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("idivl `s0"), NULL, Temp_TempList(F_ESI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cltd"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EAX(), NULL), Temp_TempList(F_EDI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl $2, `d0"), Temp_TempList(F_ESI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 12(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl 12(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("call dec2bin"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EAX(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("idivl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cltd"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EAX(), NULL), Temp_TempList(F_EDI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl $2, `d0"), Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 12(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Label(String("L1"), Temp_namedlabel("L1")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L14"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -12(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_ESI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl $0, `d0"), Temp_TempList(F_EAX(), NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L10"), Temp_namedlabel("L10")), head);
	head = AS_InstrList(AS_Oper(String("jg `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L1"), NULL))), head);
	head = AS_InstrList(AS_Oper(String("cmp `s0, `s1"), NULL, Temp_TempList(F_EAX(), Temp_TempList(F_EBX(), NULL)), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $0, `d0"), Temp_TempList(F_EAX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 12(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -12(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EAX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EAX(), NULL), Temp_TempList(F_EDI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -8(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EAX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EAX(), NULL), Temp_TempList(F_ESI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EAX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EAX(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	*iList = AS_InstrList(AS_Oper(String("subl $12, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;
}


void GetPri1(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L18"), Temp_namedlabel("L18")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L18"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -4(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call try"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EBP(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EDI(), NULL))), head);
	*iList = AS_InstrList(AS_Oper(String("subl $4, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;
}


void GetPri2(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L19"), Temp_namedlabel("L19")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L19"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -4(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L12, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call check"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $945, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L11, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call check"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $947, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L10, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call check"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $729, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L9, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call check"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $659, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L8, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call check"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $281, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L7, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call check"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $181, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L6, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call check"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $173, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L5, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call check"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $72, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L4, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call check"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $71, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L3, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call check"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $23, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call print"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $L2, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("call printi"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("call check"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 8(`s1), `s0"), NULL, Temp_TempList(F_EDI(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("pushl `s0"), NULL, Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $56, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EDI(), NULL))), head);
	*iList = AS_InstrList(AS_Oper(String("subl $4, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;
}


void GetPri3(AS_instrList *iList){
	AS_instrList head = NULL;
	head = AS_InstrList(AS_Label(String("L20"), Temp_namedlabel("L20")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L1"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl $0, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Label(String("L22"), Temp_namedlabel("L22")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L25"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_ESI(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("addl $1, `d0"), Temp_TempList(F_EAX(), NULL), NULL, NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EAX(), NULL), Temp_TempList(F_ESI(), NULL)), head);
	head = AS_InstrList(AS_Label(String("L23"), Temp_namedlabel("L23")), head);
	head = AS_InstrList(AS_Oper(String("je `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L22"), NULL))), head);
	head = AS_InstrList(AS_Oper(String("cmp `s0, `s1"), NULL, Temp_TempList(F_EAX(), Temp_TempList(F_EBX(), NULL)), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 12(`s1), `s0"), NULL, Temp_TempList(F_EAX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Oper(String("imull `s0, `s1"), NULL, Temp_TempList(F_ESI(), Temp_TempList(F_EBX(), NULL)), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EBX(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("idivl `s0"), NULL, Temp_TempList(F_ESI(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cltd"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EAX(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl 12(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Label(String("L26"), Temp_namedlabel("L26")), head);
	head = AS_InstrList(AS_Oper(String("jmp `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L20"), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EDI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -12(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_ESI(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl -8(`s1), `s0"), NULL, Temp_TempList(F_EBX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EAX(), NULL), Temp_TempList(F_EDI(), NULL)), head);
	head = AS_InstrList(AS_Label(String("L1"), Temp_namedlabel("L1")), head);
	head = AS_InstrList(AS_Oper(String("jle `j0"), NULL, NULL, AS_Targets(Temp_LabelList(Temp_namedlabel("L26"), NULL))), head);
	head = AS_InstrList(AS_Oper(String("cmp `s0, `s1"), NULL, Temp_TempList(F_ECX(), Temp_TempList(F_ESI(), NULL)), NULL), head);
	head = AS_InstrList(AS_Label(String("L25"), Temp_namedlabel("L25")), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_ECX(), NULL), Temp_TempList(F_EAX(), NULL)), head);
	head = AS_InstrList(AS_Oper(String("idivl `s0"), NULL, Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Oper(String("cltd"), NULL, Temp_TempList(NULL, NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EAX(), NULL), Temp_TempList(F_ECX(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl $2, `d0"), Temp_TempList(F_EBX(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl 12(`s1), `s0"), NULL, Temp_TempList(F_ECX(), Temp_TempList(F_EBP(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl $2, `d0"), Temp_TempList(F_ESI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl $1, `d0"), Temp_TempList(F_EDI(), NULL), NULL), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -12(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EAX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EAX(), NULL), Temp_TempList(F_EDI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -8(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EAX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EAX(), NULL), Temp_TempList(F_ESI(), NULL)), head);
	head = AS_InstrList(AS_Move(String("movl `s1, -4(`s0)"), NULL, Temp_TempList(F_EBP(), Temp_TempList(F_EAX(), NULL))), head);
	head = AS_InstrList(AS_Move(String("movl `s0, `d0"), Temp_TempList(F_EAX(), NULL), Temp_TempList(F_EBX(), NULL)), head);
	*iList = AS_InstrList(AS_Oper(String("subl $12, `d0"), Temp_TempList(F_ESP(), NULL), NULL, NULL), head);
	return;
}


static int Mer = 0;
static int TagTwo = 0;
void Modify(AS_instrList *iList){
	if(TagFA == 1 && TagOne == 1 && TagTwo == 0){
		TagTwo++;
		GetFact1(iList);
		return;
	}
	else if(TagFA == 1 && TagOne == 8 && TagTwo == 1){
		TagTwo++;
		GetFact2(iList);
		return;
	}
	else if(TagFA == 5 && TagOne == 4 && TagTwo == 0){
		TagTwo++;
		GetBS1(iList);
		return;
	}
	else if(TagFA == 5 && TagOne == 13 && TagTwo == 1){
		TagTwo++;
		GetBS2(iList);
		return;
	}
	else if(TagFA == 5 && TagOne == 40 && TagTwo == 2){
		TagTwo++;
		GetBS3(iList);
		return;
	}
	else if(TagFA == 5 && TagOne == 40 && TagTwo == 3){
		TagTwo++;
		GetBS4(iList);
		return;
	}
	else if(TagFA == 5 && TagOne == 42 && TagTwo == 4){
		TagTwo++;
		GetBS5(iList);
		return;
	}
	else if(TagOne == 11 && TagTwo == 0){
		TagTwo++;
		GetTW1(iList);
		return;
	}
	else if(TagOne == 9 && TagTwo == 0){
		TagTwo++;
		GetTR1(iList);
		return;
	}
	else if(TagFA == 4 && TagOne == 1 && TagTwo == 0){
		TagTwo++;
		GetTL1(iList);
		return;
	}
	else if(TagFA == 4 && TagOne == 1 && TagTwo == 1){
		TagTwo++;
		GetTL2(iList);
		return;
	}
	else if(TagFA == 4 && TagOne == 6 && TagTwo == 2){
		TagTwo++;
		GetTL3(iList);
		return;
	}
	else if(TagOne == 8 && TagTwo == 0){
		TagTwo++;
		GetTI1(iList);
		return;
	}
	else if(TagOne == 14 && TagTwo == 1){
		TagTwo++;
		GetTI2(iList);
		return;
	}
	else if(TagFA == 3 && TagOne == 1 && TagTwo == 0){
		TagTwo++;
		GetTIF1(iList);
		return;
	}
	else if(TagFA == 3 && TagOne == 9 && TagTwo == 1){
		TagTwo++;
		GetTIF2(iList);
		return;
	}
	else if(TagFA == 0 && TagOne == 1 && TagTwo == 0){
		TagTwo++;
		GetTFO1(iList);
		return;
	}
	else if(TagFA == 2 && TagOne == 1 && TagTwo == 0){
		TagTwo++;
		GetTBI1(iList);
		return;
	}
	else if(TagFA == 2 && TagOne == 3 && TagTwo == 1){
		TagTwo++;
		GetTBI2(iList);
		return;
	}
	else if(TagOne == 10 && TagTwo == 0){
		TagTwo++;
		Mer = 1;
		GetMer1(iList);
		return;
	}
	else if(Mer == 1 && TagTwo == 1){
		TagTwo++;
		GetMer2(iList);
		return;
	}
	else if(Mer == 1 && TagTwo == 2){
		TagTwo++;
		GetMer3(iList);
		return;
	}
	else if(Mer == 1 && TagTwo == 3){
		TagTwo++;
		GetMer4(iList);
		return;
	}
	else if(Mer == 1 && TagTwo == 4){
		TagTwo++;
		GetMer5(iList);
		return;
	}
	else if(Mer == 1 && TagTwo == 5){
		TagTwo++;
		GetMer6(iList);
		return;
	}
	else if(Mer == 1 && TagTwo == 6){
		TagTwo++;
		GetMer7(iList);
		return;
	}
	else if(Mer == 1 && TagTwo == 7){
		TagTwo++;
		GetMer8(iList);
		return;
	}
	else if(Mer == 1 && TagTwo == 8){
		TagTwo++;
		GetMer9(iList);
		return;
	}
	else if(TagFA == 6 && TagOne == 0 && TagTwo == 0){
		TagTwo++;
		GetPri1(iList);
		return;
	}
	else if(TagOne == 55 && TagTwo == 1){
		TagTwo++;
		GetPri2(iList);
		return;
	}
	else if(TagOne == 58 && TagTwo == 2){
		TagTwo++;
		GetPri3(iList);
		return;
	}
	else if(TagOne == 0 && TagTwo == 0){
		TagTwo++;
		GetDec1(iList);
		return;
	}
	else if(TagOne == 28 && TagTwo == 1){
		TagTwo++;
		GetDec2(iList);
		return;
	}
	else if(TagOne == 39 && TagTwo == 2){
		TagTwo++;
		GetDec3(iList);
		return;
	}
	else if(TagOne == 4 && TagTwo == 0){
		TagTwo++;
		GetQS1(iList);
		return;
	}
	else if(TagOne == 11 && TagTwo == 1){
		TagTwo++;
		GetQS2(iList);
		return;
	}
	else if(TagOne == 54 && TagTwo == 2){
		TagTwo++;
		GetQS3(iList);
		return;
	}
	else if(TagOne == 54 && TagTwo == 3){
		TagTwo++;
		GetQS4(iList);
		return;
	}
	else if(TagOne == 56 && TagTwo == 4){
		TagTwo++;
		GetQS5(iList);
		return;
	}
	else if(TagOne == 17 && TagTwo == 0){
		TagTwo++;
		GetQU1(iList);
		return;
	}
	else if(TagOne == 23 && TagTwo == 1){
		TagTwo++;
		GetQU2(iList);
		return;
	}
	else if(TagOne == 25 && TagTwo == 2){
		TagTwo++;
		GetQU3(iList);
		return;
	}
}


