#include "semant.h"

static struct expty transVar(Tr_level, Tr_exp, S_table venv, S_table tenv, A_var v);
static struct expty transExp(Tr_level, Tr_exp, S_table venv, S_table tenv, A_exp e);
static Tr_exp transDec(Tr_level, Tr_exp, S_table venv, S_table tenv, A_dec d);
static Ty_ty transTy (S_table tenv, A_ty t);
static Ty_tyList makeFormalTyList(S_table t, A_fieldList p);
static Ty_ty actual_ty(Ty_ty ty);
static bool args_match(Tr_level, Tr_exp, S_table v, S_table t, A_expList el, Ty_tyList fl, A_exp fun); 
static bool ty_match(Ty_ty t1, Ty_ty t2);
static bool efields_match(Tr_level, Tr_exp, S_table v, S_table t, Ty_ty ty, A_exp e);
static Ty_fieldList makeFieldTys(S_table t, A_fieldList fs);
static U_boolList makeFormals(A_fieldList);

string immutable = NULL;
bool block = FALSE;

F_fragList SEM_transProg(A_exp exp){
	S_table t = E_base_tenv();
	S_table v = E_base_venv();
	transExp(Tr_outermost(), NULL, v, t, exp);
	Tr_procEntryExit(Tr_outermost(), Tr_nilExp(), NULL);
	F_fragList resl = Tr_getResult();
	return resl;
}

static struct expty transVar(Tr_level level, Tr_exp breakk, S_table venv, S_table tenv, A_var v) {
	if (!v) {
		return expTy(Tr_noExp(), Ty_Void());
	}
	E_enventry x;
	struct expty et,et2;
	Ty_fieldList fl;
	Tr_exp trans;

	switch (v->kind) {
	case A_simpleVar:/* var id (a)*/
		x = S_look(venv, v->u.simple);
		trans = Tr_noExp();
		if (x && x->kind == E_varEntry) {
			trans = Tr_simpleVar(x->u.var.access, level);
			return expTy(trans, actual_ty(x->u.var.ty));
		} else {
			EM_error(v->pos, "undefined variable %s", S_name(v->u.simple));
			return expTy(trans, Ty_Int());
		}
		break;
	case A_fieldVar:/* var record (a.b)*/
		et = transVar(level, breakk, venv, tenv, v->u.field.var);
		trans = Tr_noExp();
		if (et.ty->kind != Ty_record) {
			EM_error(v->pos, "not a record type");
			return expTy(trans, Ty_Record(NULL));
		} else {
			int i = 0;
			for (fl = et.ty->u.record; fl; fl = fl->tail) {
				if (fl->head->name == v->u.field.sym) {
					trans = Tr_fieldVar(et.exp, i);
					return expTy(trans, actual_ty(fl->head->ty));
				}
			}
			EM_error(v->pos, "field %s doesn't exist", S_name(v->u.field.sym));
		}
		return expTy(trans, Ty_Record(NULL));
		break;
	case A_subscriptVar:/*var array (a[b])*/ 
		et  = transVar(level, breakk, venv, tenv, v->u.subscript.var);
		trans = Tr_noExp();
		if (et.ty->kind != Ty_array) {
			EM_error(v->pos, "array type required");
		} else {
			et2 = transExp(level, breakk, venv, tenv, v->u.subscript.exp);
			if (et2.ty->kind != Ty_int) {
				EM_error(v->pos, "int required");
			} else {
				return expTy(trans, actual_ty(et.ty->u.array));
			}
		}
		return expTy(trans, Ty_Array(NULL));
	default:
		assert(0);
	}
}

static struct expty transExp(Tr_level level, Tr_exp breakk, S_table v, S_table t, A_exp e){
	if (!e) { 
		return expTy(Tr_noExp(), Ty_Void()); 
	}
	A_oper oper;
	struct expty left, right, final, final2, final3, final4, final5, lo, hi;
	A_expList list;
	A_decList decs;
	E_enventry callinfo;
	Ty_ty recty, arrayty;

	switch (e->kind) {
	case A_varExp:
		return transVar(level, breakk, v, t, e->u.var);
	case A_nilExp:
		return expTy(Tr_nilExp(), Ty_Nil());
	case A_callExp:
		callinfo = S_look(v, e->u.call.func); /*get params and return from tenv*/
		A_expList args = NULL;
		Tr_expList argList = NULL;
		for (args = e->u.call.args; args; args = args->tail) { /*memory args-info by order*/
			struct expty arg = transExp(level, breakk, v, t, args->head);
			Tr_expList_prepend(arg.exp, &argList);			
		}
		Tr_exp trans = Tr_noExp();
		if (callinfo && callinfo->kind == E_funEntry){
			trans = Tr_callExp(callinfo->u.fun.label, callinfo->u.fun.level, level, &argList);
			if (args_match(level, breakk, v, t, e->u.call.args, callinfo->u.fun.formals, e)) {/*check params is matched*/
				if (callinfo->u.fun.result) {
					return expTy(trans, actual_ty(callinfo->u.fun.result));
				} else {
					return expTy(trans, Ty_Void());
				}
			} 
		} else {
			EM_error(e->pos, "undefined function %s\n", S_name(e->u.call.func));
		}
		return expTy(trans, Ty_Void());
	case A_recordExp:
		recty = actual_ty(S_look(t, e->u.record.typ));
	    if (!recty) { /*cant find record-type in table tenv*/ 
			EM_error(e->pos, "undefined type %s (debug recordExp)", S_name(e->u.record.typ)); 
		} else {
			if (recty->kind != Ty_record){
				EM_error(e->pos, "%s is not a record type", S_name(e->u.record.typ));	
				return expTy(Tr_noExp(), Ty_Record(NULL));
			}
			if (efields_match(level, breakk, v, t, recty, e)) {/*check record field is matched*/
				Tr_expList l = NULL;
				int n = 0;
				A_efieldList el;
				for (el = e->u.record.fields; el; el = el->tail, n++) {
					struct expty val = transExp(level, breakk, v, t, el->head->exp);	
					Tr_expList_prepend(val.exp, &l);
				}				
				return expTy(Tr_recordExp(n, l), recty);
			}
		}
		return expTy(Tr_noExp(), Ty_Record(NULL));
	case A_arrayExp:
		arrayty = actual_ty(S_look(t, e->u.array.typ));
		if (!arrayty) {
			EM_error(e->pos, "undeined array type %s", S_name(e->u.array.typ));
			return expTy(Tr_noExp(), Ty_Array(NULL));
		}
		if (arrayty->kind != Ty_array) {
			EM_error(e->pos, "%s is not a array type", S_name(e->u.array.typ));
			return expTy(Tr_noExp(), Ty_Array(NULL));
		}
	    final2 = transExp(level, breakk, v, t, e->u.array.size);
		final3 = transExp(level, breakk, v, t, e->u.array.init);
		if (final2.ty->kind != Ty_int) {
			EM_error(e->pos, "array size should be int %s", S_name(e->u.array.typ));
		} else if (!ty_match(final3.ty, arrayty->u.array)){
			EM_error(e->pos, "type mismatch");
		} else {
			return expTy(Tr_arrayExp(final2.exp, final3.exp), arrayty);
		}
		return expTy(Tr_noExp(), Ty_Array(NULL));
	case A_seqExp:
		{
		Tr_expList l = NULL;
		list = e->u.seq;
		struct expty seqone;
		if (!list) {
			return expTy(Tr_noExp(), Ty_Void());
		}
		while (list) {
			seqone = transExp(level, breakk, v, t, list->head);
			Tr_expList_prepend(seqone.exp, &l);
			list = list->tail;
		}
		return expTy(Tr_seqExp(l), seqone.ty);
		}
	case A_whileExp:
		final = transExp(level, breakk, v, t, e->u.whilee.test);
		if (final.ty->kind != Ty_int) {
			EM_error(e->pos, "int required");
		}    
		Tr_exp done = Tr_doneExp();
		struct expty body = transExp(level, breakk, v, t, e->u.whilee.body);
		// prevent dead loop
		if(body.ty->kind == Ty_int) {
			EM_error(e->u.whilee.body->pos, "while body must produce no value");
		}
		return expTy(Tr_whileExp(final.exp, body.exp, done), Ty_Void());
	case A_assignExp:
		final4 = transVar(level, breakk, v, t, e->u.assign.var);
		final5 = transExp(level, breakk, v, t, e->u.assign.exp);
		if (final4.ty && !ty_match(final4.ty, final5.ty)) {
			EM_error(e->pos, "unmatched assign exp");
		}
		if(e->u.assign.var->kind == A_simpleVar) {
			if(immutable && !strcmp(S_name(e->u.assign.var->u.simple), immutable)){
				EM_error(e->pos, "loop variable can't be assigned");
				immutable = NULL;
			}
		}
		return expTy(Tr_assignExp(final4.exp, final5.exp), Ty_Void());
	case A_breakExp:
		if (!breakk){
			return expTy(Tr_noExp(), Ty_Void());
		}
		return expTy(Tr_breakExp(breakk), Ty_Void());
	case A_forExp:
		{
		struct expty lo = transExp(level, breakk, v, t, e->u.forr.lo);
		struct expty hi = transExp(level, breakk, v, t, e->u.forr.hi);
		struct expty body;

		if (lo.ty != Ty_Int() || hi.ty != Ty_Int()) {
			EM_error(e->pos, "for exp's range type is not integer");
		}

		S_beginScope(v);
		immutable = S_name(e->u.forr.var);
		transDec(level, breakk, v, t, A_VarDec(e->pos, e->u.forr.var, S_Symbol("int"), e->u.forr.lo));
		body = transExp(level, breakk, v, t, e->u.forr.body);
		S_endScope(v);
		return expTy(Tr_noExp(), Ty_Void());
		}
	case A_letExp:
		{
		A_decList decs;
		Tr_expList l = NULL;
		S_beginScope(v);
		S_beginScope(t);
		for (decs = e->u.let.decs; decs; decs = decs->tail) {
			Tr_expList_prepend(transDec(level, breakk, v, t, decs->head), &l);
		}
		final = transExp(level, breakk, v, t, e->u.let.body);
		Tr_expList_prepend(final.exp, &l);
		S_endScope(v);
		S_endScope(t);
		return expTy(Tr_seqExp(l), final.ty);
		}
	case A_opExp:
		oper = e->u.op.oper;
		left  = transExp(level, breakk, v, t, e->u.op.left); 
		right = transExp(level, breakk, v, t, e->u.op.right);
		if (0 <= oper && oper < 4) {/* check +,-,*,/ */
			if (left.ty->kind != Ty_int){
				EM_error(e->u.op.left->pos, "integer required");	
			}
			else if (right.ty->kind != Ty_int) {
				EM_error(e->u.op.right->pos, "integer required");	
			}	
			else{
				return expTy(Tr_arithExp(oper, left.exp, right.exp), Ty_Int());
			}
		} else if (3 < oper && oper < 10) {
			Tr_exp translation = Tr_noExp();
			if (oper == 4 || oper == 5) {/*check record type can be nil(=, <>)*/
				switch(left.ty->kind) {
				case Ty_int:
					if (right.ty->kind == Ty_int) translation = Tr_eqExp(oper, left.exp, right.exp);
					else {EM_error(e->u.op.right->pos, "unexpected type in comparsion");}
					break;
				case Ty_string:
					if (ty_match(right.ty, left.ty)) translation = Tr_eqStringExp(oper, left.exp, right.exp);
					else {EM_error(e->u.op.right->pos, "unexpected type in comparsion");}
					break;
				case Ty_array:
					if (ty_match(right.ty, left.ty)) translation = Tr_eqRef(oper, left.exp, right.exp);
					else {EM_error(e->u.op.right->pos, "unexpected type in comparsion");}
				    break;
				case Ty_record:
					if (ty_match(right.ty, left.ty) || right.ty->kind == Ty_nil) translation = Tr_eqRef(oper, left.exp, right.exp);
					else {EM_error(e->u.op.right->pos, "unexpected type in comparsion");}
					break;
				default:
					EM_error(e->u.op.right->pos, "unexpected expression in comparsion");
				}
				return expTy(translation, Ty_Int());
			} else {
				switch(left.ty->kind) {
				case Ty_int:
					if (right.ty->kind == Ty_int) translation = Tr_relExp(oper, left.exp, right.exp); 
					else {EM_error(e->u.op.right->pos, "unexpected type in comparsion");}
					break;
				case Ty_string:
					if (right.ty->kind == Ty_string) translation = Tr_eqStringExp(oper, left.exp, right.exp);
					else {EM_error(e->u.op.right->pos, "unexpected type in comparsion");}
					break;
				default:
					EM_error(e->u.op.right->pos, "unexpected type in comparsion");
				}
				return expTy(translation, Ty_Int());
			}
		} 
		return expTy(Tr_noExp(), Ty_Int());
	case A_ifExp:
		final = transExp(level, breakk, v, t, e->u.iff.test);
		final2 = transExp(level, breakk, v, t, e->u.iff.then);
		if(e->u.iff.test->kind == A_ifExp){
			final3 = final;
			final = final2;
			final2 = final3;
		}
		final3 = expTy(Tr_nilExp(), Ty_Nil());
		if ((e->u.iff.elsee)->kind != A_nilExp) { /*no else-part*/
			final3 = transExp(level, breakk, v, t, e->u.iff.elsee);
			if (final.ty->kind != Ty_int){
				EM_error(e->u.iff.test->pos, "int required");
			} 
			else if(!ty_match(final2.ty, final3.ty)) {
				EM_error(e->pos, "then exp and else exp type mismatch");
			}
		}
		/*else{
			if(final2.ty->kind != Ty_void){
				EM_error(e->u.iff.then->pos, "if-then exp's body must produce no value");
			}
		}*/
		return expTy(Tr_ifExp(final.exp, final2.exp, final3.exp), final2.ty);
	case A_stringExp:
		if(block){
			return expTy(Tr_noExp(), Ty_String());
		}
		else{
			return expTy(Tr_stringExp(e->u.stringg), Ty_String());
		}
	case A_intExp:
		return expTy(Tr_intExp(e->u.intt), Ty_Int());
	default:
		assert(0);
	}
}

static Tr_exp transDec(Tr_level level, Tr_exp breakk, S_table v, S_table t, A_dec d) {
	struct expty final;
	A_fundec f;
	Ty_ty resTy, namety;
	Ty_tyList formalTys, s;
	A_fieldList l;
	A_nametyList nl;
	A_fundecList fcl;
	E_enventry fun;
	int iscyl;
	A_fundec prevfun = NULL;
	Tr_access ac;

	switch (d->kind) {
	case A_varDec:
		final = transExp(level, breakk, v, t, d->u.var.init);
		ac = Tr_allocLocal(level, d->u.var.escape);	
		if (!d->u.var.typ) {/*unpoint type*/
			if (final.ty->kind == Ty_nil) {
				EM_error(d->pos, "init should not be nil without type specified");
				S_enter(v, d->u.var.var, E_VarEntry(ac, Ty_Int()));
			} else {
				S_enter(v, d->u.var.var, E_VarEntry(ac, final.ty));
			}
		} else {
			resTy = S_look(t, d->u.var.typ);
			if (!resTy) {
				EM_error(d->pos, "undifined type %s", S_name(d->u.var.typ));
			} else {
				if (!ty_match(resTy, final.ty)) {
					EM_error(d->pos, "type mismatch");
					S_enter(v, d->u.var.var, E_VarEntry(ac, resTy));
				} else {
					S_enter(v, d->u.var.var, E_VarEntry(ac, resTy));
				}
			}
			
		}
		return Tr_assignExp(Tr_simpleVar(ac, level), final.exp);
		break;
	case A_functionDec:
		for (fcl = d->u.function; fcl; fcl = fcl->tail) {
			//fragList = F_FragList(F_StringFrag(NULL, NULL), fragList);
			if (fcl->head->result) {
				resTy = S_look(t, fcl->head->result);
				if (!resTy) {
					EM_error(fcl->head->pos, "undefined type for return type");
					resTy = Ty_Void();
				} 
			} else {
				resTy = Ty_Void();
			}
			formalTys = makeFormalTyList(t, fcl->head->params);
			
			if(prevfun && !strcmp(S_name(fcl->head->name), S_name(prevfun->name))){
				EM_error(prevfun->pos, "two functions have the same name");
		  	}
			Temp_label funLabel = Temp_newlabel();
			Tr_level l = Tr_newLevel(level, funLabel, makeFormals(fcl->head->params));/* create a new level */
			S_enter(v, fcl->head->name, E_FunEntry(l, funLabel, formalTys, resTy));
			prevfun = fcl->head;
		}

		for (fcl = d->u.function; fcl; fcl = fcl->tail) {
			f = fcl->head;
			E_enventry funEntry = S_look(v, f->name);
			S_beginScope(v);
			formalTys = funEntry->u.fun.formals;
			Tr_accessList acls = Tr_formals(funEntry->u.fun.level);
			for (l = f->params, s = formalTys; l && s; l = l->tail, s = s->tail) {
				S_enter(v, l->head->name, E_VarEntry(acls->head, s->head));
			}
			final = transExp(funEntry->u.fun.level, breakk, v, t, f->body);
			fun = S_look(v, f->name);
			if(fun->u.fun.result->kind == Ty_void && final.ty->kind != Ty_void){
				EM_error(f->body->pos, "procedure returns value");
			}
			/*else if (!ty_match(fun->u.fun.result, final.ty)) {
				EM_error(f->pos, "incorrect return type in function '%s'", S_name(f->name));
			}*/
			Tr_procEntryExit(funEntry->u.fun.level, final.exp, acls);
			S_endScope(v);
		}
		return Tr_noExp();
		break;
	case A_typeDec:
		for (nl = d->u.type; nl; nl = nl->tail) {
			S_enter(t, nl->head->name, Ty_Name(nl->head->name,NULL));
		} /* add name to tenv, Ty_ty set NULL*/
		A_nametyList nList = d->u.type;
		A_namety prev = NULL;
		while (nList) {
			if(prev && !strcmp(S_name(nList->head->name),S_name(prev->name))) {
				EM_error(prev->ty->pos,"two types have the same name");
			}
			S_enter(t, nList->head->name, Ty_Name(nList->head->ty->u.name, NULL));
			prev = nList->head;
			nList = nList->tail;
		}
		iscyl = TRUE;
		for (nl = d->u.type; nl; nl = nl->tail) {
			resTy = transTy(t, nl->head->ty);
			if (iscyl) {
				if (resTy->kind != Ty_name) {
					iscyl = FALSE;
				}
			}
			namety = S_look(t, nl->head->name);
			namety->u.name.ty = resTy;
		}
		if (iscyl) EM_error(d->pos,"illegal type cycle: cycle must contain record, array");
		return Tr_noExp();
		break;
	default:
		assert(0);
	}
}

static Ty_ty transTy(S_table tb, A_ty ty) {
	Ty_ty final = NULL;
	Ty_fieldList fieldTys;

	switch (ty->kind) {
	case A_nameTy:
		final = S_look(tb, ty->u.name);
		if (!final) {
			EM_error(ty->pos, "undefined type %s", S_name(ty->u.name));
			return Ty_Int();
		}
		return final;
	case A_recordTy:
		fieldTys = makeFieldTys(tb, ty->u.record);
		return Ty_Record(fieldTys);
	case A_arrayTy:
		final = S_look(tb, ty->u.array);
		if (!final) EM_error(ty->pos, "undefined type %s", S_name(ty->u.array));
		return Ty_Array(final);
	default:
		assert(0);
	}
}

static Ty_fieldList makeFieldTys(S_table t, A_fieldList fs) {
	A_fieldList f;
	Ty_fieldList tys = NULL, head;
	Ty_ty ty;
	Ty_field tmp;

	for (f = fs; f; f = f->tail) {
		ty = S_look(t, f->head->typ);
		if (!ty) {
			EM_error(f->head->pos, "undefined type %s", S_name(f->head->typ));
		} else {
		tmp = Ty_Field(f->head->name, ty);
		if (tys) {
			tys->tail = Ty_FieldList(tmp, NULL);
			tys = tys->tail;
		} else {
			tys = Ty_FieldList(tmp, NULL);
			head = tys;
		}

		}
	}
	return head;
}

static Ty_tyList makeFormalTyList(S_table t, A_fieldList fl) {
	Ty_tyList final = NULL, head = final;
	A_fieldList l = fl;
	Ty_ty ty;

	for (; l; l = l->tail) {
		ty = S_look(t, l->head->typ);
		if(!ty) {
			EM_error(l->head->pos, "undefined type %s", S_name(l->head->typ));
			ty = Ty_Int();
		}
		if (!final) {
			final = Ty_TyList(ty, NULL);
			head = final;
		} else {
			final->tail = Ty_TyList(ty, NULL);
			final = final->tail;
		}
	}
	return head;
}

static Ty_ty actual_ty(Ty_ty ty){
	if (!ty){
        return ty;
    }
	if (ty->kind == Ty_name){
        return actual_ty(ty->u.name.ty);
    }
	else{
        return ty;
    }
}

static bool args_match(Tr_level level, Tr_exp breakk, S_table v, S_table tt, A_expList ell, Ty_tyList fll, A_exp fun) {
	block = TRUE;
	struct expty t;
	A_expList el = ell;
	Ty_tyList fl = fll;

	while (el && fl) {
		t = transExp(level, breakk, v, tt, el->head);
		if (!ty_match(t.ty, fl->head)){
			EM_error(fun->pos, "para type mismatch");
			block = FALSE;
			return FALSE;
		}
		el = el->tail;
		fl = fl->tail;
	}
	if (el && !fl) {
		EM_error(fun->pos,"too many params in function %s\n", S_name(fun->u.call.func));
		block = FALSE;
		return FALSE;
	} else if (!el && fl) {
		EM_error(fun->pos, "less params in function %s\n", S_name(fun->u.call.func));
		block = FALSE;
		return FALSE;
	} else {
		block = FALSE;
		return TRUE;
	}
}

static bool ty_match(Ty_ty tt, Ty_ty ee) {
	Ty_ty t = actual_ty(tt);
	Ty_ty e = actual_ty(ee);
	int tk = t->kind;
	int ek = e->kind;

    if((tk == Ty_record || tk == Ty_array) && t == e){
        return TRUE;
    }
    else if(tk == Ty_record && ek == Ty_nil){
        return TRUE;
    }
    else if(ek == Ty_record && tk == Ty_nil){
        return TRUE;
    }
    else if(tk != Ty_record && tk != Ty_array && tk == ek){
        return TRUE;
    }
    else{
        return FALSE;
    }
}

static bool efields_match(Tr_level level, Tr_exp breakk, S_table v, S_table t, Ty_ty tyy, A_exp e) {
	block = TRUE;
	struct expty et;
	Ty_fieldList ty = tyy->u.record;
	A_efieldList fl = e->u.record.fields;
	while (ty && fl) {
		et = transExp(level, breakk, v, t, fl->head->exp);
		if (!(ty->head->name == fl->head->name) || !ty_match(ty->head->ty, et.ty)){
			EM_error(e->pos, "unmatched name: type in %s", S_name(e->u.record.typ));
			block = FALSE;
			return FALSE;
		}
		ty = ty->tail;
		fl = fl->tail;
	}
	if (ty && !fl) {
		EM_error(e->pos, "less fields in %s", S_name(e->u.record.typ));
		block = FALSE;
		return FALSE;
	} else if (!ty && fl) {
		EM_error(e->pos, "too many field in %s", S_name(e->u.record.typ));
		block = FALSE;
		return FALSE;
	}
	
	block = FALSE;
	return TRUE;
}
static U_boolList makeFormals(A_fieldList params) {
	/* HACK (short escape-var judge) default all escape-var */
	U_boolList head = NULL, tail = NULL;
	A_fieldList p = params;
	for (; p; p = p->tail) {
		if (head) {
			tail->tail = U_BoolList(TRUE, NULL);
			tail = tail->tail;
		} else {
			head = U_BoolList(TRUE, NULL);
			tail = head;
		}
	}
	return head;
}

struct expty expTy(Tr_exp e, Ty_ty t) {
	struct expty et;
	et.exp = e;
	et.ty  = t;
	return et;
}
