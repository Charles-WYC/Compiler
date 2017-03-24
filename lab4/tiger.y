%{
#include <stdio.h>
#include <stdlib.h>
#include "util.h"
#include "symbol.h" 
#include "errormsg.h"
#include "absyn.h"

int yylex(void); /* function prototype */

A_exp absyn_root;

void yyerror(char *s)
{
 EM_error(EM_tokPos, "%s", s);
 exit(1);
}
%}


%union {
	int pos;
	int ival;
	string sval;
	A_var var;
	A_exp exp;
        A_expList explist;
        A_decList declist;
        A_dec  dec;
        A_efieldList efieldlist;
        A_efield  efield;
        A_namety namety;
        A_nametyList nametylist;
        A_fieldList fieldlist;
        A_field field;
        A_fundecList fundeclist;
        A_fundec fundec;
        A_ty ty;
	}

%token <sval> ID STRING
%token <ival> INT

%token 
  COMMA COLON SEMICOLON LPAREN RPAREN LBRACK RBRACK 
  LBRACE RBRACE DOT 
  PLUS MINUS TIMES DIVIDE EQ NEQ LT LE GT GE
  AND OR ASSIGN
  ARRAY IF THEN ELSE WHILE FOR TO DO LET IN END OF 
  BREAK NIL
  FUNCTION VAR TYPE 

%type <exp>		exp atomExp emExp seqExp
%type <explist>		emParameter parameter expSeq
%type <var>		lvalue attribute attributes
%type <declist>		emDecs decs
%type <dec>		dec vardec
%type <efieldlist>	emRecords records
%type <efield>		record
%type <nametylist>	tydecs
%type <namety>		tydec
%type <fieldlist>	tyfields oneTyfields
%type <ty>		ty
%type <fundeclist>	fundec
%type <fundec>		oneFundec

%left SEMICOLON
%nonassoc DO
%nonassoc LOWER
%nonassoc TYPE
%nonassoc FUNCTION
%nonassoc OF
%nonassoc LOW
%nonassoc  ELSE
%right ASSIGN
%left OR
%left AND
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS
%left DOT LBRACK 

%start program

%%

program	:	emExp	{absyn_root = $1;}
	;

emExp	:	exp	{$$ = $1;}
	|		{$$ = NULL;}
	;

exp	:	atomExp	{$$ = $1;}
	|	atomExp PLUS atomExp	{$$ = A_OpExp(EM_tokPos,A_plusOp,$1,$3);}
	|	atomExp MINUS atomExp	{$$ = A_OpExp(EM_tokPos,A_minusOp,$1,$3);}
	|	exp EQ exp	{$$ = A_OpExp(EM_tokPos,A_eqOp,$1,$3);}
	|	exp NEQ exp	{$$ = A_OpExp(EM_tokPos,A_neqOp,$1,$3);}
	|	exp LT exp	{$$ = A_OpExp(EM_tokPos,A_ltOp,$1,$3);}
	|	exp LE exp	{$$ = A_OpExp(EM_tokPos,A_leOp,$1,$3);}
	|	exp GT exp	{$$ = A_OpExp(EM_tokPos,A_gtOp,$1,$3);}
	|	exp GE exp	{$$ = A_OpExp(EM_tokPos,A_geOp,$1,$3);}
	|	exp AND exp	{$$ = A_IfExp(EM_tokPos,$1,$3,A_IntExp(EM_tokPos,0));} 
	|	exp OR exp	{$$ = A_IfExp(EM_tokPos,$1,A_IntExp(EM_tokPos,1),$3);}
	|	lvalue ASSIGN exp		{$$ = A_AssignExp(EM_tokPos,$1,$3);}
	|	IF exp THEN exp	%prec LOW	{$$ = A_IfExp(EM_tokPos,$2,$4,A_NilExp(EM_tokPos));}
	|	IF exp THEN exp ELSE exp	{$$ = A_IfExp(EM_tokPos,$2,$4,$6);}
	|	WHILE exp DO exp		{$$ = A_WhileExp(EM_tokPos,$2,$4);}
	|	FOR ID ASSIGN exp TO exp DO exp	{$$ = A_ForExp(EM_tokPos,S_Symbol($2),$4,$6,$8);}
	|	BREAK				{$$ = A_BreakExp(EM_tokPos);}
	|	LET emDecs IN seqExp END	{$$ = A_LetExp(EM_tokPos,$2,$4);}
	|	ID LBRACE emRecords  RBRACE	{$$ = A_RecordExp(EM_tokPos,S_Symbol($1),$3);}
	|	ID LBRACK exp RBRACK OF exp	{$$ = A_ArrayExp(EM_tokPos,S_Symbol($1),$3,$6);}
	;

atomExp	:	INT	{$$ = A_IntExp(EM_tokPos,$1);}
	|	STRING	{$$ = A_StringExp(EM_tokPos,$1);}
	|	lvalue	{$$ = A_VarExp(EM_tokPos,$1);}
	|	NIL	{$$ = A_NilExp(EM_tokPos);}
	|	LPAREN seqExp RPAREN	{$$ = $2;}
	|	atomExp TIMES atomExp	{$$ = A_OpExp(EM_tokPos,A_timesOp,$1,$3);}
	|	atomExp DIVIDE atomExp	{$$ = A_OpExp(EM_tokPos,A_divideOp,$1,$3);}
	|	MINUS atomExp	%prec UMINUS	{$$ = A_OpExp(EM_tokPos,A_minusOp,A_IntExp(EM_tokPos,0),$2);}
	|	ID LPAREN emParameter RPAREN	{$$ = A_CallExp(EM_tokPos,S_Symbol($1),$3);}
	;

emDecs	:	decs	{$$ = $1;}
	|		{$$ = NULL;}
	;

decs	:	dec		{$$ = A_DecList($1,NULL);}
	|	dec decs	{$$ = A_DecList($1,$2);}
	;

dec	:	tydecs	{$$ = A_TypeDec(EM_tokPos,$1);}
	|	vardec	{$$ = $1;}
	|	fundec	{$$ = A_FunctionDec(EM_tokPos,$1);}
	;

tydecs	:	tydec	%prec LOWER	{$$ = A_NametyList($1,NULL);}
	|	tydec tydecs		{$$ = A_NametyList($1,$2);}
	;

tydec	:	TYPE ID EQ ty	{$$ = A_Namety(S_Symbol($2),$4);}
	;

ty	:	ID			{$$ = A_NameTy(EM_tokPos,S_Symbol($1));}
	|	LBRACE tyfields RBRACE	{$$ = A_RecordTy(EM_tokPos,$2);}
	|	ARRAY OF ID		{$$ = A_ArrayTy(EM_tokPos,S_Symbol($3));}
	;

tyfields	:	oneTyfields	{$$ = $1;}
		|			{$$ = NULL;}
		;

oneTyfields	:	ID COLON ID 			{$$ = A_FieldList(A_Field(EM_tokPos,S_Symbol($1),S_Symbol($3)),NULL);}
		|	ID COLON ID COMMA oneTyfields	{$$ = A_FieldList(A_Field(EM_tokPos,S_Symbol($1),S_Symbol($3)),$5);}
		;

vardec	:	VAR ID ASSIGN exp		{$$ = A_VarDec(EM_tokPos,S_Symbol($2),NULL,$4);}
	|	VAR ID COLON ID ASSIGN exp	{$$ = A_VarDec(EM_tokPos,S_Symbol($2),S_Symbol($4),$6);}
	;

fundec	:	oneFundec	%prec LOWER	{$$ = A_FundecList($1,NULL);}
	|	oneFundec fundec		{$$ = A_FundecList($1,$2);}
	;

oneFundec	:	FUNCTION ID LPAREN tyfields RPAREN EQ exp		{$$ = A_Fundec(EM_tokPos,S_Symbol($2),$4,NULL,$7);}
		|	FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp	{$$ = A_Fundec(EM_tokPos,S_Symbol($2),$4,S_Symbol($7),$9);}
		;
	
lvalue	:	ID		{$$ = A_SimpleVar(EM_tokPos,S_Symbol($1));}
	|	attributes	{$$ = $1;}
	;

attributes	:	attribute		{$$ = $1;}
		|	attributes DOT ID	{$$ = A_FieldVar(EM_tokPos,$1,S_Symbol($3));}
		|	attributes LBRACK exp RBRACK   {$$ = A_SubscriptVar(EM_tokPos,$1,$3);}
		;

attribute	:	ID DOT ID	{$$ = A_FieldVar(EM_tokPos,A_SimpleVar(EM_tokPos,S_Symbol($1)),S_Symbol($3));}
		|	ID LBRACK exp RBRACK	%prec LOWER	{$$ = A_SubscriptVar(EM_tokPos,A_SimpleVar(EM_tokPos,S_Symbol($1)),$3);}
		;

emRecords	:	records	{$$ =$1;}
		|		{$$ = NULL;} 
		;

records	:	record			{$$ = A_EfieldList($1,NULL);}
	|	record COMMA records	{$$ = A_EfieldList($1,$3);}
	;

record	:	ID EQ exp	{$$ = A_Efield(S_Symbol($1),$3);}
	;

seqExp	:	expSeq	{$$ = A_SeqExp(EM_tokPos,$1);}
	|		{$$ = A_SeqExp(EM_tokPos,NULL);}
	;

expSeq	:	exp	{$$ = A_ExpList($1,NULL);}
	|	exp SEMICOLON expSeq  {$$ = A_ExpList($1,$3);}
	;

emParameter	:	parameter	{$$ = $1;}
		|			{$$ = NULL;} 
		;

parameter	:	exp			{$$ = A_ExpList($1,NULL);}
                |	exp COMMA parameter	{$$ = A_ExpList($1,$3);}
                ;










