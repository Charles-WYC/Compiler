/*
 * main.c
 */

#include <stdio.h>
#include "util.h"
#include "symbol.h"
#include "types.h"
#include "absyn.h"
#include "errormsg.h"
#include "temp.h" /* needed by translate.h */
#include "tree.h" /* needed by frame.h */
#include "frame.h" /* needed by translate.h and printfrags prototype */
#include "translate.h"
#include "semant.h" /* function prototype for transProg */
#include "canon.h"
#include "prabsyn.h"
#include "printtree.h"
//#include "escape.h" /* needed by escape analysis */
#include "parse.h"
#include "regalloc.h"

extern bool anyErrors;

/* print the assembly language instructions to filename.s */
static void doProc(FILE *out, F_frame frame, T_stm body)
{
 AS_proc proc;
 T_stmList stmList;
 AS_instrList iList;

 Temp_map F_tempMap = Temp_empty();

 stmList = C_linearize(body);
 stmList = C_traceSchedule(C_basicBlocks(stmList));
 /* printStmList(stdout, stmList);*/
 iList  = F_codegen(frame, stmList); /* 9 */

 RA_result ra = RA_regAlloc(frame, iList);  /* 10, 11 */

 //fprintf(out, "BEGIN function\n");
 AS_printInstrList (out, iList, Temp_layerMap(F_tempMap,ra->coloring));
}

void TransStr(char* d, char* s){
	int i =0;
	int k =0;
	while(s[i] != '\0'){
		if(s[i] == '\n'){
			d[k++]= '\\';
			d[k++]= 'n';
		}
		else if(s[i] == '\t'){
			d[k++]= '\\';
			d[k++]= 't';
		}
		else{
			d[k++]= s[i];
		}
		i++;
	}
	d[k++] = '\0';
	return;
}

int main(int argc, string *argv)
{
 A_exp absyn_root;
 S_table base_env, base_tenv;
 F_fragList frags;
 char outfile[100];
 FILE *out = stdout;
 
 /*FILE *myIn = fopen("in.txt", "r");
 FILE *myOut = fopen("out.txt", "w");
 
 char buf[200];
 while(fgets(buf, 200, myIn)){
	if(buf[0] == ' '){
		fprintf(myOut*/
 
 if (argc==2) {
   absyn_root = parse(argv[1]);
   if (!absyn_root)
     return 1;
     
#if 0
   pr_exp(out, absyn_root, 0); /* print absyn data structure */
   fprintf(out, "\n");
#endif
	//If you have implemented escape analysis, uncomment this
   //Esc_findEscape(absyn_root); /* set varDec's escape field */

   TagOne = 0;
   TagFA = 0;
   frags = SEM_transProg(absyn_root);
   if (anyErrors) return 1; /* don't continue */

   /* convert the filename */
   sprintf(outfile, "%s.s", argv[1]);
   out = fopen(outfile, "w");
   /* Chapter 8, 9, 10, 11 & 12 */
   fprintf(out, "%s\n", ".text");
   fprintf(out, "%s\n", ".globl tigermain");
   fprintf(out, "%s\n\n", ".type tigermain, @function");
   F_fragList tempFrags = frags;
   for (;frags;frags=frags->tail){
     if (frags->head->kind == F_procFrag) {
   	   fprintf(out, "%s:\n", frags->head->u.proc.frame->name->name);
   	   fprintf(out, "    %s\n", "pushl %ebp");
   	   fprintf(out, "    %s\n", "movl %esp, %ebp");
       doProc(out, frags->head->u.proc.frame, frags->head->u.proc.body);
   	   fprintf(out, "    %s\n", "movl %ebp, %esp");
   	   fprintf(out, "    %s\n", "popl %ebp");
 	   fprintf(out, "    ret\n\n");
     }
   }
   frags = tempFrags;
   fprintf(out, "%s\n", ".section .rodata");
   for (;frags;frags=frags->tail){
     if (frags->head->kind == F_stringFrag){
       fprintf(out, "%s:\n", frags->head->u.stringg.label->name);
       int l = strlen(frags->head->u.stringg.str);
       char *a = (char*)&l;
	   char d[256];
	   TransStr(d, frags->head->u.stringg.str);
       fprintf(out, ".string \"%c%c%c%c%s\"\n", a[0], a[1], a[2], a[3], d);
     }
   }

   fclose(out);
   return 0;
 }
 EM_error(0,"usage: tiger file.tig");
 return 1;
}

