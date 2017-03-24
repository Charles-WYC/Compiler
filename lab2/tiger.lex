%{
#include <string.h>
#include "util.h"
#include "tokens.h"
#include "errormsg.h"

int charPos=1;

int yywrap(void)
{
 charPos=1;
 return 1;
}

void adjust(void)
{
 EM_tokPos=charPos;
 charPos+=yyleng;
}
/*
* Please don't modify the lines above.
* You can add C declarations of your own below.
*/

%}
  /* You can add lex definitions here. */

letter	[A-Za-z]
digit	[0-9]
id		{letter}({letter}|{digit}|_)*
int		({digit})+
NewLine		(\n|\r\n|\n\r)
WhiteSpace	(\r|\t|\f)

%s COMMENT STRING_1 STRING_2

%%
  /* 
  * Below are some examples, which you can wipe out
  * and write reguler expressions and actions of your own.
  */ 

<INITIAL>while		{adjust(); return WHILE;}
<INITIAL>for		{adjust(); return FOR;}
<INITIAL>to			{adjust(); return TO;}
<INITIAL>break		{adjust(); return BREAK;}
<INITIAL>let		{adjust(); return LET;}
<INITIAL>in 		{adjust(); return IN;}
<INITIAL>end		{adjust(); return END;}
<INITIAL>function	{adjust(); return FUNCTION;}
<INITIAL>var		{adjust(); return VAR;}
<INITIAL>type		{adjust(); return TYPE;}
<INITIAL>array		{adjust(); return ARRAY;}
<INITIAL>if 		{adjust(); return IF;}
<INITIAL>then		{adjust(); return THEN;}
<INITIAL>else		{adjust(); return ELSE;}
<INITIAL>do			{adjust(); return DO;}
<INITIAL>of 		{adjust(); return OF;}
<INITIAL>nil		{adjust(); return NIL;}
<INITIAL>","		{adjust(); return COMMA;}
<INITIAL>":"		{adjust(); return COLON;}
<INITIAL>";"		{adjust(); return SEMICOLON;}
<INITIAL>"("		{adjust(); return LPAREN;}
<INITIAL>")"		{adjust(); return RPAREN;}
<INITIAL>"["		{adjust(); return LBRACK;}
<INITIAL>"]"		{adjust(); return RBRACK;}
<INITIAL>"{"		{adjust(); return LBRACE;}
<INITIAL>"}"		{adjust(); return RBRACE;}
<INITIAL>"."		{adjust(); return DOT;}
<INITIAL>"+"		{adjust(); return PLUS;}
<INITIAL>"-"		{adjust(); return MINUS;}
<INITIAL>"*"		{adjust(); return TIMES;}
<INITIAL>"/"		{adjust(); return DIVIDE;}
<INITIAL>"="		{adjust(); return EQ;}
<INITIAL>"<>"		{adjust(); return NEQ;}
<INITIAL>"<="		{adjust(); return LE;}
<INITIAL>">="		{adjust(); return GE;}
<INITIAL>"<"		{adjust(); return LT;}
<INITIAL>">"		{adjust(); return GT;}
<INITIAL>"&"		{adjust(); return AND;}
<INITIAL>"|"		{adjust(); return OR;}
<INITIAL>":="		{adjust(); return ASSIGN;}

<INITIAL>" "		{adjust(); continue;}
<INITIAL>{WhiteSpace}	{adjust(); continue;}
<INITIAL>{NewLine}	{adjust(); EM_newline(); continue;}
<INITIAL>{int}		{adjust(); yylval.ival=atoi(yytext); return INT;}
<INITIAL>{id}		{adjust(); yylval.sval=yytext; return ID;}
<INITIAL>"/*"		{adjust(); BEGIN COMMENT;}
<INITIAL>\"			{adjust(); yylval.sval=""; BEGIN STRING_1;}
<INITIAL>.	 		{adjust(); EM_error(EM_tokPos,"illegal token");}

<STRING_1>\"		{charPos+=yyleng;
					 if(strlen(yylval.sval) == 0){
						yylval.sval = "(null)";
					 }
					 BEGIN INITIAL; return STRING;}
<STRING_1>\\[0-9][0-9][0-9]	{
								char tempString[4];
								strncpy(tempString, yytext+1, 3);
								tempString[3] = '\0';
								int temp = atoi(tempString);
								if(temp>255){
									EM_error(EM_tokPos, "\\ddd Error");
								}
								else{
									yylval.sval+=(char)temp;
								}
							}
<STRING_1>\\t		{charPos+=yyleng; 
					 char* temp = (char*)checked_malloc(strlen(yylval.sval)+yyleng+1);
					 strcpy(temp, yylval.sval);
					 temp[strlen(yylval.sval)]='\t';
					 yylval.sval = String(temp);}
<STRING_1>\\n		{charPos+=yyleng; 
					 char* temp = (char*)checked_malloc(strlen(yylval.sval)+yyleng+1);
					 strcpy(temp, yylval.sval);
					 temp[strlen(yylval.sval)]='\n';
					 yylval.sval = String(temp);}
<STRING_1>\\\"		{charPos+=yyleng; 
					 char* temp = (char*)checked_malloc(strlen(yylval.sval)+yyleng+1);
					 strcpy(temp, yylval.sval);
					 temp[strlen(yylval.sval)]='\"';
					 yylval.sval = String(temp);}
<STRING_1>\\\\		{charPos+=yyleng; 
					 char* temp = (char*)checked_malloc(strlen(yylval.sval)+yyleng+1);
					 strcpy(temp, yylval.sval);
					 temp[strlen(yylval.sval)]='\\';
					 yylval.sval = String(temp);}
<STRING_1>\\		{charPos+=yyleng; BEGIN STRING_2;}
<STRING_1>.			{charPos+=yyleng; 
					 char* temp = (char*)checked_malloc(strlen(yylval.sval)+yyleng+1);
					 strcpy(temp, yylval.sval);
					 strncpy(temp+strlen(yylval.sval), yytext, yyleng);
					 yylval.sval = String(temp);
					}

<STRING_2>\\		{charPos+=yyleng; BEGIN STRING_1;}
<STRING_2>{WhiteSpace}	{charPos+=yyleng;}
<STRING_2>" "		{charPos+=yyleng;}
<STRING_2>{NewLine}	{charPos+=yyleng;}
<STRING_2>.			{charPos+=yyleng; EM_error(EM_tokPos, "\\ Error");}

<COMMENT>"*/"		{charPos+=yyleng; BEGIN INITIAL;}
<COMMENT>.			{charPos+=yyleng;}
<COMMENT>{NewLine}	{charPos+=yyleng;}

