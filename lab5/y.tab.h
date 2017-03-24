/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    ID = 258,
    STRING = 259,
    INT = 260,
    ASSIGN = 261,
    AND = 262,
    OR = 263,
    EQ = 264,
    NEQ = 265,
    LT = 266,
    LE = 267,
    GT = 268,
    GE = 269,
    PLUS = 270,
    MINUS = 271,
    TIMES = 272,
    DIVIDE = 273,
    UMINUS = 274,
    COMMA = 275,
    COLON = 276,
    SEMICOLON = 277,
    LPAREN = 278,
    RPAREN = 279,
    LBRACK = 280,
    RBRACK = 281,
    LBRACE = 282,
    RBRACE = 283,
    DOT = 284,
    ARRAY = 285,
    IF = 286,
    THEN = 287,
    ELSE = 288,
    WHILE = 289,
    FOR = 290,
    TO = 291,
    DO = 292,
    LET = 293,
    IN = 294,
    END = 295,
    OF = 296,
    BREAK = 297,
    NIL = 298,
    FUNCTION = 299,
    VAR = 300,
    TYPE = 301
  };
#endif
/* Tokens.  */
#define ID 258
#define STRING 259
#define INT 260
#define ASSIGN 261
#define AND 262
#define OR 263
#define EQ 264
#define NEQ 265
#define LT 266
#define LE 267
#define GT 268
#define GE 269
#define PLUS 270
#define MINUS 271
#define TIMES 272
#define DIVIDE 273
#define UMINUS 274
#define COMMA 275
#define COLON 276
#define SEMICOLON 277
#define LPAREN 278
#define RPAREN 279
#define LBRACK 280
#define RBRACK 281
#define LBRACE 282
#define RBRACE 283
#define DOT 284
#define ARRAY 285
#define IF 286
#define THEN 287
#define ELSE 288
#define WHILE 289
#define FOR 290
#define TO 291
#define DO 292
#define LET 293
#define IN 294
#define END 295
#define OF 296
#define BREAK 297
#define NIL 298
#define FUNCTION 299
#define VAR 300
#define TYPE 301

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE YYSTYPE;
union YYSTYPE
{
#line 21 "tiger.y" /* yacc.c:1909  */

	int pos;
	int ival;
	string sval;
	S_symbol sym;
	A_var var;
	A_exp exp;
    A_dec dec;
	A_decList declist;
	A_expList explist;
	A_efieldList efieldlist;
	A_ty ty;
	A_fieldList fieldlist;
	A_fundec fundec;
	A_namety namety;
	/* et cetera */

#line 164 "y.tab.h" /* yacc.c:1909  */
};
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
