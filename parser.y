%{
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "getterms.h"

#include <string.h>

extern int yylex(void);

int ruleline=1;
int rulenum=0;
char *bufferDefaultValue = ""; 
char *predicate;

extern int yyrestart(FILE *);
int yyerror (char *s)  /* Called by yyparse on error */
{
  fprintf (stderr,"%d: %s after `%s'\n", yyline, s, yytext);
  return 1;
}

/* Build the body of a rule handling with bufferDefaultValue */
char* buildBody(char* body)
{
  char* finalBody;

  finalBody = body;

  /* Adding extras to the body */
  if (strcmp(bufferDefaultValue,"") != 0)
  {
    if (strcmp(finalBody,"") != 0)
      finalBody = strCat(finalBody, ",", bufferDefaultValue, NULL);
    else
      finalBody = bufferDefaultValue;
    
    bufferDefaultValue = "";
  }

  return finalBody;
}

%}

%union {
char  	*strval; /* For returning a string */
}


%token ABS
%token EXISTS
%token FORSOME
%token FUNCTION
%token IF
%token IN
%token NOT
%token OR
%token SHOW

%token DOTS
%token ASSIGN
%token ARROW
%token GEQ
%token LEQ
%token NEQ

%token  NUMBER     /* integer   */
%token  ID         /* identifier */
%token  VID        /* variable identifier */
%token  STRING     /* string literal */


%type <strval> fname
%type <strval> atom
%type <strval> predatom
%type <strval> literal
%type <strval> body
%type <strval> head
%type <strval> set
%type <strval> term
%type <strval> fterm
%type <strval> aterm
%type <strval> termlist
%type <strval> id
%type <strval> vidlist
%type <strval> vid
%type <strval> num

%left IF
%left OR
%left NOT
%right '=' LEQ GEQ NEQ '>' '<'
%left '-' '+'
%left '*' '/' '\\'
%left UMINUS     /* unary minus */

/* Grammar follows */

%%

program : sentences;

sentences : sentence '.' | sentences sentence '.';

sentence : 
            FUNCTION {predicate="fname";} fnames;
          | SHOW     {predicate="show";} fnames;
	  | rule
	  ;

fnames :
      fname		{printf("%s(%s).\n",predicate,$1); }
    | fnames ',' fname	{printf("%s(%s).\n",predicate,$3); }
  ;  

fname :
	id			{ $$=strCat($1,"/0",NULL); }
  | id '/' num  { $$=strCat($1,"/",$3,NULL); }
  
rule :
    head								{ruleline=yyline; printf("rule(%d/%d,%s,[%s]).\n", rulenum++, ruleline, $1, buildBody(""));}
  | head {ruleline=yyline;} IF body		{printf("rule(%d/%d,%s,[%s]).\n", rulenum++, ruleline, $1, buildBody($4));}
  | IF {ruleline=yyline;} body			{printf("rule(%d/%d,[],[%s]).\n",rulenum++,ruleline,$3);}
  ;

atom :
    predatom			{$$=$1;}
  | term '=' term		{$$=strCat($1,"=",$3,NULL);}
  | term NEQ term		{$$=strCat($1,"=\\=",$3,NULL);}
  | term '>' term		{$$=strCat($1,">",$3,NULL);}
  | term '<' term		{$$=strCat($1,"<",$3,NULL);}
  | term GEQ term		{$$=strCat($1,">=",$3,NULL);}
  | term LEQ term		{$$=strCat($3,">=",$1,NULL);}
  | EXISTS term			{$$=strCat($2,"=",$2,NULL);}
  | FORSOME vidlist '(' body ')'		{$$=strCat("forsome([",$2,"],[",$4,"])",NULL); }
  ;
  
predatom :
    id					{$$=$1;}
  | id '(' termlist ')'	{$$=strCat($1,"(",$3,")",NULL);}
  ;
  
literal :
    atom				{$$=$1;}
  | NOT atom			        {$$=strCat("not (",$2,")",NULL);}
  | NOT NOT atom	  	        {$$=strCat("not not  (",$3,")",NULL);}
  ;

body :
    literal				{$$=$1;}
  | body ',' literal	{$$=strCat($1,",",$3,NULL);}
  ;  

head :
    predatom			{$$=strCat("predic(",$1,")",NULL);}
  | fterm ASSIGN term	{$$=strCat("assign(",$1,",",$3,")",NULL);}
  | fterm '~' term {$$=strCat("assign(",$1,",",$3,")",NULL);bufferDefaultValue=strCat("not (", $1, "=\\=", $3, ")", NULL);}
  | fterm IN set		{$$=strCat("choice(",$1,",",$3,")",NULL);}
  ;

set :
	'{' '}'					{$$=strCat("set([])",NULL);}
  | '{' termlist '}'		{$$=strCat("set([",$2,"])",NULL);}
  | '{' vid '|' body '}'	{$$=strCat("set(",$2,",[",$4,"])",NULL);}
/*  
  | '{' fterm  '|' body '}'	{$$=strCat("set(",$2,",[",$4,"])",NULL);}
  | '{' aterm  '|' body '}'	{$$=strCat("set(",$2,",[",$4,"])",NULL);}
  | '{' fterm '}'	{$$=strCat("set(",$2,",[",$4,"])",NULL);}
  | '{' aterm '}'	{$$=strCat("set(",$2,",[",$4,"])",NULL);}
*/
  ;

term :
	vid					{$$=$1;}
  | num					{$$=$1;}
  | fterm				{$$=$1;}
  | aterm				{$$=$1;}
  | '(' term ')'		{$$=$2;}
  ;

fterm :
    id					{$$=strCat("fterm(",$1,",[])",NULL);}
  | id '(' termlist ')'	{$$=strCat("fterm(",$1,",[",$3,"])",NULL);}
  ;

aterm :
    term '+' term		{$$=strCat("(",$1," + ",$3,")",NULL);}
  | term '-' term		{$$=strCat("(",$1," - ",$3,")",NULL);}
  | term '*' term		{$$=strCat("(",$1," * ",$3,")",NULL);}
  | term '/' term		{$$=strCat("(",$1," / ",$3,")",NULL);}
  | term '\\' term		{$$=strCat("(",$1," '\\\\ ' ",$3,")",NULL);}
  | ABS '(' term ')'	{$$=strCat("abs(",$3,")",NULL);}
  | '|' term '|'		{$$=strCat("abs(",$2,")",NULL);}
  | '-' term  %prec UMINUS  {$$=strCat("(0-",$2,")",NULL);}
  ;
  
termlist :
    term			    {$$=$1;}
  | termlist ',' term	{$$=strCat($1,",",$3,NULL);}
  ;

id : 
    ID 		{ $$=yylval.strval; }
  ;

vidlist :
	  vid			    {$$=$1;}
	| vidlist ',' vid	{$$=strCat($1,",",$3,NULL);}
	;

vid : VID 		{ $$=strCat("v('",yylval.strval,"')",NULL); } ;

num : NUMBER 		{ $$=yylval.strval; } ;

/* End of grammar */
%%
