%{
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "getterms.h"

extern int yylex(void);

int ruleline=1;
int rulenum=0;
char *predicate;

extern int yyrestart(FILE *);
int yyerror (char *s)  /* Called by yyparse on error */
{
  fprintf (stderr,"%d: %s after `%s'\n", yyline, s, yytext);
  return 1;
}

%}

%union {
char  	*strval; /* For returning a string */
}


%token ABS
%token SUM
%token EXISTS
%token FUNCTION
%token CHOOSE
%token IF
%token IN
%token NOT
%token OR
%token SHOW

%token DOTS
%token ASSIGN
%token DEFVALUE
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
%type <strval> literal
%type <strval> body
%type <strval> head
%type <strval> set
%type <strval> term
%type <strval> fterm
%type <strval> aterm
%type <strval> termlist
%type <strval> item
%type <strval> itemlist
%type <strval> id
%type <strval> vid
%type <strval> num
%type <strval> aggregate

%left IF
%left OR
%left NOT
%right '=' LEQ GEQ NEQ '>' '<'
%left '-' '+'
%left '*' '/' '\\'
%left UMINUS     /* unary minus */
%left '~'

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
    head								{ruleline=yyline; printf("rule(%d/%d,%s,[]).\n",rulenum++,ruleline,$1);}
  | head {ruleline=yyline;} IF body		{printf("rule(%d/%d,%s,[%s]).\n",rulenum++,ruleline,$1,$4);}
  | IF {ruleline=yyline;} body			{printf("rule(%d/%d,[],[%s]).\n",rulenum++,ruleline,$3);}
  ;

atom :
    fterm		        {$$=strCat($1,"==fterm(true,[])",NULL);}
  | '~' fterm                   {$$=strCat($2,"==fterm(false,[])",NULL);}
  | term '=' term		{$$=strCat($1,"=",$3,NULL);}
  | term NEQ term		{$$=strCat($1,"=\\=",$3,NULL);}
  | term '>' term		{$$=strCat($1,">",$3,NULL);}
  | term '<' term		{$$=strCat($1,"<",$3,NULL);}
  | term GEQ term		{$$=strCat($1,">=",$3,NULL);}
  | term LEQ term		{$$=strCat($3,">=",$1,NULL);}
  | EXISTS '{' item '}' {$$=strCat("agg(exists,[",$3,"])",NULL);}
  ;
    
literal :
    atom				  {$$=$1;}
  | NOT atom			{$$=strCat("not (",$2,")",NULL);}
  | NOT NOT atom	{$$=strCat("not not  (",$3,")",NULL);}
  ;

itemlist:
    item              {$$=$1;}
  | itemlist ';' item {$$=strCat($1,",",$3,NULL);}
  ;

item:
    termlist ':' body {$$=strCat("[",$1,"]:[",$3,"]",NULL);}
  | termlist          {$$=strCat("[",$1,"]:[]",NULL);}
  ; 


body :
    literal				{$$=$1;}
  | body ',' literal	{$$=strCat($1,",",$3,NULL);}
  ;  

head :
    fterm         {{$$=strCat("assign(",$1,", fterm(true,[]))",NULL);}}
  | '~' fterm         {{$$=strCat("assign(",$2,", fterm(false,[]))",NULL);}}
  | fterm ASSIGN term	{$$=strCat("assign(",$1,",",$3,")",NULL);}
  | fterm DEFVALUE term {$$=strCat("def_assign(",$1,",",$3,")",NULL);}
  | fterm ASSIGN CHOOSE set		{$$=strCat("choice(",$1,",",$4,")",NULL);}
  ;

set :
	'{' '}'					{$$=strCat("set([])",NULL);}
  | '{' termlist '}'		{$$=strCat("set([",$2,"])",NULL);}
  | '{' vid ':' body '}'	{$$=strCat("set(",$2,",[",$4,"])",NULL);}
/*  
  | '{' fterm  '|' body '}'	{$$=strCat("set(",$2,",[",$4,"])",NULL);}
  | '{' aterm  '|' body '}'	{$$=strCat("set(",$2,",[",$4,"])",NULL);}
  | '{' fterm '}'	{$$=strCat("set(",$2,",[",$4,"])",NULL);}
  | '{' aterm '}'	{$$=strCat("set(",$2,",[",$4,"])",NULL);}
*/
  ;

term :
    aggregate '{' itemlist '}' {$$=strCat("agg(",$1,",[",$3,"])", NULL);}
  | vid					{$$=$1;}
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
  | '~' term        {$$=strCat("neg(",$2,")",NULL);}
  ;
  
termlist :
    term			    {$$=$1;}
  | termlist ',' term	{$$=strCat($1,",",$3,NULL);}
  ;

id : 
    ID 		{ $$=yylval.strval; }
  ;

vid : VID 		{ $$=strCat("v('",yylval.strval,"')",NULL); } ;

num : NUMBER 		{ $$=yylval.strval; } ;

aggregate :
    SUM   {$$="sum";}
  ;

/* End of grammar */
%%
