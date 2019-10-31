YACC=bison -b y
LEX=flex
VER=`cat VERSION`
CC=gcc
PL=swipl

LPPF_SRC=./src/lppf/
GTERMS_SRC=./src/getterms/

SRCS=$(LPPF_SRC)util.pl $(LPPF_SRC)display.pl $(LPPF_SRC)loadfile.pl $(LPPF_SRC)translate.pl $(LPPF_SRC)solutions.pl $(LPPF_SRC)explanations.pl $(LPPF_SRC)htmlReportStyle.pl $(LPPF_SRC)lppf.pl


lppf : $(SRCS)
	$(PL) -g main_c -t halt -q -o lppf -c $(LPPF_SRC)lppf.pl

getterms : lex.yy.c y.tab.c $(GTERMS_SRC)getterms.c
	$(CC) -I $(GTERMS_SRC) -o getterms lex.yy.c y.tab.c $(GTERMS_SRC)getterms.c

y.tab.c : $(GTERMS_SRC)parser.y
	$(YACC) -d -v $(GTERMS_SRC)parser.y
	
lex.yy.c : $(GTERMS_SRC)scanner.l
	$(LEX) $(GTERMS_SRC)scanner.l	
