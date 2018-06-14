YACC=bison -b y
LEX=flex
VER=`cat VERSION`
CC= gcc
PL=swipl
SRCS=util.pl display.pl loadfile.pl translate.pl solutions.pl htmlReportStyle.pl lppf.pl
LIVER= ex/liver/facts.lppf ex/liver/rules_value.lppf ex/liver/constraints.lppf ex/liver/liver_calc.lppf
lppf : $(SRCS)
	$(PL) -g main_c -t halt -q -o lppf -c lppf.pl

getterms : lex.yy.c y.tab.c getterms.c
	$(CC) -o getterms lex.yy.c y.tab.c getterms.c

y.tab.c : parser.y
	$(YACC) -d -v parser.y
	
lex.yy.c : scanner.l
	$(LEX) scanner.l	

liver:
	./lppf -l $(LIVER)