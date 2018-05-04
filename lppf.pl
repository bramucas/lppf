/******************************************************************************
 lppf.pl version 2 12/01/2017 (Pedro Cabalar)
% Addition of double negation and existential quantifier "forsome"
******************************************************************************/ 

%----- operators -------------------------------------------------------------

% arithmetic operators have priority 400 (*,/,mod) or 500 (+,-)
% relational operators have priority 700
:-op(400,yfx,'\\ ').
:-op(700,xfx,:=).
:-op(700,yfx,<=).
:-op(700,yfx,'!=').
:-op(710,fy,not).
:-op(710,fy,'not -').

%----- general dynamic predicates ----------------------------------------------
   
:- dynamic rule/3, fname/1, show/1, explainRule/2.

%----- other "modules" --------------------------------------------------------
:- consult('util.pl').
:- consult('display.pl').
:- consult('loadfile.pl').
:- consult('translate.pl').
:- consult('solutions.pl').
:- consult('htmlReportStyle.pl').

:- dynamic opt/1.	% Program options like, for instance, opt(debug)

%----- main ------------------------------------------------------------------

% retrieves command line arguments Args and calls main(Args)

main_c :- unix(argv(Argv)),
        ( append(_PrologArgs, [--|AppArgs], Argv), !,
          main(AppArgs)
        ; main(Argv)).


% if no arguments, print help
main([]):- !,
  header,nl,
  write('lppf [options] filenames'),nl,
  write('  -n <number>    computes at most <number> stable models. Default <number> is 1.'),nl,
  write('                 When <number> is 0, computes all stable models.'),nl,
  write('  -o <filename>  output file.'),nl,
  write('  -q             supresses information header and messages.'),nl,
  write('  -t             just print the translation of functions into predicates.'),nl,
  write('  -l             just print causes of labeled rules.'),nl,
  write('  -r, --report   builds an html report with graphs.'),nl,
  (opt(nohalt),!; halt(0)).

main(Args):-
	checkoptions(Args,Args2),
	(opt(nummodels(N)),!; N=1,asserta(opt(nummodels(N))) ),
	loadfiles(Args2),
	(opt(translation),!;tell('lppf.tmp')),
	translate,
	(opt(translation),!;told,get_solutions('lppf.tmp',N)),
	(opt(nohalt),!; halt(0)).

get_solutions(File,N):-
    	concat_atom(['clingo --verbose=0 ',N,' ',File,' > lppf.2.tmp'],Command),
	shell(Command,_),
	see('lppf.2.tmp'),
	(opt(outfile(F)),!,tell(F); true),
	display_solutions(NSol),
	seen,
	(opt(outfile(F)),!,told;true),
	delete_file('lppf.2.tmp'),
	writelist([NSol,' solution']),
	(NSol=1,!; write('s')),nl,
	NSol>0,!.
	
checkoptions([],[]):-!.
checkoptions([X|Xs],Ys):- 
	    option(X,Opt),!,asserta(opt(Opt)),checkoptions(Xs,Ys)
	  ; X='-o',!, Xs=[File|Zs],
	    asserta(opt(outfile(File))),checkoptions(Zs,Ys)
	  ; X='-n',!,Xs=[N|Zs],
	    asserta(opt(nummodels(N))),checkoptions(Zs,Ys).
			    
checkoptions([X|Xs],[X|Ys]):- checkoptions(Xs,Ys).

option('-q',quiet).
option('-v',debug).
option('-t',translation).
option('--nohalt',nohalt).
option('-l',labels).
option('-r',report).
option('--report',report).

header :-
  write(' Logic Programs with Partial Functions 2.0'),nl,
  write(' (c) 2017- Pedro Cabalar, 12/01/2017, University of Corunna'),nl.
