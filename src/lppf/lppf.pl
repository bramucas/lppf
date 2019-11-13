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
   
:- dynamic rule/3, fname/1, show/1, explainRule/2, labelRule/4.

%----- other "modules" --------------------------------------------------------
:- consult('util.pl').
:- consult('display.pl').
:- consult('loadfile.pl').
:- consult('translate.pl').
:- consult('solutions.pl').
:- consult('htmlReportStyle.pl').
:- consult('explanations.pl').

:- dynamic opt/1.	% Program options like, for instance, opt(version)

%----- main ------------------------------------------------------------------

% retrieves command line arguments Args and calls main(Args)

main_c :- unix(argv(Argv)),
        ( append(_PrologArgs, [--|AppArgs], Argv), !,
          main(AppArgs)
        ; main(Argv)).


% if no arguments, print help
main([]):- !,
  writeln('lppf [options] filenames'),
  writeln('  --version      prints information header'),
  writeln('  -q             only prints the solutions, omiting other messages'),
  writeln('  -n <number>    computes at most <number> stable models. Default <number> is 1.'),
  writeln('                 When <number> is 0, computes all stable models.'),
  writeln('  -o <filename>  output file.'),
  writeln('  -t             just print the translation of functions into predicates.'),
  writeln('  -r, --report   builds an html report with graphs.'),
  writeln('  -sr            builds the report but does not open it automatically.'),
  writeln('  Formatting modes:'),
  writeln('     --format 0:       ignores labels and #explain sentences. Does not produce explanations.'),
  writeln('     --format trees:   (default) explanations are shown as trees.'),
  writeln('     --format cterms:  automatically labels all non-labeled facts in the program. This mode ignores textual labels.'),
  writeln('  Head labeling:'),
  writeln('     --labelHeads none:   (default) only the labels explicitily written by the programmer are used.'),
  writeln('     --labelHeads facts:  automatically labels all non-labeled facts in the program.'),
  writeln('     --labelHeads all:    automatically labels all non-labeled rules in the program.'),
  (opt(nohalt),!; halt(0)).

main(Args):-
	checkoptions(Args,Args2),
	(opt(nummodels(N)),!; N=1,asserta(opt(nummodels(N))) ),
	(opt(version),!,header,halt(0);true),
	loadfiles(Args2),
	(opt(translation),!;tell('lppf.tmp')),
	translate,
	(opt(translation),!;told,get_solutions('lppf.tmp',N)),
	(opt(nohalt),!; halt(0)).

get_solutions(File,N):-
    concat_atom(['clingo --verbose=0 ',N,' ',File,' > lppf.2.tmp'],Command),
	shell(Command,_),
	see('lppf.2.tmp'),
	(\+ opt(report), \+ opt(static_report),opt(outfile(F)),!,tell(F); true),
	display_solutions(NSol),
	seen,
	(\+ opt(report),\+ opt(static_report),opt(outfile(F)),!,told;true),
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
	    asserta(opt(nummodels(N))),checkoptions(Zs,Ys)
	  ; X='--format',!,Xs=[N|Zs],
	  	concat_atom(['format ', N], VerboseOption),
	  	option(VerboseOption, Opt),
	    asserta(opt(Opt)),checkoptions(Zs,Ys)
	  ; X='--labelHeads',!,Xs=[N|Zs],
	  	concat_atom(['labelHeads ', N], VerboseOption),
	  	option(VerboseOption, Opt),
	    asserta(opt(Opt)),checkoptions(Zs,Ys).
			    
checkoptions([X|Xs],[X|Ys]):- checkoptions(Xs,Ys).


option('--version',version).
option('-q', quiet).
option('-t',translation).
option('--nohalt',nohalt).

option('-m',minimal_explanations). /* Desuse? */

/* Report options */
option('-r',report).
option('--report',report).
option('-sr',static_report).

/* Format modes */
option('format 0', no_explanations).
option('format trees', default).
option('format cterms', causal_terms).

/* Auto labeling modes */
option('labelHeads none', default).
option('labelHeads facts', label_facts).
option('labelHeads all', complete).


header :-
  write(' Logic Programs with Partial Functions 2.0'),nl,
  write(' (c) 2019- Pedro Cabalar, 12/01/2017, University of Corunna'),nl.
