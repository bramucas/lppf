/******************************************************************************
 loadfile.pl 28/10/2008 (Pedro Cabalar)
******************************************************************************/ 

loadfiles([]):-!.
loadfiles([Fname|Fs]):-
	(opt(quiet),!; header),

	atom_concat(Fname,'.tmp',TmpFile),
	
	% Remove temporal file
	(exists_file(TmpFile),!,delete_file(TmpFile); true),
	
	preparse_files([Fname|Fs],TmpFile),

	see(TmpFile), read_terms, seen,

	% Remove temporal file
	(delete_file(TmpFile),!; true),
	
	% Get implicit function names
	get_implicit_fnames.

preparse_files([],_):-!.
preparse_files([F|Fs],TmpFile):-
	(opt(quiet),!; writelist(['loading file ',F,'...']),nl),
	concatall(['./getterms < ',F,' >> ',TmpFile],Command),
	shell(Command),
	preparse_files(Fs,TmpFile).

read_terms:-
	read(Term),
	(
	  Term=end_of_file,!
	   
	; ( Term=rule(Code,Head,Body),!,
	    assertz(rule(Code,Head,Body))
		
	  ; Term=fname(F/N),!,
	    (fname(F/N),!; assertz(fname(F/N)) )	    
		
	  ; Term=show(F/N),!,
	    (fname(F/N),!; assertz(show(F/N)) )	    

	  ; Term=explainrule(Head,Body),!,
	  	assertz(explainRule(Head,Body)) 

	  ; Term=labelrule(LabelNumber,Label,Head,Body),!,
	  	assertz(labelRule(LabelNumber,Label,Head,Body)) 

	  ; debugln(['Not implemented:',Term])
	  ),
	  read_terms
	).

get_implicit_fnames:-
	repeat,
	  ( rule(_,H,B),
	    get_implicit_fname(H),
	    get_non0ary_fname(H),
	    get_non0ary_fname(B),
	    get_boolean_equalities(B),
	    fail
	  ; true),!.

get_implicit_fname( assign(fterm(F,L),_) ) :- length(L,N), assertfname(F/N).
get_implicit_fname( def_assign(fterm(F,L),_) ) :- length(L,N), assertfname(F/N).
get_implicit_fname( choice(fterm(F,L),_) ) :- length(L,N), assertfname(F/N).

get_non0ary_fname(T):-
	subterms([fterm/2],T,Xs),
	repeat, (
	  member(fterm(F,Args),Xs),
	  length(Args,N),
	  N \= 0,
	  assertfname(F/N),
	  fail
	; true
	),!.

get_boolean_equalities(T):-
	subterms(['=='/2],T,Xs),
	repeat, (
	  member(fterm(F,Args) == _,Xs),
	  length(Args,N),
	  assertfname(F/N),
	  fail
	; true
	),!.


	
assertfname(N):-fname(N),!.
assertfname(N):-assertz(fname(N)).
