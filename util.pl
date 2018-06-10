/******************************************************************************
 util.pl 4/12/2003 (Pedro Cabalar)
******************************************************************************/ 

writelist([]):-!.
writelist([A|As]):-write(A),writelist(As).

writelist(_Sep,[A]):-!,write(A).
writelist(Sep,[A|As]):-write(A),write(Sep),writelist(Sep,As).

concatall([],''):-!.
concatall([A|As],C):-
	concatall(As,B),atom_concat(A,B,C).

append_all([],[]):-!.
append_all([L|Ls],N):-append(L,M,N),append_all(Ls,M).

extract(X,L,M):-append(Pre,[X|Suf],L),append(Pre,Suf,M).

% Updates a dynamic predicate Pred/1 whose argument is a list L, by adding
% an element to L
add_to_set(Pred,X):-
	functor(T,Pred,1),arg(1,T,L),
	(call(T),!, retractall(T); L=[]),
	merge_set(L,[X],L2),
	functor(T2,Pred,1),arg(1,T2,L2),
	asserta(T2).

% Updates a dynamic predicate Pred/1 whose argument is an integer I, by adding
% a quantity X to I.
incr(Pred,X):-
	T =.. [Pred,N],
	(call(T),!, retractall(T); N=0),
	M is N + X,
	T2 =.. [Pred,M],
	asserta(T2).

set_count(Pred,X):-
	T=..[Pred,_],
	(retractall(T),!; true),
	T2=..[Pred,X],asserta(T2).
		
% Retrieves a set Ss of subterms of term T that are built with one of the
% functors in list Fs. Each functor is described as Name/Arity

subterms(Fs,T,Ss):- 
	functor(T,F,N),!,subterms_args(N,Fs,T,[],Ss0),
	(member(F/N,Fs),!,merge_set(Ss0,[T],Ss)
	;Ss=Ss0).
	
subterms_args(0,_Fs,_T,Ss,Ss):-!.
subterms_args(N,Fs,T,Ss0,Ss):-
	arg(N,T,A), subterms(Fs,A,SsA), 
	merge_set(Ss0,SsA,Ss1),
	M is N-1,
	subterms_args(M,Fs,T,Ss1,Ss).

% Applies predicate P/2 to transform any subterm S of T that matches with some
% functor in Fs. Thus, if the subterm is S and we apply P(S,S2), then S is
% replaced by S2.
% example:
%   replace(r(A,B),r(B,A)).
%   ? map_subterms(replace,[r/2],r(a,r(9,0)),T).
%   T=r(r(0,9),a)

map_subterms(P,Fs,T,T2):- 
	functor(T,F,N),!,
	functor(T1,F,N),		% build a new term T1
	map_subterms_args(N,P,Fs,T,T1),	% map P to arguments of T
	(member(F/N,Fs),!,call(P,T1,T2)
	; T2=T1).
	
map_subterms_args(0,_P,_Fs,_T,_T1):-!.
map_subterms_args(N,P,Fs,T,T1):-
	arg(N,T,A), map_subterms(P,Fs,A,A1), 
	arg(N,T1,A1),
	M is N-1,
	map_subterms_args(M,P,Fs,T,T1).




% filter(Pred,L,L1,L2)
% Separates list L into list L1 (those elements that satisfy predicate Pred) and % L2, the rest of elements.

filter(_P,[],[],[]):-!.
filter(P,[X|Xs],L1,L2):-
	( call(P,X),!, L1=[X|M1], L2=M2; L1=M1, L2=[X|M2]),
	filter(P,Xs,M1,M2).

join([],[]):-!.
join([L|Ls],[X|Xs]):-member(X,L),join(Ls,Xs).

% binop(N,F,L,T)
% constructs a term T by successive applications of binary functor F 
% to all elements in list L. N representes the 'neuter' element for F

binop(N,_F,[],N):-!.
binop(_N,F,L,T):-binop(F,L,T).

% binop(F,L,T)
% constructs a term T by successive applications of binary functor F 
% to all elements in (nonempty) list L

binop(_F,[X],X):-!.
binop(F,[X|L],T):-binop(F,L,T0),T =.. [F,X,T0].


% replaceString(OriginalString, StringToReplace, Replacement, Result)
replaceString(OriginalString, StringToReplace, Replacement, Result) :-
	atomic_list_concat(AuxString, StringToReplace, OriginalString), 
	atomic_list_concat(AuxString, Replacement, Result).

sort_atoms_by_length(Atoms, ByLength) :-
        map_list_to_pairs(atom_length, Atoms, Pairs),
        keysort(Pairs, Sorted),
        pairs_values(Sorted, ByLength).