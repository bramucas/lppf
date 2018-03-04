:- dynamic sol_fact/3, numsol/1.

display_solutions(NSol):-
	set_count(numsol,0),
	show_next_solution,numsol(NSol).
	
%%% lparse %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
show_next_solution:-
        gettoken([0' ,10],T,D),!,
	(T='UNSATISFIABLE',!
	;T='SATISFIABLE',!
	;incr(numsol,1), numsol(N), 
	 writelist(['Answer:',N]),nl,
	 atom_to_term(T,Term,_),writefact(Term),
	 (D=0' ,!,get_next_fact; true),
	 show_next_solution
	).
show_next_solution.	

get_next_fact :-
	gettoken([0' ,10],T,D),!,
	atom_to_term(T,Term,_),writefact(Term),
	(D=0' ,!,get_next_fact; true).
/*	 
writefact(Term):-
	(   Term =.. [F|_], functor_prefix(F,aux,_),!   % ignore auxiliary atoms
	  ; Term =.. [F|_], functor_prefix(F,nholds,_),!   % ignore auxiliary atoms
	  ; Term = -_X,!   % ignore auxiliary atoms	  
	  ; Term =.. [F|Args],
	    functor_prefix(F,atom_,F2),!,
		Term2 =.. [F2|Args],
		writelist([Term2,'.']),nl
	  ; Term =.. [F|Args],
	    functor_prefix(F,holds_,F2),!,
		append(Args0,[Value],Args),
		Term2 =.. [F2|Args0],
		writelist([Term2,'=',Value,'.']),nl
	  ; opt(debug),!,write(Term),nl
	  ; true
	).
*/

writefact(Term):-
	(   Term =.. [F|_], functor_prefix(F,aux,_),!   % ignore auxiliary atoms
	  ; Term =.. [F|_], functor_prefix(F,nholds,_),!   % ignore auxiliary atoms
	  ; Term = -_X,!   % ignore auxiliary atoms	  
	  ; Term =.. [F|Args],
	    functor_prefix(F,atom_,F2),!,
		Term2 =.. [F2|Args],
		writelist([Term2,'.']),nl
	  ; Term =.. [F|Args],
	    functor_prefix(F,holds_,F2),!,
		append(Args0,[Value],Args),
		Term2 =.. [F2|Args0],
		% If it is a boolean value change the output format
		(	
			Value = true  -> writelist([Term2,'.']),nl
		;	Value = false -> writelist(['~',Term2,'.']),nl
		;					 writelist([Term2,'=',Value,'.']),nl	
		)
	  ; opt(debug),!,write(Term),nl
	  ; true
	).


gettoken(Delims,Tok,Delim):-
	gettokenchars(Delims,Chars,Delim),
	atom_codes(Tok,Chars).

gettokenchars(Delims,Cs,Delim):-
  get0(C),!,
  ( C = -1,!,fail
  ; member(C,Delims),!,Delim=C,Cs=[]  
  ; gettokenchars(Delims,Cs0,Delim), Cs=[C|Cs0]
  ).

functor_prefix(F,Pref,Rest):-
	atom_chars(F,FL),atom_chars(Pref,PL),
	append(PL,RestL,FL),atom_chars(Rest,RestL).

commas_to_list((A,B), [A|B1]) :- !,commas_to_list(B,B1).
commas_to_list(A,[A]).
