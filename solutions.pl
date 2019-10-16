:- dynamic sol_fact/3, numsol/1, cause/3, explainCount/1, label/2.

display_solutions(NSol):-
	set_count(numsol,0),
	show_next_solution,numsol(NSol).
	
show_next_solution:-
        gettoken([0' ,10],T,D),!,
	(T='UNSATISFIABLE',!
	;T='SATISFIABLE',!
	;incr(numsol,1), numsol(N), 
	 writelist(['Answer:',N]),nl,
	 atom_to_term(T,Term,_),writefact(Term),
	 (D=0' ,!,get_next_fact; true),
	 nl,
	 findCauses,
	 skipEquivalentExplanations,
	 (
	 	opt(report),!,
	 	makeReportDir,
	 	writeReport
	 ;
	 	opt(static_report),!,
	 	makeReportDir,
	 	writeReport
	 ;
	 	writeCauses
	 ),
	
	retractall(explainCount(_C)),
	retractall(fired(_RN1, _V1)),
	retractall(justExplain(_ET)),
	retractall(graphPath(_T, _V2, _L1, _RN2, _RIP)),
	retractall(cause(_TF, _LF, _VF, _RN3, _L2)),
	retractall(reportRow(_R)),	
	retractall(toExplain(_Expl)),
	retractall(label(_FT, _LF2)),

	 show_next_solution,nl,nl
	).
show_next_solution.	

get_next_fact :-
	gettoken([0' ,10],T,D),!,
	atom_to_term(T,Term,_),writefact(Term),
	(D=0' ,!,get_next_fact; true).

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

writefact(Term):-
	(   
		Term =.. [F|_], functor_prefix(F,aux,_),!   	% ignore auxiliary atoms
	;	Term =.. [F|_], functor_prefix(F,nholds,_),!	% ignore auxiliary atoms
	;	Term = -_X,!   									% ignore auxiliary atoms	  
	; 
	  	% Fired rules 
	  	Term =.. [F|Values],
	    functor_prefix(F,fired_,F2),!,
		atom_number(F2,RuleNumber),

		assert(fired(RuleNumber, Values))
	; 
	  	% Explain rules
	  	Term =.. [F|Values],
	  	functor_prefix(F,explain_,F2),!,
	  	ExplainTerm =.. [F2|Values],

	  	assert(explain),
	  	assert(justExplain(ExplainTerm))
	;
		% Label rules
		Term =.. [F|ValuesAndLabelNumber],

		% Fired Term
		functor_prefix(F,label_,F2),

		% getting LabelNumber
		append(FiredValues, [LabelNumber], ValuesAndLabelNumber),
		labelInfo(LabelNumber, ArgNames, BodyVariables, Label),

		% Fired Term Values
		append(Aux, [_TermValue], FiredValues),
		length(BodyVariables,N),
		length(Prefix, N),
		append(Prefix, FiredTermValues, Aux),


		FiredTerm =.. [F2|FiredTermValues],

		% Process Label
		append(BodyVariables, ArgNames, Names),
		processLabel(Label, _, FiredValues, Names, LabelFired),

		assert(label(FiredTerm, LabelFired))

	; 
		opt(facts_output),
	  	% Holds rules
	  	Term =.. [F|Args],
	    functor_prefix(F,holds_,F2),!,
		append(Args0,[Value],Args),
		Term2 =.. [F2|Args0],
		% If it is a boolean value change the output format
		(	
		  Value = true  -> 
			writelist([Term2,'.']),nl
		; Value = false -> 
			writelist(['~',Term2,'.']),nl
		;	
			writelist([Term2,'=',Value,'.']),nl	
		)
	  ; 
	  	opt(debug),!,
	  	write(Term),nl
	  ; 
	  	true
	).