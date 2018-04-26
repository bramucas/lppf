:- dynamic sol_fact/3, numsol/1, cause/3.

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
	 show_next_solution,nl,
	 writeCauses
	).
show_next_solution.	

get_next_fact :-
	gettoken([0' ,10],T,D),!,
	atom_to_term(T,Term,_),writefact(Term),
	(D=0' ,!,get_next_fact; true).

writefact(Term):-
	(   Term =.. [F|_], functor_prefix(F,aux,_),!   % ignore auxiliary atoms
	  ; Term =.. [F|_], functor_prefix(F,nholds,_),!   % ignore auxiliary atoms
	  ; Term = -_X,!   % ignore auxiliary atoms	  
	  ; Term =.. [F|Values],	% Fired rules
	    functor_prefix(F,fired_,F2),!,
		atom_number(F2,RuleNumber),
		% Record values that fired the rule
		assert(fired(RuleNumber, Values))
	  ; Term =.. [F|Values],	% Explain rules
	  	functor_prefix(F,explain_,F2),!,
	  	ExplainTerm =.. [F2|Values],
	  	assert(justExplain(ExplainTerm))
	  ; Term =.. [F|Args],		% Holds rules
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

% writeCauses
%	First find the causes of fired rules, and finally write them.
writeCauses :-
	% Facts first (rules without body)
	repeat,
	(
		fired(RuleNumber, ArgValues),
		ruleInfo(Label,RuleNumber, Fname, Args, VarNames, []),
		FiredTerm =.. [Fname|Args],
		replaceValues(ArgValues, VarNames, [FiredTerm], [EvaluatedFiredTerm]),		
		replaceValues(ArgValues, VarNames, [Label], [EvaluatedLabel]),
		assert(cause(EvaluatedFiredTerm,EvaluatedLabel,RuleNumber,[])),
		fail
	;	true
	),!,
	% Rules with body
	repeat,
	(
		fired(RuleNumber, ArgValues),
		ruleInfo(Label, RuleNumber, Fname, Args, VarNames, Body),
		(
			Body = [] -> true
		;	
			getCauses(ArgValues, VarNames, Body, Causes),
			FiredTerm =.. [Fname|Args],
			replaceValues(ArgValues, VarNames, [FiredTerm], [EvaluatedFiredTerm]),
			replaceValues(ArgValues, VarNames, [Label], [EvaluatedLabel]),
			assert(cause(EvaluatedFiredTerm,EvaluatedLabel,RuleNumber,Causes))
		),
		fail
	;	true
	),!,
	% Writting causes
	(
	current_predicate(cause/4) ->
		(
		current_predicate(justExplain/1) ->
			repeat,
			(
				justExplain(Term),
				cause(Term, Label, RuleNumber, Causes),
				writeCauseTree(Term, Label, Causes, 0),nl,
				fail
			;	true
			),!	
		;
			repeat,
			(
				cause(Term, Label, RuleNumber, Causes),
				writeCauseTree(Term, Label, Causes, 0),nl,
				fail
			;	true
			),!
		)
	; 
		true
	).

writeCauseTree(Term, Label, Causes, Level) :- 
	% Root marker
	(
	Level = 0 ->
		write('*')
	;
		true
	),

	% Node Term
	write(Term),
	% Label or not
	(
	Label = no_label ->
		write('\n')
	;
		writelist([' $',Label,'\n'])
	),
	
	% Causes or not
	(
	Causes = [] ->
		!,true
	;
		% Causes
		repeat,
		(
			member(C, Causes),
			C =.. [cause|[CTerm, CLabel, _RuleNumber, CTermCauses]],
			writeBranch(Level),
			NextLevel is Level+1,
			writeCauseTree(CTerm,CLabel,CTermCauses,NextLevel),
			fail
		;	
			true
		),!
	).


writeBranch(Level) :-
	writeBirth(Level),
	write('-- ').

writeBirth(0) :-
	write(' |'),!.
writeBirth(Level) :-
	write(' |   '),
	NextLevel is Level-1,
	writeBirth(NextLevel).

% getCauses(ArgValues, VarNames, Body, Causes)
%	Find the causes that make true a given body with a given pair of Variable-Value.
%		- ArgValues: List of values for the variables in 'VarNames' (order matters).
%		- VarNames: List of variable names that you can find in the body.
%		- Body
%		- Causes (return)
getCauses(ArgValues, VarNames, Body, Causes) :-
	simplifyBody(Body, SimplifiedBody),
	replaceValues(ArgValues, VarNames, SimplifiedBody, Result),
	(
	opt(labels) ->
		evaluateCausesLabels(Result, Causes)
	;
		evaluateCauses(Result, Causes)
	).

% evaluateCauses(Body, Causes)
% Find the causes that make true a given body with a given pair of Variable-Value. The lack of
% causes for a term on the body don't make fail this function.
%	- Body: it is supposed to be simplifyBody and have no variables without vari.
%	- Causes (return)
evaluateCauses(_, []) :-
	\+ current_predicate(cause/4),!.

evaluateCauses([HTerm|Tail], [Cause|MoreCauses]) :-
	current_predicate(cause/4),
	cause(HTerm, Label, RuleNumber, TermCauses),
	Cause =.. [cause|[HTerm, Label, RuleNumber, TermCauses]],
	evaluateCauses(Tail, MoreCauses).

evaluateCauses([HTerm|Tail], MoreCauses) :-
	current_predicate(cause/4),
	\+ cause(HTerm, _Label, _RuleNumber, _TermCauses),
	evaluateCauses(Tail, MoreCauses).

evaluateCauses([],[]).


evaluateCausesLabels([HTerm|Tail], MoreCauses) :-
	current_predicate(cause/4),
	cause(HTerm, no_label, _RuleNumber, TermCauses),!,
	evaluateCausesLabels(Tail, TailCauses),
	append(TermCauses, TailCauses, MoreCauses).

evaluateCausesLabels([HTerm|Tail], [Cause|MoreCauses]) :-
	current_predicate(cause/4),
	cause(HTerm, Label, RuleNumber, TermCauses),
	Cause =.. [cause|[HTerm, Label, RuleNumber, TermCauses]],
	evaluateCausesLabels(Tail, MoreCauses).

evaluateCausesLabels([],[]).

% replaceValues(ArgValues, VarNames, TermList, Result)
%	Replace variables inside 'TermList' that match with anyone in 'VarNames' with the value in 'ArgValues' (based in order).
%	Arguments that doesn't appear in 'VarNames' will be ignored.
%		- ArgValues: List of values for the variables in 'VarNames' (order matters).
%		- VarNames: List of variable names that you can find in the body.
%		- TermList
%		- Result (return)
replaceValues(ArgValues, VarNames, [Item|T], [NewItem|Result]) :-
	Item =.. [F|Args],
	getVarValues(ArgValues, VarNames, Args, Values),
	NewItem =.. [F|Values],
	replaceValues(ArgValues, VarNames, T, Result).
replaceValues(_, _, [], []).


% getVarValues(ArgValues, VarNames, VarList, Result).
%	Return a list of values for a given var list, based in 'VarNames' and 'ArgValues'.
%		- ArgValues: List of values for the variables in 'VarNames' (order matters).
%		- VarNames: List of variable names that you can find in the body.
%		- VarList: List of variables that will be replaced.
%		- Result (return): List of values.
getVarValues(ArgValues, VarNames, [Var|T], [Var|Rest]) :-
	\+ nth0(_Position, VarNames, Var),!,
	getVarValues(ArgValues, VarNames, T, Rest).

getVarValues(ArgValues, VarNames, [Var|T], [Value|Rest]) :-
	nth0(Position, VarNames, Var),
	nth0(Position, ArgValues, Value),
	getVarValues(ArgValues, VarNames, T, Rest).

getVarValues(_, _, [], []).


buildCauses([HItem|TItem], [HLabel|TLabel], [HRuleNumber|TRuleNumber], [HCauses|TCauses], [cause(HItem,HLabel,HRuleNumber,HCauses)|MoreCauses]) :-
	buildCauses(TItem,TLabel,TRuleNumber,TCauses,MoreCauses).
buildCauses([],[],[],[],[]).


% simplifyBody(Original, SimplifiedBody)
%	Just keeps 'holds_' form terms, and eliminate the last argument of same ones.
simplifyBody([H|T], [Simplified|ResultBody]) :-
	H =.. [F|Args],
	concat_atom(['holds_',Fname],F),
	append(Vars, [_Value], Args),
	Simplified =.. [Fname|Vars],
	simplifyBody(T, ResultBody).

simplifyBody([H|T], ResultBody) :-
	H =.. [F|_Args],
	\+ concat_atom(['holds_',_Fname],F),
	simplifyBody(T, ResultBody).

simplifyBody([],[]).



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