:- dynamic sol_fact/3, numsol/1, cause/3, explainCount/1.

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
	 findCauses,
	 (
	 	opt(report),
	 	writeReport
	 ;
	 	writeCauses
	 )
	).
show_next_solution.	

get_next_fact :-
	gettoken([0' ,10],T,D),!,
	atom_to_term(T,Term,_),writefact(Term),
	(D=0' ,!,get_next_fact; true).

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

	  	assert(justExplain(ExplainTerm))
	; 
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

% findCauses
%	Find the causes of fired rules.
findCauses :-
	% Facts first (rules without body)
	repeat,
	(
		fired(RuleNumber, ArgValues),
		ruleInfo(RuleNumber, OriginalLabel, OriginalTerm, VarNames, []),
		
		replaceValues(ArgValues, VarNames, [OriginalTerm], [TermFired]),		
		replaceValues(ArgValues, VarNames, [OriginalLabel], [LabelFired]),
		append(_Args2, [ValueFired], ArgValues),

		assert(cause(TermFired, LabelFired, ValueFired, RuleNumber, is_leaf)),
		fail
	;	
		true
	),!,
	% Rules with body
	repeat,
	(
		fired(RuleNumber, ArgValues),
		ruleInfo(RuleNumber, OriginalLabel, OriginalTerm, VarNames, Body),
		(
			Body = [] -> true
		;	
			getCauses(ArgValues, VarNames, Body, Causes),
			
			replaceValues(ArgValues, VarNames, [OriginalTerm], [TermFired]),
			replaceValues(ArgValues, VarNames, [OriginalLabel], [LabelFired]),
			append(_Args, [ValueFired], ArgValues),

			assert(cause(TermFired, LabelFired, ValueFired, RuleNumber, Causes))
		),
		fail
	;	true
	),!.

% writeCauses
% Write ASCII trees for explanations.
writeCauses :-
	% Writting causes
	(
	  current_predicate(cause/5) ->
		(
		  current_predicate(justExplain/1) ->
			repeat,
			(
				justExplain(Term),
				cause(Term, Label, Value, _RuleNumber, Causes),
				writeCauseTree(Term, Label, Value, Causes, 0),nl,
				incr(explainCount,1),
				fail
			;	true
			),
			explainCount(ExplainCount),
			writelist([ExplainCount, ' ocurrences explained.']),nl,
			!	
		;
			repeat,
			(
				cause(Term, Label, Value, _RuleNumber2, Causes),
				writeCauseTree(Term, Label, Value, Causes, 0),nl,
				fail
			;	true
			),!
		)
	; 
		true
	).

writeReport :-
	(
	  current_predicate(cause/5) ->
	  	makeDirectory('report'),
	  	makeGraphs,
	  	makeReport,
	  	shell('sensible-browser report/report.html &')
	; 
		write('Report cant be written'),nl
	).

makeReport :-
	open('report/report.html', write, ReportFile),
	write(ReportFile, 
		'<html>
<head>
	<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
</head>
<body>
<div class="container">\n'
		),

	repeat,
	(
		graphPath(Term, RuleNumber ,JpgFileName),

		term_to_atom(Term, WTerm), 
		concat_atom(['<h2>', RuleNumber, ' - ', WTerm, '</h2>\n'], TitleTag),
		concat_atom(['<div class="text-center border"><img src="', JpgFileName, '"></img>\n'], ImageTag),

		write(ReportFile, TitleTag),
		write(ReportFile, ImageTag),

		fail
	;	true
	),!,

	write(ReportFile, '</div></body></html>'),
	close(ReportFile).

makeGraphs :-
	(
	  current_predicate(justExplain/1) ->
		repeat,
		(
			justExplain(Term),
			cause(Term, Label, Value, RuleNumber, Causes),
			
			makeGraph(Term, Label, Value, RuleNumber, Causes),

			incr(explainCount,1),
			fail
		;	true
		),
		explainCount(ExplainCount),
		writelist([ExplainCount, ' ocurrences explained.']),nl,
		!	
	;
		repeat,
		(
			cause(Term, Label, Value, RuleNumber, Causes),
			makeGraph(Term, Label, Value, RuleNumber, Causes), 
			fail
		;	true
	),!
).

makeGraph(Term, Label, Value, RuleNumber, Causes) :-
	% Create and open file
	term_to_atom(Term, WTerm),
	concat_atom(['report/', WTerm] ,DirectoryName),
	makeDirectory(DirectoryName),

	concat_atom([DirectoryName, '/', RuleNumber, '.dot'], FileName),
	open(FileName, write, FileStream),

	% Writting dot file
	write(FileStream, 'digraph {\n'),
	buildEdges(FileStream, Term, Label, Value, Causes),
	write(FileStream, '}'),
	close(FileStream),

	% Creating image
	concat_atom([DirectoryName, '/', RuleNumber, '.jpg'], JpgFileName),
	concat_atom(["dot -Tjpg '", FileName,"' > '", JpgFileName, "'"], Command),
	shell(Command),

	% Saving path
	concat_atom([WTerm, '/', RuleNumber, '.jpg'], RelativeImagePath),
	assert(graphPath(Term, RuleNumber, RelativeImagePath)).

buildEdges(FileStream, Term, Label, Value, [HCause | Tail]) :-
	HCause =.. [cause|[CTerm, CLabel, CValue, _RuleNumber, CTermCauses]],

	(
		Label = no_label ->
		term_to_atom(Term, WTerm),!
	;
		term_to_atom(Label, WTerm),!
	),
	(
		CLabel = no_label ->
			term_to_atom(CTerm, WCTerm),!	
	;
		term_to_atom(CLabel, WCTerm),!
	),
	
	concat_atom(['"', WCTerm, '=', CValue, '" -> "', WTerm, '=', Value, '";\n'], GraphLine),
	write(FileStream, GraphLine),
	
	buildEdges(FileStream, CTerm, CLabel, CValue, CTermCauses),
	buildEdges(FileStream, Term, Label, Value, Tail).

buildEdges(FileStream, Term, Label, Value, is_leaf) :-
	(
		Label = no_label ->
		term_to_atom(Term, WTerm),!
	;
		term_to_atom(Label, WTerm),!
	),
	
	concat_atom(['"', WTerm, '=', Value, '";\n'], GraphLine),
	write(FileStream, GraphLine).

buildEdges(FileStream, Term, Label, Value, []) :-
	(
		Label = no_label ->
		term_to_atom(Term, WTerm),!
	;
		term_to_atom(Label, WTerm),!
	),
	
	concat_atom(['"', WTerm, '=', Value, '";\n'], GraphLine),
	write(FileStream, GraphLine).


makeDirectory(Path) :-
	exists_directory(Path).

makeDirectory(Path) :-
	\+ exists_directory(Path),
	make_directory(Path).


% Writes an ASCII tree explanation for a cause
writeCauseTree(Term, Label, Value, Causes, Level) :- 
	% Root marker
	(
	  Level = 0 ->
		write('*')
	;
		true
	),

	% Label
	(
	  Label = no_label ->
		true
	;
		writelist([' \033[1;33m',Label,'\033[0m '])

	),

	% Node Term and value
	(
		opt(labels), Level>0 ->
			writelist([' = ', Value])
	;
		% If it is a boolean value change the output format
		(	
		  Value = true  -> 
			write(Term)
		; Value = false -> 
			writelist(['~',Term])
		;	
			writelist([Term,' = ',Value])
		)
	),nl,
	
	% Causes
	(
	  Causes = [] ->
		!,true
	;
		repeat,
		(
			member(C, Causes),
			C =.. [cause|[CTerm, CLabel, CValue, _RuleNumber, CTermCauses]],
			writeBranch(Level),
			NextLevel is Level+1,
			writeCauseTree(CTerm, CLabel, CValue, CTermCauses,NextLevel),
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
%	- Body: it is supposed to be a simplifiedBody and have no variables without value.
%	- Causes (return)
evaluateCauses(_, []) :-
	\+ current_predicate(cause/5),!.

evaluateCauses([HTerm|Tail], [Cause|MoreCauses]) :-
	current_predicate(cause/5),
	cause(HTerm, Label, Value, RuleNumber, TermCauses),
	Cause =.. [cause|[HTerm, Label, Value, RuleNumber, TermCauses]],
	evaluateCauses(Tail, MoreCauses).

evaluateCauses([HTerm|Tail], MoreCauses) :-
	current_predicate(cause/5),
	\+ cause(HTerm, _Label, _Value, _RuleNumber, _TermCauses),
	evaluateCauses(Tail, MoreCauses).

evaluateCauses([],[]).

% Special version of evaluateCauses that skips a cause if it has no label.
evaluateCausesLabels([HTerm|Tail], MoreCauses) :-
	current_predicate(cause/5),
	cause(HTerm, no_label, _Value, _RuleNumber, TermCauses),!,
	evaluateCausesLabels(Tail, TailCauses),
	(
		TermCauses=is_leaf ->
			MoreCauses = TailCauses
	;
		append(TermCauses, TailCauses, MoreCauses)
	).

evaluateCausesLabels([HTerm|Tail], [Cause|MoreCauses]) :-
	current_predicate(cause/5),
	cause(HTerm, Label, Value, RuleNumber, TermCauses),
	Cause =.. [cause|[HTerm, Label, Value, RuleNumber, TermCauses]],
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