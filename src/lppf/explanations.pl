:- dynamic toExplain/1, graphId/1.



%%%%%%%%%%%%% Causes search %%%%%%%%%%%%%%


% findCauses
%	Find the causes of the fired facts and build their causal trees.
findCauses :-
	% Facts first (rules without body)
	repeat,
	(
		fired(RuleNumber, ArgValues),
		ruleInfo(RuleNumber, OriginalLabel, OriginalTerm, VarNames, []),

		replaceValues(ArgValues, VarNames, [OriginalTerm], [TermFired]),

		% Automatically label no-labeled-facts if option label facts is active
		(
			OriginalLabel = no_label, opt(label_facts) ->

				append(_Arguments, [Value], ArgValues),
				term_to_atom(TermFired, PrintableTermFired),
				(	
				  Value = true  -> 
					concat_atom(['',PrintableTermFired], LabelFired)
				; Value = false -> 
					concat_atom(['-',PrintableTermFired], LabelFired)
				;	
					concat_atom([PrintableTermFired,'=',Value,'.'], LabelFired)
				)
			;
				processLabel(OriginalLabel, TermFired, ArgValues, VarNames, LabelFired)
		),

		append(_Args2, [ValueFired], ArgValues),

		addExplanation(cause(TermFired, LabelFired, ValueFired, RuleNumber, is_leaf)),
		fail
	;	
		true
	),!,
	% Rules with body
	repeat,
	(
		fired(RuleNumber, ArgValues),
		ruleInfo(RuleNumber, Label, OriginalTerm, VarNames, Body),
		(
			Body = [] -> true
		;	
			getCauses(ArgValues, VarNames, Body, Causes),
			
			replaceValues(ArgValues, VarNames, [OriginalTerm], [TermFired]),

			processLabel(Label, TermFired, ArgValues, VarNames, LabelFired),

			append(_Args, [ValueFired], ArgValues),

			addExplanation(cause(TermFired, LabelFired, ValueFired, RuleNumber, Causes))
		),
		fail
	;	true
	),!.


addExplanation(Expl) :-
	\+ current_predicate(cause/5),
	assert(Expl).

addExplanation(Expl) :-
	\+ Expl,!,
	assert(Expl).

addExplanation(Expl) :-
	Expl.


% getCauses(ArgValues, VarNames, Body, Causes)
%	Find the causes that make true a given body with a given pair of Variable-Value.
%		- ArgValues: List of values for the variables in 'VarNames' (order matters).
%		- VarNames: List of variable names that you can find in the body.
%		- Body
%		- Causes (return)
getCauses(ArgValues, VarNames, Body, TrimmedCauses) :-
	simplifyBody(Body, SimplifiedBody),
	replaceValues(ArgValues, VarNames, SimplifiedBody, Result),
	(
	\+ opt(complete) ->
		evaluateCausesLabels(Result, Causes0),
		trimCausesLabels(Causes0, Causes)
	;
		evaluateCauses(Result, Causes)
	),
	sort(Causes, TrimmedCauses).

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

% If cause has not label
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

% If cause has label
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
replaceValues([], _, _, []).
replaceValues(_, [], _, []).


% getVarValues(ArgValues, VarNames, VarList, Result).
%	Return a list of values for a given var list, based in 'VarNames' and 'ArgValues'.
%		- ArgValues: List of values for the variables in 'VarNames' (order matters).
%		- VarNames: List of variable names that you can find in the body.
%		- VarList: List of variables that will be replaced.
%		- Result (return): List of values.
getVarValues(ArgValues, VarNames, [Var|T], [Value|Rest]) :-
	Var = 'VAR_Value',!,
	last(ArgValues, Value),
	getVarValues(ArgValues, VarNames, T, Rest).	


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


trimCausesLabels([cause(HTerm, Label, Value, RuleNumber, TermCauses)|Tail], [cause(HTerm, Label, Value, RuleNumber, TermCauses)|TrimmedCauses]) :-
	findall(C, (member(C, Tail), C =..[cause|[_CTerm, CLabel, _CValue, _RuleNumber, CTermCauses]], Label = CLabel, TermCauses = CTermCauses), BranchesToTrim),
	subtract(Tail, BranchesToTrim, TrimmedTail),
	trimCausesLabels(TrimmedTail, TrimmedCauses).

trimCausesLabels([],[]).








%%%%%%%%%% Processing labels %%%%%%%%%%%%%%%

% processLabel(Label, TermFired, Values, Names, LabelFired)
%	Get the labels of a Fired Term
%		- Label: individual label
processLabel(no_label, TermFired, _, _, no_label) :-
	\+ label(TermFired,_).

processLabel(label(OriginalLabel), _, Values, Names, LabelFired) :-
	prepareLabel(OriginalLabel, PreparedLabel),
	replaceValues(Values, Names, [PreparedLabel], [LabelFired]).

processLabel(text(OriginalLabel), _, Values, Names, LabelFired) :-
	getVarReferences(OriginalLabel, References),
	maplist(name, Names, L),
	maplist(string_codes, VarNamesStrings, L),
	replaceAllReferences(Values, VarNamesStrings, References, OriginalLabel, NaturalText),
	LabelFired = NaturalText.

processLabel(_, TermFired, _, _, LabelFired) :-
	label(TermFired, LabelFired).


% Prepare variable names of a non-processed label to be replaced for its values.
prepareLabel(Label, PreparedLabel) :-
	Label =.. [_F|[LabelFname, LabelVars]],
	orderedAuxVarNames(LabelVars, PreparedLabelVars),
	PreparedLabel =.. [LabelFname|PreparedLabelVars].

% Variables
orderedAuxVarNames([v(X)|T], [Var|VarNames]) :-
	concat_atom(['VAR',X], Var),!,
	orderedAuxVarNames(T, VarNames).

% Constants
orderedAuxVarNames([X|T], [Var|VarNames]) :-
	X =.. [fterm|[Var,_]],
	orderedAuxVarNames(T, VarNames).

% Base case
orderedAuxVarNames([],[]).


% Text labels

replaceAllReferences([], _, _, NaturalExplanation, NaturalExplanation).
replaceAllReferences(_, _, [], NaturalExplanation, NaturalExplanation).


replaceAllReferences(ArgValues, VarNames, [HRef|Tail], Text, NaturalExplanation) :-
	HRef = "_Value",!,
	last(ArgValues, Value),
	string_concat("%", HRef, Reference),
	replaceString(Text, Reference, Value, NewText),!,
	replaceAllReferences(ArgValues, VarNames, Tail, NewText, NaturalExplanation).

replaceAllReferences(ArgValues, VarNames, [HRef|Tail], Text, NaturalExplanation) :-
	string_concat("VAR", HRef, VarName),
	nth0(I, VarNames, VarName),
	nth0(I, ArgValues, Value),
	string_concat("%", HRef, Reference),
	replaceString(Text, Reference, Value, NewText),!,
	replaceAllReferences(ArgValues, VarNames, Tail, NewText, NaturalExplanation).

replaceAllReferences(ArgValues, VarNames, [HRef|Tail], Text, NaturalExplanation) :-
	string_concat("VAR", HRef, VarName),
	\+ nth0(_I, ArgValues, VarName),
	replaceAllReferences(ArgValues, VarNames, Tail, Text, NaturalExplanation).	

getVarReferences(Text, Vars) :-
	string_chars(Text, Chars),
	findall(Index, nth0(Index, Chars, '%'), Indexes),
	auxGetVarReferences(Indexes, Chars, References),
	sort_atoms_by_length(References, Sorted),
	reverse(Sorted, Vars).

auxGetVarReferences([HI|Tail], Chars, [Var|MoreVars]) :-
	Index is HI +1,
	varReference(Index, Index, Chars, Var),
	auxGetVarReferences(Tail, Chars, MoreVars).

auxGetVarReferences([], _, []).


varReference(First, Index, Chars, Reference) :-
	nth0(Index, Chars, Char),
	char_code(Char, Code),
	refCode(Code),!,
	Next is Index+1,
	varReference(First, Next, Chars, Reference).

varReference(First, Index, Chars, Reference) :-
	nth0(Index, Chars, Char),
	char_code(Char, Code),
	\+ refCode(Code),!,	% delimitador
	Len is Index-First,
	string_chars(String, Chars),
	sub_string(String, First, Len, _, Reference).

varReference(First, Index, Chars, Reference) :-
	\+ nth0(Index, Chars, _Char),	% final de string
	Len is Index-First,
	string_chars(String, Chars),
	sub_string(String, First, Len, _, Reference).

refCode(241).	% ñ
refCode(209).	% Ñ
refCode(95).	% _
refCode(C) :- C>47, C<58.	% 0-9
refCode(C) :- C>96, C<122.	% a-z
refCode(C) :- C>64, C<91.	% A-Z





%%%%%%%%%%%%% ASCII trees output %%%%%%%%%%%%%

% writeCauses
% Write ASCII trees for explanations.
writeCauses :-
	% Writting causes
	(
	  current_predicate(cause/5) ->
		(
		  current_predicate(explain/0) ->
		  	(
		  	  current_predicate(justExplain/1) ->
		  	  (
		  		repeat,
				(
			      justExplain(Term),
				  toExplain(cause(Term, Label, Value, _RuleNumber, Causes)),
				  writeCauseTree(Term, Label, Value, Causes, 0),nl,
				  incr(explainCount,1),
				  fail
				; true
				),

				explainCount(ExplainCount),
				writelist([ExplainCount, ' ocurrences explained.']),nl,nl,
				!
		  	  	)
		  	;
		  	  nl,write('Wrong explain sentences'),nl,!
			)	
		;
			repeat,
			(
				toExplain(cause(Term, Label, Value, _RuleNumber2, Causes)),
				writeCauseTree(Term, Label, Value, Causes, 0),nl,
				fail
			;	true
			),!
		)
	;
		true
	).


% Writes an ASCII tree explanation for a cause
writeCauseTree(Term, no_label, Value, Causes, Level) :-
	!,
	% Root marker
	(
	  Level = 0 ->
		write('*')
	;
		true
	),


	% Node Term and value
	% If it is a boolean value change the output format
	(	
	  Value = true  -> 
		write(Term)
	; Value = false -> 
		writelist(['-',Term])
	;	
		writelist([Term,'=',Value])
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

writeCauseTree(Term, Label, Value, Causes, Level) :- 
	!,
	% Root marker
	(
	  Level = 0 ->
		write('*')
	;
		true
	),

	% Label
	writelist([' \033[1;33m',Label,'\033[0m ']),

	% Node Term and value
	(
		\+ opt(complete) ->
			true
	;
		% If it is a boolean value change the output format
		(	
		  Value = true  -> 
			write(Term)
		; Value = false -> 
			writelist(['-',Term])
		;	
			writelist([Term,'=',Value])
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




%%%%%%%%%%% HTML report output ECharts %%%%%%%%%%%%

makeReportDir :-
	opt(outfile(OutFile)),
	makeDirectory(OutFile).

makeReportDir :-
	\+ opt(outfile(_)).


writeReport :-
	(
	  current_predicate(cause/5) ->
	  	numsol(N),
	  	(
		opt(outfile(OutFile)) ->
	  		concat_atom([OutFile, '/'], OutDir)
	  	;
  			OutDir = ''
	  	),
	  	concat_atom([OutDir,'report',N], CompletePath),
	  	makeDirectory(CompletePath),

	  	write('Making graphs'),
	  	makeGraphs,nl,

	  	(
	  		\+ current_predicate(graphData/5),
	  		nl, write('Error making graphs'), nl
	  	;
		  	writelist(['Making html report',N]),
		  	makeReport,nl,

		  	(
			  	\+ opt(static_report),
			  	\+ opt(outfile(_)),
			  	concat_atom(['sensible-browser ', CompletePath, '/', CompletePath, '.html &'], Command),
			  	shell(Command)
		  	)
	  	)
	; 
		write('Report cant be written'),nl
	).

makeReport :-
	numsol(N),
	concat_atom(['report',N], ReportName),

	% Copy resources
  	(
		opt(outfile(OutFile)) ->
		concat_atom([OutFile, '/'], OutDir)
	;
		OutDir = ''
	),
	concat_atom([OutDir, ReportName,'/.resources'], ResDirectory),
	copy_directory('reportTemplate/resources', ResDirectory),

	concat_atom([OutDir, ReportName,'/',ReportName,'.html'], ReportFileName),
	open(ReportFileName, write, ReportFile),
	
	htmlReportTemplate(HtmlReportTemplate),
	
	
	(
		current_predicate(explainCount/1),
		explainCount(ExplainCount),
		concat_atom([ExplainCount, ' ocurrences explained.'], ExplainingResults)
	;
		ExplainingResults = 'All ocurrences explained.'
	),
	
	replaceString(HtmlReportTemplate, '#ResultsNumber#', ExplainingResults, AuxHtmlReport),

	repeat,
	(
		graphData(Term, Value, Label, _RuleNumber, Data),

		htmlReportRowTemplate(HtmlRowTemplate),

		replaceString(HtmlRowTemplate, '#GraphData#', Data, AuxString),
		(
			Label = no_label ->
			term_to_atom(Term, WTerm),
			concat_atom([WTerm, ' = ', Value], Title)	
		;
			term_to_atom(Label, WLabel),
			concat_atom([WLabel], Title)
		),
		
		% replace title token
		replaceString(AuxString, '#Title#', Title, AuxString2),

		% replce term token
		incr(graphId, 1),
		graphId(GraphId),
		concat_atom([GraphId, '_', Title], TermToken),
		replaceString(AuxString2, '#GraphId#', TermToken, Row),

		assert(reportRow(Row)),
		write('.'),flush_output(),
		fail
	;	true
	),!,

	(
	current_predicate(reportRow/1) ->
		findall(X, reportRow(X), RowList),
		concat_atom(RowList, Rows),
		replaceString(AuxHtmlReport, '#Terms#', Rows, ReadyHtml)
	;
		replaceString(AuxHtmlReport, '#Terms#', 'No results', ReadyHtml)
	),
	write(ReportFile, ReadyHtml),
	close(ReportFile).

makeGraphs :-
	(
	  current_predicate(explain/0) ->
		(
		  current_predicate(justExplain/1) ->
		  (
		  	repeat,
			(
				justExplain(Term),
				toExplain(cause(Term, Label, Value, RuleNumber, Causes)),
				
				makeGraph(Term, Label, Value, RuleNumber, Causes),

				incr(explainCount,1),
				fail
			;	true
			),
			explainCount(ExplainCount),
			writelist([ExplainCount, ' ocurrences explained.']),nl,
			!	
		  )
		;
			true
		)	
	;
		repeat,
		(
			toExplain(cause(Term, Label, Value, RuleNumber, Causes)),
			makeGraph(Term, Label, Value, RuleNumber, Causes), 
			fail
		;	true
		),!
	).

makeGraph(Term, Label, Value, RuleNumber, Causes) :-
	(
		Label = no_label ->
		term_to_atom(Term, WriteTerm),
		concat_atom(['', WriteTerm, ' = ', Value ,''], ItemName)
	;
		term_to_atom(Label, WriteTerm),
		concat_atom([WriteTerm, ''], ItemName)
	),

	% Build children graph data
	childrenData(Causes, Children),

	% Build Item Graph data
	concat_atom(['[ { name: "', ItemName, '", children:[', Children, '] }, ]'], Data),

	assert(graphData(Term, Value, Label, RuleNumber, Data)),
	write('.'),flush_output().


childrenData([Item|Tail], Data) :- 
	Item =.. [cause|[Term, Label, Value, _RuleNumber, Causes]],

	(
		Label = no_label ->
		term_to_atom(Term, WriteTerm),
		concat_atom(['', WriteTerm, ' = ', Value ,''], ItemName)
	;
		term_to_atom(Label, WriteTerm),
		concat_atom([WriteTerm, ''], ItemName)
	),

	% This item causes
	childrenData(Causes, Children),

	% Other causes graph data
	childrenData(Tail, OtherItems),

	% Build this item graph data
	concat_atom(['{ name:"', ItemName, '", children : [', Children, '] },'], ItemData),

	concat_atom([ItemData, OtherItems], Data).

childrenData(is_leaf, '').
childrenData([], '').




makeDirectory(Path) :-
	exists_directory(Path).

makeDirectory(Path) :-
	\+ exists_directory(Path),
	make_directory(Path).



%%%%%%%%%%%%% Equivalent explanations %%%%%%%%%%%%%

% Cuando es sin labels
skipEquivalentExplanations :-
	opt(complete),!,
	current_predicate(cause/5),
	repeat,
	(
		cause(TermFired, LabelFired, ValueFired, RuleNumber, Causes),
		Expl =.. [cause|[TermFired, LabelFired, ValueFired, RuleNumber, Causes]],
		assert(toExplain(Expl)),
		fail
	;	true
	),!.

% Cuando es con labels pero sin minimal
skipEquivalentExplanations :-
	\+ opt(complete),
	\+ opt(minimal_explanations),!,
	current_predicate(cause/5),
	repeat,
	(
		cause(TermFired, LabelFired, ValueFired, RuleNumber, Causes),
		Expl =.. [cause|[TermFired, LabelFired, ValueFired, RuleNumber, Causes]],
		\+ existsEquivalent(Expl),
		assert(toExplain(Expl)),
		fail
	;	true
	),!.

% Cuando es con labels y con minimal
skipEquivalentExplanations :-
	\+ opt(complete),
	opt(minimal_explanations),!,
	current_predicate(cause/5),
	repeat,
	(
		cause(TermFired, LabelFired, ValueFired, RuleNumber, Causes),
		Expl =.. [cause|[TermFired, LabelFired, ValueFired, RuleNumber, Causes]],
		(
			% Existe una explicación de la misma etiqueta
			toExplain(cause(TFOld, LabelFired, VFOld, RNOld, CausesOld)),
			depth(Causes, NewDepth),
			depth(CausesOld, OldDepth),
			NewDepth < OldDepth,
			retract(toExplain(cause(TFOld, LabelFired, VFOld, RNOld, CausesOld))),
			assert(toExplain(Expl))
		;
			\+ toExplain(cause(_, LabelFired, _, _, _)),
			assert(toExplain(Expl))
		),
		fail
	;	true
	),!.
	
% called only with opt(labels) active.
existsEquivalent(cause(_, no_label, _, _, _)) :-
	!,
	fail.

existsEquivalent(cause(_, LabelFired, _, _, Causes1)) :-
	toExplain(cause(_, LabelFired, _, _, Causes2)),
	getLabelTree(Causes1, Labels1),
	getLabelTree(Causes2, Labels2),
	sort(Labels1, Sorted1),
	sort(Labels2, Sorted2),
	Sorted1 = Sorted2.

getLabelTree([cause(_, Label, _, _, Causes)|T], [[Label,CTree]|MoreLabels]) :-
	getLabelTree(Causes, CTree),
	getLabelTree(T, MoreLabels).

getLabelTree(is_leaf, []).
getLabelTree([],[]).

depth(Causes, Depth) :-
	findall(D, (member(X, Causes), X =.. [cause|[_TF, _LF, _VF, _RN, C]], depth(C, D)), Depths),
	max_list(Depths, Aux),
	Depth is Aux+1.

depth([], 0).
depth(is_leaf, 0).