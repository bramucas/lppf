:- dynamic to_explain/1, graph_id/1, graph_data/5, cause/5, explain_count/1, causal_term/2, report_row/1.



%%%%%%%%%%%%% Causes search %%%%%%%%%%%%%%


% findCauses
%	Find the causes of the fired facts and build their causal trees.
find_causes :-
	% Facts first (rules without body)
	repeat,
	(
		fired(RuleNumber, ArgValues),
		rule_info(RuleNumber, OriginalLabel, OriginalTerm, VarNames, []),

		replace_values(ArgValues, VarNames, [OriginalTerm], [TermFired]),

		(
			opt(causal_terms), OriginalLabel =.. [text|_] ->
				AuxLabel = no_label
			;
				AuxLabel = OriginalLabel
		),

		% Automatically label no-labeled-facts if option label facts is active
		(
			AuxLabel = no_label, (opt(label_facts); opt(complete)) ->
				append(_Arguments, [Value], ArgValues),
				term_value(TermFired, Value, LabelFired)
			;
				process_label(AuxLabel, TermFired, ArgValues, VarNames, LabelFired)
		),

		append(_Args2, [ValueFired], ArgValues),

		add_explanation(cause(TermFired, LabelFired, ValueFired, RuleNumber, is_leaf)),
		fail
	;	
		true
	),!,
	% Rules with body
	repeat,
	(
		fired(RuleNumber, ArgValues),
		rule_info(RuleNumber, Label, OriginalTerm, VarNames, Body),

		(
			Body = [] -> true
		;	
			get_causes(ArgValues, VarNames, Body, Causes),

			replace_values(ArgValues, VarNames, [OriginalTerm], [TermFired]),

			(
				opt(causal_terms), Label =.. [text|_] ->
					AuxLabel = no_label
				;
					AuxLabel = Label
			),

			(
				AuxLabel = no_label, opt(complete),
					append(_Arguments2, [Value], ArgValues),
					term_value(TermFired, Value, LabelFired)
				;
					process_label(AuxLabel, TermFired, ArgValues, VarNames, LabelFired)
			),
			

			append(_Args, [ValueFired], ArgValues),

			add_explanation(cause(TermFired, LabelFired, ValueFired, RuleNumber, Causes))
		),
		fail
	;	true
	),!.


add_explanation(Expl) :-
	\+ Expl,!,
	assert(Expl).

add_explanation(Expl) :-
	Expl.


% get_causes(ArgValues, VarNames, Body, Causes)
%	Return the causes (cause/5) that make true a given body with a given pair of Variable-Value.
%		- ArgValues: List of values for the variables in 'VarNames' (order matters).
%		- VarNames: List of variable names that you can find in the body.
%		- Body
%		- Causes (return)
get_causes(ArgValues, VarNames, Body, TrimmedCauses) :-
	simplify_body(Body, SimplifiedBody),
	replace_values(ArgValues, VarNames, SimplifiedBody, Result),!,
	evaluate_causes_labels(Result, Causes0),
	trim_causes_labels(Causes0, Causes),
	sort(Causes, TrimmedCauses).

% If cause has not label
evaluate_causes_labels([HTerm|Tail], MoreCauses) :-
	cause(HTerm, no_label, _Value, _RuleNumber, TermCauses),!,
	evaluate_causes_labels(Tail, TailCauses),
	(
		TermCauses=is_leaf ->
			MoreCauses = TailCauses
	;
		append(TermCauses, TailCauses, MoreCauses)
	).

% If cause has label
evaluate_causes_labels([HTerm|Tail], [Cause|MoreCauses]) :-
	cause(HTerm, Label, Value, RuleNumber, TermCauses),
	Cause =.. [cause|[HTerm, Label, Value, RuleNumber, TermCauses]],
	evaluate_causes_labels(Tail, MoreCauses).

evaluate_causes_labels([],[]).

% replace_values(ArgValues, VarNames, TermList, Result)
%	Replace variables inside 'TermList' that match with anyone in 'VarNames' with the value in 'ArgValues' (based in order).
%	Arguments that doesn't appear in 'VarNames' will be ignored.
%		- ArgValues: List of values for the variables in 'VarNames' (order matters).
%		- VarNames: List of variable names that you can find in the body.
%		- TermList
%		- Result (return)
replace_values(ArgValues, VarNames, [Item|T], [NewItem|Result]) :-
	Item =.. [F|Args],
	get_var_values(ArgValues, VarNames, Args, Values),
	NewItem =.. [F|Values],
	replace_values(ArgValues, VarNames, T, Result).
replace_values(_, _, [], []):-!.
replace_values([], _, _, []):-!.
replace_values(_, [], _, []):-!.


% get_var_values(ArgValues, VarNames, VarList, Result).
%	Return a list of values for a given var list, based in 'VarNames' and 'ArgValues'.
%		- ArgValues: List of values for the variables in 'VarNames' (order matters).
%		- VarNames: List of variable names that you can find in the body.
%		- VarList: List of variables that will be replaced.
%		- Result (return): List of values.
get_var_values(ArgValues, VarNames, [Var|T], [Value|Rest]) :-
	Var = 'VAR_Value',!,
	last(ArgValues, Value),
	get_var_values(ArgValues, VarNames, T, Rest).	


get_var_values(ArgValues, VarNames, [Var|T], [Var|Rest]) :-
	\+ nth0(_Position, VarNames, Var),!,
	get_var_values(ArgValues, VarNames, T, Rest).

get_var_values(ArgValues, VarNames, [Var|T], [Value|Rest]) :-
	nth0(Position, VarNames, Var),
	nth0(Position, ArgValues, Value),!,
	get_var_values(ArgValues, VarNames, T, Rest).

get_var_values(_, _, [], []).


% simplify_body(Original, SimplifiedBody)
%	Just keeps 'holds_' form terms, and eliminate the last argument of same ones.
simplify_body([H|T], [Simplified|ResultBody]) :-
	H =.. [F|Args],
	concat_atom(['holds_',Fname],F),
	append(Vars, [_Value], Args),
	Simplified =.. [Fname|Vars],
	simplify_body(T, ResultBody).

simplify_body([H|T], ResultBody) :-
	H =.. [F|_Args],
	\+ concat_atom(['holds_',_Fname],F),
	simplify_body(T, ResultBody).

simplify_body([],[]).


trim_causes_labels([cause(HTerm, Label, Value, RuleNumber, TermCauses)|Tail], [cause(HTerm, Label, Value, RuleNumber, TermCauses)|TrimmedCauses]) :-
	findall(C, (member(C, Tail), C =..[cause|[_CTerm, CLabel, _CValue, _RuleNumber, CTermCauses]], Label = CLabel, TermCauses = CTermCauses), BranchesToTrim),
	subtract(Tail, BranchesToTrim, TrimmedTail),
	trim_causes_labels(TrimmedTail, TrimmedCauses).

trim_causes_labels([],[]).



%%%%%%%%%% Processing labels %%%%%%%%%%%%%%%

% process_label(Label, TermFired, Values, Names, LabelFired)
%	Get the labels of a Fired Term
%		- Label: individual label
process_label(no_label, TermFired, _, _, no_label) :-
	\+ opt(complete), \+ label(TermFired,_).

% functional label
process_label(label(OriginalLabel), _, Values, Names, LabelFired) :-
	prepare_label(OriginalLabel, PreparedLabel),
	replace_values(Values, Names, [PreparedLabel], [LabelFired]).

% textual label
process_label(text(OriginalLabel), _, Values, Names, LabelFired) :-
	get_var_references(OriginalLabel, References),
	maplist(name, Names, L),
	maplist(string_codes, VarNamesStrings, L),
	replace_all_references(Values, VarNamesStrings, References, OriginalLabel, NaturalText),
	LabelFired = NaturalText.

% use labels generated by #label rules
process_label(_, TermFired, _, _, LabelFired) :-
	label(TermFired, LabelFired).


% Prepare variable names of a non-processed label to be replaced for its values.
prepare_label(Label, PreparedLabel) :-
	Label =.. [_F|[LabelFname, LabelVars]],
	ordered_aux_var_names(LabelVars, PreparedLabelVars),
	PreparedLabel =.. [LabelFname|PreparedLabelVars].

% Variables
ordered_aux_var_names([v(X)|T], [Var|VarNames]) :-
	concat_atom(['VAR',X], Var),!,
	ordered_aux_var_names(T, VarNames).

% Constants
ordered_aux_var_names([X|T], [Var|VarNames]) :-
	X =.. [fterm|[Var,_]],
	ordered_aux_var_names(T, VarNames).

% Base case
ordered_aux_var_names([],[]).


% Text labels
replace_all_references([], _, _, NaturalExplanation, NaturalExplanation).
replace_all_references(_, _, [], NaturalExplanation, NaturalExplanation).


replace_all_references(ArgValues, VarNames, [HRef|Tail], Text, NaturalExplanation) :-
	HRef = "_Value",!,
	last(ArgValues, Value),
	string_concat("%", HRef, Reference),
	replace_string(Text, Reference, Value, NewText),!,
	replace_all_references(ArgValues, VarNames, Tail, NewText, NaturalExplanation).

replace_all_references(ArgValues, VarNames, [HRef|Tail], Text, NaturalExplanation) :-
	string_concat("VAR", HRef, VarName),
	nth0(I, VarNames, VarName),
	nth0(I, ArgValues, Value),
	string_concat("%", HRef, Reference),
	replace_string(Text, Reference, Value, NewText),!,
	replace_all_references(ArgValues, VarNames, Tail, NewText, NaturalExplanation).

replace_all_references(ArgValues, VarNames, [HRef|Tail], Text, NaturalExplanation) :-
	string_concat("VAR", HRef, VarName),
	\+ nth0(_I, ArgValues, VarName),
	replace_all_references(ArgValues, VarNames, Tail, Text, NaturalExplanation).	

% Find the '%VAR' references in the textual label and returns them in a list
get_var_references(Text, Vars) :-
	string_chars(Text, Chars),
	findall(Index, nth0(Index, Chars, '%'), Indexes),
	aux_get_var_references(Indexes, Chars, References),
	sort_atoms_by_length(References, Sorted),
	reverse(Sorted, Vars).

aux_get_var_references([HI|Tail], Chars, [Var|MoreVars]) :-
	Index is HI +1,
	var_reference(Index, Index, Chars, Var),
	aux_get_var_references(Tail, Chars, MoreVars).

aux_get_var_references([], _, []).


var_reference(First, Index, Chars, Reference) :-
	nth0(Index, Chars, Char),
	char_code(Char, Code),
	ref_code(Code),!,
	Next is Index+1,
	var_reference(First, Next, Chars, Reference).

var_reference(First, Index, Chars, Reference) :-
	nth0(Index, Chars, Char),
	char_code(Char, Code),
	\+ ref_code(Code),!,	% delimitador
	Len is Index-First,
	string_chars(String, Chars),
	sub_string(String, First, Len, _, Reference).

var_reference(First, Index, Chars, Reference) :-
	\+ nth0(Index, Chars, _Char),	% final de string
	Len is Index-First,
	string_chars(String, Chars),
	sub_string(String, First, Len, _, Reference).

% Chars that are allowed to be part of a variable name inside textual labels
ref_code(241).	% ñ
ref_code(209).	% Ñ
ref_code(95).	% _
ref_code(C) :- C>47, C<58.	% 0-9
ref_code(C) :- C>96, C<122.	% a-z
ref_code(C) :- C>64, C<91.	% A-Z


%%%%%%%%%%%%% ASCII trees output %%%%%%%%%%%%%

% writeCauses
% Write ASCII trees for explanations.
write_causes :-
	(	% If there is some #explain sentence
		explain,
	  	(
	  		repeat,
			(
		    	just_explain(Term),
			  	to_explain(cause(Term, Label, Value, _RuleNumber, Causes)),
			  	write_cause_tree(Term, Label, Value, Causes, 0),nl,
			  	incr(explain_count,1),
			  	fail
			; 
				true
			),

			explain_count(ExplainCount),
			writelist([ExplainCount, ' ocurrences explained.']),nl,nl,
			!
	  	;
	  	  	% There is some #explain sentence but it does not match any derived atom.
	  	  	\+ just_explain(_),nl,write('Wrong explain sentences'),nl,!
		)	
	;	
		% If there is not any #explain sentence
		\+ explain,
		repeat,
		(
			to_explain(cause(Term, Label, Value, _RuleNumber2, Causes)),
			write_cause_tree(Term, Label, Value, Causes, 0),nl,
			fail
		;	
			true
		),!
	).


% Writes an ASCII tree explanation for a cause
write_cause_tree(Term, no_label, Value, Causes, Level) :-
	!,
	% Root marker
	(
	  Level = 0 ->
		write('*')
	;
		true
	),


	% Node Term and value
	term_value(Term, Value, TermAndValue),
	write(TermAndValue),nl,
	
	% Causes
	(
	  Causes = [] ->
		!,true
	;
		repeat,
		(
			member(C, Causes),
			C =.. [cause|[CTerm, CLabel, CValue, _RuleNumber, CTermCauses]],
			write_branch(Level),
			NextLevel is Level+1,
			write_cause_tree(CTerm, CLabel, CValue, CTermCauses,NextLevel),
			fail
		;	
			true
		),!
	).	

write_cause_tree(_Term, Label, _Value, Causes, Level) :- 
	!,
	% Root marker
	(
	  Level = 0 ->
		write('*')
	;
		true
	),

	% Label
	writelist([' \033[1;33m',Label,'\033[0m \n']),

	% Causes
	(
	  Causes = [] ->
		!,true
	;
		repeat,
		(
			member(C, Causes),
			C =.. [cause|[CTerm, CLabel, CValue, _RuleNumber, CTermCauses]],
			write_branch(Level),
			NextLevel is Level+1,
			write_cause_tree(CTerm, CLabel, CValue, CTermCauses,NextLevel),
			fail
		;	
			true
		),!
	).


write_branch(Level) :-
	write_birth(Level),
	write('-- ').

write_birth(0) :-
	write(' |'),!.
write_birth(Level) :-
	write(' |   '),
	NextLevel is Level-1,
	write_birth(NextLevel).




%%%%%%%%%%% HTML report output ECharts %%%%%%%%%%%%

make_report_dir :-
	opt(outfile(OutFile)),
	make_directory(OutFile).

make_report_dir :-
	\+ opt(outfile(_)).


write_report :-
	(
	  	numsol(N),
	  	(
			opt(outfile(OutFile)),
	  		concat_atom([OutFile, '/'], OutDir)
	  	;	
	  		\+ opt(outfile(_)),
  			OutDir = ''
	  	),
	  	concat_atom([OutDir,'report',N], CompletePath),
	  	make_directory(CompletePath),

	  	write('Making graphs'),
	  	make_graphs,nl,

	  	(
	  		\+ graph_data(_,_,_,_,_),
	  		nl, write('Error making graphs'), nl
	  	;
		  	writelist(['Making html report',N]),
		  	make_report,nl,

		  	(
			  	\+ opt(static_report),
			  	\+ opt(outfile(_)),
			  	concat_atom(['sensible-browser ', CompletePath, '/', CompletePath, '.html &'], Command),
			  	shell(Command)
		  	)
	  	)
	;
		% If there is no causes
		\+ cause(_,_,_,_,_),
		write('Report cant be written'),nl
	).

make_report :-
	numsol(N),
	concat_atom(['report',N], ReportName),

	% Copy resources
  	(
		opt(outfile(OutFile)),
		concat_atom([OutFile, '/'], OutDir)
	;
		\+ opt(outfile(_)),
		OutDir = ''
	),
	concat_atom([OutDir, ReportName,'/.resources'], ResDirectory),
	copy_directory('reportTemplate/resources', ResDirectory),

	concat_atom([OutDir, ReportName,'/',ReportName,'.html'], ReportFileName),
	open(ReportFileName, write, ReportFile),
	
	html_report_template(HtmlReportTemplate),
	
	
	(
		explain_count(ExplainCount),
		concat_atom([ExplainCount, ' ocurrences explained.'], ExplainingResults)
	;
		\+ explain_count(_),
		ExplainingResults = 'All ocurrences explained.'
	),
	
	replace_string(HtmlReportTemplate, '#ResultsNumber#', ExplainingResults, AuxHtmlReport),

	repeat,
	(
		graph_data(Term, Value, Label, _RuleNumber, Data),

		html_report_row_template(HtmlRowTemplate),

		replace_string(HtmlRowTemplate, '#GraphData#', Data, AuxString),
		(
			Label = no_label ->
			term_to_atom(Term, WTerm),
			concat_atom([WTerm, ' = ', Value], Title)	
		;
			term_to_atom(Label, WLabel),
			concat_atom([WLabel], Title)
		),
		
		% replace title token
		replace_string(AuxString, '#Title#', Title, AuxString2),

		% replce term token
		incr(graph_id, 1),
		graph_id(GraphId),
		concat_atom([GraphId, '_', Title], TermToken),
		replace_string(AuxString2, '#GraphId#', TermToken, Row),

		assert(report_row(Row)),
		write('.'),flush_output(),
		fail
	;	true
	),!,

	(
		findall(X, report_row(X), RowList),
		concat_atom(RowList, Rows),
		replace_string(AuxHtmlReport, '#Terms#', Rows, ReadyHtml)
	;
		\+ report_row(_),
		replace_string(AuxHtmlReport, '#Terms#', 'No results', ReadyHtml)
	),
	write(ReportFile, ReadyHtml),
	close(ReportFile).

make_graphs :-
	(
		% There is some #explain sentence
	  	explain,
		(

		  	repeat,
			(
				just_explain(Term),
				to_explain(cause(Term, Label, Value, RuleNumber, Causes)),
				
				make_graph(Term, Label, Value, RuleNumber, Causes),

				incr(explain_count,1),
				fail
			;	true
			),
			explain_count(ExplainCount),
			writelist([ExplainCount, ' ocurrences explained.']),nl,
			!	
		;
			% There is some #explain sentence but it does not match any derived atom.
	  	  	\+ just_explain(_),nl,write('Wrong explain sentences'),nl,!
		)	
	;
		% There is any #explain sentence
		\+ explain,
		repeat,
		(
			to_explain(cause(Term, Label, Value, RuleNumber, Causes)),
			make_graph(Term, Label, Value, RuleNumber, Causes), 
			fail
		;	true
		),!
	).

make_graph(Term, Label, Value, RuleNumber, Causes) :-
	(
		Label = no_label ->
		term_to_atom(Term, WriteTerm),
		concat_atom(['', WriteTerm, ' = ', Value ,''], ItemName)
	;
		term_to_atom(Label, WriteTerm),
		concat_atom([WriteTerm, ''], ItemName)
	),

	% Build children graph data
	children_data(Causes, Children),

	% Build Item Graph data
	concat_atom(['[ { name: "', ItemName, '", children:[', Children, '] }, ]'], Data),

	assert(graph_data(Term, Value, Label, RuleNumber, Data)),
	write('.'),flush_output().


children_data([Item|Tail], Data) :- 
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
	children_data(Causes, Children),

	% Other causes graph data
	children_data(Tail, OtherItems),

	% Build this item graph data
	concat_atom(['{ name:"', ItemName, '", children : [', Children, '] },'], ItemData),

	concat_atom([ItemData, OtherItems], Data).

children_data(is_leaf, '').
children_data([], '').




make_directory(Path) :-
	exists_directory(Path).

make_directory(Path) :-
	\+ exists_directory(Path),
	make_directory(Path).


%%%%%%%%%%%%% Causal terms %%%%%%%%%%%%%

make_causal_terms :-
	(
		% If there is some #explain sentence
		explain,
	  	(
	  		% Explaining everything derived from #explain sentences (T from just_explain(T))
	  		repeat,
			(
		    	just_explain(Term),
			  	c_term(Term,  CTerm),

				% Assert causal term
				distinct(Value, to_explain(cause(Term, _L, Value, _RN, _C))),
				term_value(Term, Value, TermValue),
				assert(causal_term(TermValue, CTerm)),
			  	fail
			; 	true
			)
	  	;
	  		% There is some #explain sentence but it does not match any derived atom.
			\+ just_explain(_), nl, write('Wrong explain sentences'), nl,!
		)	
	;
		% If there isn't any #explain setnence then explain everything
		\+ explain,
		repeat,
		(
			% Get causal term
			distinct(Term, to_explain(cause(Term, _Label, Value, _RuleNumber2, _Causes))),
			c_term(Term,  CTerm),

			% Assert causal term
			term_value(Term, Value, TermValue),
			assert(causal_term(TermValue, CTerm)),
			fail
		;	true
		),!
	).

print_causal_terms :-
	% Printing causal terms
	(
		findall([T,CT], causal_term(T,CT), CTList),
		sort(CTList, SortedCTList),
		maplist(printct_, SortedCTList)
	;
		\+ causal_term(_,_),
		true
	).

printct_([T, CT]) :-
	writelist([T, '\n\t', CT, '\n']).


c_term(T, CTerm) :-
	to_explain(cause(T, L, _V, _RN, C)),
	C=is_leaf,
	(
		L = no_label ->
			CTerm = '1'
		;
			CTerm = L
	).


c_term(T, CTerm) :-
	findall(A, 
		(
			(
				to_explain(cause(T, no_label, _V, _RN2, C)),
				dif(C,is_leaf),
				(
					C=[],A='1'
					;
					\+ C=[], 
					causal_joint(C, J),
					sort(J, SortedJ),
					binop('*', SortedJ, A)
				)
			)
		),
		NoLabelAlternatives),

	findall(A,
		(
			distinct(L, (to_explain(cause(T, L, _, _, C)), dif(C, is_leaf), dif(L, no_label) )),
			findall(J, 
				(distinct(J, (to_explain(cause(_, L, _, _, C2)), causal_joint(C2, JList), sort(JList, SortedJList), binop('*', SortedJList, J)) )), 
				Joints),
			(
				Joints = [] ->
					A = L
				;
					binop('+', Joints, Alt),
					binop('·', [Alt, L], A)
				)
		),
		LabelAlternatives),

	append(NoLabelAlternatives, LabelAlternatives, Alternatives),
	sort(Alternatives, SortedAlternatives),
	binop('+', SortedAlternatives, CTerm).


causal_joint([], []).
causal_joint([HCause|TailCauses], [J|JTail]) :-
	HCause =.. [cause|[T,_L,_V,_RN, _C]],
	c_term(T, J),
	causal_joint(TailCauses, JTail).



%%%%%%%%%%%%% Equivalent explanations %%%%%%%%%%%%%

% Cuando es sin labels
skip_equivalent_explanations :-
	(opt(complete);opt(causal_terms)),!,
	repeat,
	(
		cause(TermFired, LabelFired, ValueFired, RuleNumber, Causes),
		Expl =.. [cause|[TermFired, LabelFired, ValueFired, RuleNumber, Causes]],
		assert(to_explain(Expl)),
		fail
	;	true
	),!.

% Cuando es con labels pero sin minimal
skip_equivalent_explanations :-
	\+ opt(complete), \+ opt(causal_terms),
	\+ opt(minimal_explanations),!,
	repeat,
	(
		cause(TermFired, LabelFired, ValueFired, RuleNumber, Causes),
		Expl =.. [cause|[TermFired, LabelFired, ValueFired, RuleNumber, Causes]],
		\+ exists_equivalent(Expl),
		assert(to_explain(Expl)),
		fail
	;	true
	),!.

% Cuando es con labels y con minimal
skip_equivalent_explanations :-
	\+ opt(complete), \+ opt(causal_terms),
	opt(minimal_explanations),!,
	repeat,
	(
		cause(TermFired, LabelFired, ValueFired, RuleNumber, Causes),
		Expl =.. [cause|[TermFired, LabelFired, ValueFired, RuleNumber, Causes]],
		(
			% Existe una explicación de la misma etiqueta
			to_explain(cause(TFOld, LabelFired, VFOld, RNOld, CausesOld)),
			depth(Causes, NewDepth),
			depth(CausesOld, OldDepth),
			NewDepth < OldDepth,
			retract(to_explain(cause(TFOld, LabelFired, VFOld, RNOld, CausesOld))),
			assert(to_explain(Expl))
		;
			\+ to_explain(cause(_, LabelFired, _, _, _)),
			assert(to_explain(Expl))
		),
		fail
	;	true
	),!.
	
% called only with opt(labels) active.
exists_equivalent(cause(_, no_label, _, _, _)) :-
	!,
	fail.

exists_equivalent(cause(_, LabelFired, _, _, Causes1)) :-
	to_explain(cause(_, LabelFired, _, _, Causes2)),
	get_label_tree(Causes1, Labels1),
	get_label_tree(Causes2, Labels2),
	sort(Labels1, Sorted1),
	sort(Labels2, Sorted2),
	Sorted1 = Sorted2.

get_label_tree([cause(_, Label, _, _, Causes)|T], [[Label,CTree]|MoreLabels]) :-
	get_label_tree(Causes, CTree),
	get_label_tree(T, MoreLabels).

get_label_tree(is_leaf, []).
get_label_tree([],[]).

depth(Causes, Depth) :-
	findall(D, (member(X, Causes), X =.. [cause|[_TF, _LF, _VF, _RN, C]], depth(C, D)), Depths),
	max_list(Depths, Aux),
	Depth is Aux+1.

depth([], 0).
depth(is_leaf, 0).