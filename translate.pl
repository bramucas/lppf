:- dynamic varnum/1, prednum/1, body/2.
varnum(0).
prednum(0).

translate :-
  write('neg(true,false).\n'),
  write('neg(false,true).\n'),
  repeat,
    (show(F), write_show_clause(F),fail; true),!,
  repeat,
    (explainRule(Head, Body), write_explain_rule(Head, Body),fail; true),!, 
  repeat,
    ( fname(A),uniquevalue(A,UV),write_rule(UV),nl,fail; true),!,
  repeat,
    ( rule(C,H,B), writelist(['% ',H,' :- ',B]),nl,
	  set_count(varnum,0),
	  t_rule(H,B,Rs),
	  member(R,Rs),
	  remove_quantifiers([],R,Rs1),
	  lparse_divides(Rs1,Rs2),
	  member(RR,Rs2),
	  write_rule(RR,C),nl,
	  fail
	; true),!.

write_show_clause(F/N) :-
    (  fname(F/N),!,  % a function
       M is N+1,concat_atom(['holds_',F],G)      
     ; % a predicate
       concat_atom(['atom_',F],G),M=N),writelist(['#show ',G,'/',M,'.\n']).

write_explain_rule(OHead, OBody) :-
	t_rule(OHead, OBody, Rs),
	member(R,Rs),
	remove_quantifiers([],R,Rs1),
	lparse_divides(Rs1,Rs2),
	member(RR,Rs2),
	
	map_subterms(replacevars,[v/1,vaux/1],RR,Rule),
	Rule=(Head :- B),
    replace_not_eq(B,Body),

    Head =.. [FiredName|ArgsAndValue],
    concat_atom(['fired_',FName], FiredName),

    %quitar esto en el futuro
    append(Args, [_LastItem], ArgsAndValue),

    % Preparing head
    concat_atom(['explain_',FName], HeadFname),
    PreparedHead =.. [HeadFname|Args],

    % Preparing body
    concat_atom(['holds_',FName], BodyFname),
    append(Args, ['Value'], BodyArgs),
    BodyF =.. [BodyFname|BodyArgs],
    append(Body, [BodyF], PreparedBody),

    binop(',',PreparedBody,PreparedBody2),
    writelist([PreparedHead, ' :- ', PreparedBody2, '.\n']).



%%% TERMS %%%%%%%%%%%%

% t_term(T,T1,Gs)
% T - the original term
% T1 - the translation of T
% Gs - a list (conjunction) of formulas, [] means "true"

% LP terms

t_term(X,X,[]) :- number(X),!.
t_term(fterm(C,[]),C,[]) :- \+ fname(C/0),!.
t_term(v(X),v(X),[]):- !.


% Arithmetic terms
t_term(T,vaux(AUXVAR),[(vaux(AUXVAR)=T1)|Gs]) :-
	T =.. [Op,A,B],
	member(Op,['+','*']),!,
	newvar(AUXVAR),	t_term(A,A1,As), t_term(B,B1,Bs),
	T1=.. [Op,A1,B1], append(As,Bs,Gs).

t_term(A-B,vaux(AUXVAR),Gs) :-
	!,newvar(AUXVAR),	t_term(A,A1,As), t_term(B,B1,Bs),
	ATOM=(vaux(AUXVAR)=(A1-B1)),
	append_all([[ATOM],As,Bs],Gs).

t_term(A/B,vaux(AUXVAR),Gs) :-
	!,newvar(AUXVAR),	t_term(A,A1,As), t_term(B,B1,Bs),
	ATOM=divide(A1,B1,vaux(AUXVAR)),
	append_all([[ATOM],As,Bs],Gs).

t_term(A '\\ ' B,vaux(AUXVAR),Gs) :-
	!,newvar(AUXVAR),	t_term(A,A1,As), t_term(B,B1,Bs),
	ATOM=modulo(A1,B1,vaux(AUXVAR)),
	append_all([[ATOM],As,Bs],Gs).

% functional terms

t_term(fterm(F,Args),vaux(AUXVAR),[G|Gs]):-
	newvar(AUXVAR),
	t_terms(Args,Args1,Gs),
	append(Args1,[vaux(AUXVAR)],Args2),
	concat_atom([holds_,F],F1),G =.. [F1|Args2].

%t_term(neg(fterm(F,Args)),AUXVAR2,[G|Gs]):-
%	newvar(AUXVAR),
%	newvar(AUXVAR2),
%	Negation = neg(AUXVAR,AUXVAR2),
%	t_terms(Args,Args1,Gs),
%	append(Args1,[vaux(AUXVAR)],Args2),
%	concat_atom([holds_,F],F1),G =.. [F1|Args2].

t_term(neg(Fterm),AUXVAR,[neg(AUXVAR2,AUXVAR)|Gs]) :-
	newvar(AUXVAR),
	t_term(Fterm,AUXVAR2,Gs).

% A tuple of terms

t_terms([],[],[]) :- !.
t_terms([T|Ts],[S|Ss],Hs) :- t_term(T,S,Fs), t_terms(Ts,Ss,Gs), append(Fs,Gs,Hs).


%%% ATOMS %%%%%%%%%%%%

% t_atom(A,Fs)
% A - the atom
% Fs - a list (conjunction) of formulas (the atom translation)
%      possibly existentially quantified

t_atom(Term, ATOM ):- 
	Term=.. [RELOP,T1,T2],member(RELOP,[(=),(=\=),(>),(<),(>=)]),!,
    t_term(T1,S1,Fs1),
	t_term(T2,S2,Fs2),
	append(Fs1,Fs2,Fs),
	(RELOP=(=\=),!, A=(S1 '!=' S2)
	; A =.. [RELOP,S1,S2]
	),
	subterms([vaux/1],A,AUXVARS),
	( AUXVARS=[],Fs=[],!,ATOM=A; ATOM=exists(AUXVARS,[A|Fs])).
	
t_atom(A,ATOM):- !, 
	A =.. [P|Args], t_terms(Args,Args1,Fs), 
	concat_atom(['atom_',P],P1),
	A1 =.. [P1|Args1],
	subterms([vaux/1],A1,AUXVARS),
	( AUXVARS=[],!,ATOM=A1; ATOM=exists(AUXVARS,[A1|Fs])).	

% Lists of atoms
t_atoms([],[]):-!.
t_atoms([A|As],Hs):-t_atom(A,Fs), t_atoms(As,Gs),append(Fs,Gs,Hs).


%%% LITERALS  %%%%%%%%

t_literal(not A, not F) :- !,t_atom(A,A1), (A1=[L],!,F=L; F=A1).
t_literal(A,A1) :- t_atom(A,A1).

%%% BODIES %%%%%%%%%%%

t_body(B,B1) :- maplist(t_literal,B,B1).

%%% RULES %%%%%%%%%%%%

% t_rule(H,B,Rs)
% H - head
% B - body
% Rs - result of translation

:- dynamic localvar/2.
replace_var(v(X),v(AUXVAR)):-localvar(X,AUXVAR),!.
replace_var(v(X),v(X)).

% a constraint
t_rule([],B,[[] :- B1]) :- t_body(B,B1).

% Head atom
t_rule(predic(A),B,[(A1 :- B2)]) :-
    A =.. [P | Args],
	t_terms(Args,Args1,Gs),
	concat_atom(['atom_',P],P1),
	A1 =.. [P1 | Args1],
	t_body(B,B1),
	append(B1,Gs,B2).

% Default assignment	
t_rule(def_assign(Fterm,T),B,L) :-
	!, t_rule(assign(Fterm,T),[not Fterm=\=T|B],L).

%  Assignment
t_rule(assign(fterm(F,Args),T),B,[(A1 :- B1)]) :-
    concat_atom(['fired_',F],HOLDSF),
	t_terms(Args,Args1,Gs),
	t_term(T,T1,Hs),
	append(Args1,[T1],Args2),
	A1 =.. [HOLDSF | Args2],
	t_body(B,B0),
	append_all([B0,Gs,Hs],B1).

% Choice
/* term choices
t_rule(choice(FTerm,set(A,Cond)),B,Rs) :-
	A \= v(_),!,newvar(AUXVAR),
	append(Cond,[v(AUXVAR)=A],Cond2),
	t_rule(choice(FTerm,set(v(AUXVAR),Cond2)),B,Rs).
*/

t_rule(choice(fterm(F,Args),set(v(X),Cond)),B,[R1,R2,R3]) :-
    % Replace the local variable v(X) by a new auxiliary var
	!,newvar(AUXVAR),
    (retractall(localvar(_,_)),!; true),
	asserta(localvar(X,AUXVAR)),
	map_subterms(replace_var,[v/1],Cond,Cond2),

    % Translate the head functional term
    concat_atom(['fired_',F],HOLDSF),
	t_terms(Args,Args1,Gs),
	append(Args1,[v(AUXVAR)],Args2),
	A =.. [HOLDSF | Args2],

    % Creation of a choice: a pair of rules R1/R2 negative cycle
	t_body(B,B_t),
	t_body(Cond2,Cond_t),
	append_all([Gs,B_t,Cond_t],Body),
	concat_atom(['n',HOLDSF],NHOLDSF),
	A2 =.. [NHOLDSF | Args2],
	append(Body,[not A2],B2),
    R1 = (A :- B2),
	append(Body,[not A],B3),
	R2 = (A2 :- B3),
	% Create the constraint: take at least one value
	append_all([B_t,Gs,[not exists([v(AUXVAR)],[A|Cond_t])]],B4),
	R3=([] :- B4).

t_rule(choice(fterm(F,Args),set(List)),B,[R1,R2,R3|Rs]) :-
	!,newvar(AUXVAR),
	
    % Translate the head functional term
    concat_atom(['fired_',F],HOLDSF),
	t_terms(Args,Args1,Gs),
	append(Args1,[v(AUXVAR)],Args2),
	A =.. [HOLDSF | Args2],

	newpred(AUXPRED),
	freevars(List,VARs),
	ATOM =.. [AUXPRED,v(AUXVAR)|VARs],

    % Creation of a choice: a pair of rules R1/R2 negative cycle
	t_body(B,B_t),
	Cond_t=[ATOM],
	append_all([Gs,B_t,Cond_t],Body),
	concat_atom(['n',HOLDSF],NHOLDSF),
	A2 =.. [NHOLDSF | Args2],
	append(Body,[not A2],B2),
    R1 = (A :- B2),
	append(Body,[not A],B3),
	R2 = (A2 :- B3),
	% Create the constraint: take at least one value
	append_all([B_t,Gs,[not exists([v(AUXVAR)],[A|Cond_t])]],B4),
	R3=([] :- B4),
	
	% Elements of auxiliary predicate
	set_elements(List,AUXPRED,VARs,B_t,Rs).

set_elements([],_,_,_,[]):-!.
set_elements([T|Ts],AUXPRED,VARs,B_t,[(HEAD :- BODY)|Rs]):-
	t_term(T,T1,Gs),
	HEAD =.. [AUXPRED,T1|VARs],
	append(B_t,Gs,BODY),
	set_elements(Ts,AUXPRED,VARs,B_t,Rs).

%%% Preprocess choices for a list of terms
choice_list_preproc :-
	repeat,
	( rule(C,choice(FTerm,set(List)),Body),
	  retract(rule(C,choice(FTerm,set(List)),Body)),
	  create_choice_rules(C,FTerm,List,Body),
	  fail
	; true),!.

create_choice_rules(C,FTerm,List,Body):-
	newpred(AUXPRED),
	newvar(AUXVAR),
	freevars(List,VARS),
	append(VARS,[vaux(AUXVAR)],Vs),
	ATOM =..[AUXPRED|Vs],
	assertz(rule(C,choice(FTerm,set(vaux(AUXVAR),[ATOM])),Body)),
	assert_choice_terms(C,AUXPRED,VARS,List,Body).
assert_choice_terms(_C,_AUXPRED,_Vs,[],_Body):-!.
assert_choice_terms(C,AUXPRED,Vs,[T|Ts],Body):-
	append(Vs,[T],Args),
	ATOM =.. [AUXPRED|Args],
	assertz(rule(C,predic(ATOM),Body)),
	assert_choice_terms(C,AUXPRED,Vs,Ts,Body).




%%% REMOVING QUANTIFIERS: new auxiliary predicates
	
freevars(v(X),[v(X)]):-!.
freevars(vaux(X),[vaux(X)]):-!.
freevars(exists(Vs,F),Ws):-!,freevars(F,Us),subtract(Us,Vs,Ws).
freevars(T,Vs):-
	T =.. [_ | Args],!,
	freevars_args(Args,Vs).
freevars_args([],[]):-!.
freevars_args([F|Fs],Vs):-freevars(F,Vs0),freevars_args(Fs,Vs1),merge_set(Vs0,Vs1,Vs).

negative_formula(not _).
negative_exists(not exists(_,_)).

remove_quantifiers(Bp0,(H :- B),[(H :- Body) | Rs]):-
    remove_positive_exists(B,B1),
	filter(negative_formula,B1,Bn,Bp1),
	filter(negative_exists,Bn,Bne,Bn1),
	append(Bp0,Bp1,Bp),
	neg_exists(Bp,Bne,Bne1,Rs),
	append_all([Bp,Bn1,Bne1],Body).

remove_positive_exists([],[]):-!.
remove_positive_exists([exists(_Vs,F)|B],B2):-!,remove_positive_exists(B,B1),append(F,B1,B2).
remove_positive_exists([F|B],[F|B1]):-!,remove_positive_exists(B,B1).

neg_exists(_Bp,[],[],[]):-!.
neg_exists(Bp,[not exists(Vs,F)|Fs],[not A|Fs1],Rs):-
	neg_exists(Bp,Fs,Fs1,Rs0),
	freevars(exists(Vs,F),Xs),
	newpred(AUXPRED),
	A =.. [AUXPRED | Xs],
	Rule = (A :- F),
	remove_quantifiers(Bp,Rule,Rs1),
	append(Rs0,Rs1,Rs).

%%% LPARSE DIVISIONS
% The auxiliary 'divide' predicate must be made context dependent in order to avoid unsafe vars

lparse_divides([],[]):-!.
lparse_divides([(H :- B)|Rs],Us):-
	filter(iscontext,B,Context,_),
	freevars(Context,Vars),
	body_divides(Context,Vars,B,B1,Ss),
	lparse_divides(Rs,Ts),
	append(Ss,[(H:-B1)|Ts],Us).

iscontext(not _):-!,fail.
iscontext(divide(_,_,_)):-!,fail.
iscontext(modulo(_,_,_)):-!,fail.
iscontext(_).

body_divides(_Context,_Vars,[],[],[]):-!.
body_divides(Context,Vars,[LIT|B],[DIVATOM|B1],[Rule1,Rule2|Rs]):-
	divideop(LIT,X,Y,Z, RESULT),!,
	newpred(DEFDIVIDE),	DEFDIVATOM =.. [DEFDIVIDE | Vars],
	append(Context,[Y '!=' 0],Cond),
	Rule1 = (DEFDIVATOM :- Cond),
	newpred(DIVIDE), DIVATOM =.. [DIVIDE,X,Y,Z],
	append(Context,[DEFDIVATOM, RESULT],Cond2),
	Rule2 = (DIVATOM :- Cond2),
	body_divides(Context,Vars,B,B1,Rs).
	
body_divides(Context,Vars,[Lit|B],[Lit|B1],Rs):-body_divides(Context,Vars,B,B1,Rs).

divideop(divide(X,Y,Z),X,Y,Z,Z=X/Y).
divideop(modulo(X,Y,Z),X,Y,Z,Z=(X '\\ ' Y)). % modulo

%%% OTHER PREDICATES

newvar(AUXVAR):-varnum(X),incr(varnum,1),concat_atom(['AUX',X],AUXVAR).
newpred(AUXPRED) :- prednum(N),incr(prednum,1), concat_atom(['aux',N],AUXPRED).

replacevars(v(X),V):-concat_atom(['VAR',X],V).
replacevars(vaux(X),X).

write_rule(R) :-
	map_subterms(replacevars,[v/1,vaux/1],R,R1),
	R1=(H :- B),
    replace_not_eq(B,B1),
	(H = [],!; write(H)),
	(B1=[],!; write(' :- '),binop(',',B1,B2),write(B2)),
	write('.').

% write_rule(Rule, Code)
%	Writes the fired and holds rule for an input rule.
%		- Rule: The translation of the original rule.
%		- Code: format = Label/RuleNumber/LineNumber.
write_rule(R, Label/RuleNum/_LineNum) :-
	map_subterms(replacevars,[v/1,vaux/1],R,Rule),
	Rule = (Head :- B),
    replace_not_eq(B, Body),
	
	% Head of fired rule
	Head =.. [RuleFname | Args],
	(
		% auxiliary predicates
		sub_string(RuleFname,_,3,_,"aux") ->
		write(Head)
	;
		% Adding extra arguments
		body_variables(Body, BodyVariables),
		subtract(BodyVariables, Args, ExtraArgs),
		append(ExtraArgs, Args, All),

		% Write fired rule head
		concat_atom(['fired_',RuleNum],FiredFname),
		FiredHead =.. [FiredFname | All],
		write(FiredHead)	
	),
	
	% Body of fired rule
	(
		Body=[],!
	;
		write(' :- '),
		binop(',', Body, PreparedBody),
		write(PreparedBody)
	),
	write('.'),nl,

	(
		sub_string(RuleFname,_,3,_,"aux") ->
		true
	;
		% Write holds rule
		append(TrueArgs, [_Value], Args),
		concat_atom(['fired_',Fname],RuleFname), 
		write_holds(Fname, TrueArgs ,FiredHead),

		% Saving rule info with the ruleNumber
		prepareLabel(Label, PreparedLabel),
		append(FiredVarsNoValue, [_Value2], All),
		OriginalTerm =.. [Fname|TrueArgs],

		assert(ruleInfo(RuleNum, PreparedLabel, OriginalTerm, FiredVarsNoValue, Body))
	).

% write_holds(Fname, FTrueArgs, FiredHead)
%	Write the holds rule for a fired rule.
%		- Fname: fname of the original function
%		- FTrueArgs: arguments of the original function
%		- FiredHead: Head of fired rule.
write_holds(Fname, FTrueArgs, FiredHead) :-
	% Head
	concat_atom(['holds_',Fname], HoldsFname),
	append(FTrueArgs, ['Value'], HoldsArgs),
	Head =.. [HoldsFname | HoldsArgs],
	
	% Body
	FiredHead =.. [FiredFname|FiredArgs],
	append(FiredArgsNoValue, [_Value], FiredArgs),
	append(FiredArgsNoValue, ['Value'], NewFiredArgs),
	Body =.. [FiredFname|NewFiredArgs],

	writelist([Head, ' :- ', Body, '.']).

replace_not_eq([],[]):-!.
replace_not_eq([not (A=B)|Ls],[A '!=' B|Ms]):-!,replace_not_eq(Ls,Ms).
replace_not_eq([A|Ls],[A|Ms]):-!,replace_not_eq(Ls,Ms).

uniquevalue(F/N,R):-
	set_count(varnum,0),
	vartuple(N,Vs),
	newvar(X),newvar(Y),
	concat_atom(['holds_',F],HOLDSF),
	append(Vs,[X],Vs1),
	A1 =.. [HOLDSF | Vs1],
	append(Vs,[Y],Vs2),
	A2 =.. [HOLDSF | Vs2],
	R=([] :- [A1,A2, X '!=' Y]).
	
vartuple(0,[]):-!.
vartuple(N,[V|Vs]):-newvar(V),M is N-1, vartuple(M,Vs).

% Prepare a label to be processed on solutions module.
prepareLabel(Label, PreparedLabel) :-
	Label =.. [_F|[LabelFname, LabelVars]],
	orderedAuxVarNames(LabelVars, PreparedLabelVars),
	PreparedLabel =.. [LabelFname|PreparedLabelVars].

orderedAuxVarNames([v(X)|T], [Var|VarNames]) :-
	concat_atom(['VAR',X], Var),
	orderedAuxVarNames(T, VarNames).
orderedAuxVarNames([],[]).

% body_variables(Body, Arguments)
%	Return all 'holds predicates arguments' (not the value) for a given translated body.
body_variables([HTerm|Tail], Arguments) :-
	HTerm =.. [Fname|_Args],
	\+ concat_atom(['holds_',_F],Fname),
	body_variables(Tail, Arguments).

body_variables([HTerm|Tail], Arguments) :-
	HTerm =.. [Fname|ArgsAndValue],
	concat_atom(['holds_',_F],Fname),
	append(Args, [_Value], ArgsAndValue),
	body_variables(Tail, MoreArgs),
	merge_set(Args, MoreArgs, Arguments).

body_variables([],[]).
