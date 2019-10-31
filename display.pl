debug(L):- writelist(L).
debug(_).

debugln(L):-writelist(L),nl.
debugln(_).

debug_expr_ln(E):- pretty_expr(E,F),write(F),nl.
debug_expr_ln(_).
