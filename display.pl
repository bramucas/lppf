debug(L):- opt(debug),!,writelist(L).
debug(_).

debugln(L):- opt(debug),!,writelist(L),nl.
debugln(_).

debug_expr_ln(E):- opt(debug),!,pretty_expr(E,F),write(F),nl.
debug_expr_ln(_).
