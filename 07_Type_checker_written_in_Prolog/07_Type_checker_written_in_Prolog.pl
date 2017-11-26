%% Check member of a list or not (used for assumption)
member(_,[]) :- fail.
member(X,[X|Y]) :- !.
member(X,[Y|Z]) :- member(X,Z).

%% Rule for assumption
hastype(Gamma, X, T) :- member((X,T), Gamma).

%% Rule for and introduction
hastype(Gamma, pair(E1, E2), and(T1, T2)):-
	hastype(Gamma, E1, T1),
	hastype(Gamma, E2, T2).

%% Rule for and elimination left
hastype(Gamma, projl(E), T1):-
	hastype(Gamma, E, and(T1, T2)).

%% Rule for and elimination right
hastype(Gamma, projr(E), T2):-
	hastype(Gamma, E, and(T1, T2)).

%% Rule for implies introduction
hastype(Gamma, lambda(X,E), arrow(T1, T2)):-
	hastype(Gamma, X, T1),
	hastype(Gamma, E, T2).

%% Rule for implies elimination
hastype(Gamma, apply(E1, E2), T2):-
	hastype(Gamma, E1, arrow(T1, T2)),
	hastype(Gamma, E2, T1).

%% Rule for or introduction left
hastype(Gamma, inl(E), or(T1, T2)):-
	hastype(Gamma, E, T1).

%% Rule for or introduction right
hastype(Gamma, inr(E), or(T1, T2)):-
	hastype(Gamma, E, T2).

mygamma([(v(x),int), (v(y),char)]).