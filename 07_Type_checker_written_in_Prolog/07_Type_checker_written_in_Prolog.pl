%% Check member of a list or not (used for assumption)
member(_,[]) :- fail.
member(X,[X|Y]) :- !.
member(X,[Y|Z]) :- member(X,Z).

%% Rule for assumption
hastype(Gamma, X, T) :- member((X,T), Gamma).

mygamma([(v(x),int)]).