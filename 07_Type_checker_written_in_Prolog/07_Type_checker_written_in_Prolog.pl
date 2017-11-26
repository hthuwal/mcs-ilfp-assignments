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

%% Rule for or elimination
hastype(Gamma, case(E0, X, Y, EX, EY), T3):-
	hastype(Gamma, X, T1), 
	hastype(Gamma, Y, T2),
	hastype(Gamma, EX, T3),
	hastype(Gamma, EY, T3),
	hastype(Gamma, inl(E0), or(T1, T2)) | 
	hastype(Gamma, inr(E0), or(T1, T2)).

mygamma([(4,int), (v(x),int), (x,char), (v(y),char)]).

%% Test Queries

%% mygamma(G), hastype(G, pair(4, v(y)), and(int, char)).
%% mygamma(G), hastype(G, pair(v(x), v(y)), and(int, char)).
%% hastype(G, pair(v(x), v(y)), and(int, char)).
%% mygamma(G), hastype(G, pair(v(x), 4), and(int, char)).
%% hastype(G, pair(v(x), 4), and(int, char)).
%% mygamma(G), hastype(G, pair(X, 4), and(int, char)). %% causes infinite loop
%% mygamma(G), hastype(G, pair(X, Y), and(int, char)).


%% mygamma(G), hastype(G, projl(pair(4,4)), int).
%% mygamma(G), hastype(G, projl(pair(4,4)), X).
%% mygamma(G), hastype(G, projl(pair(v(y),4)), X).
%% hastype(G, projl(pair(v(y),4)), char).
%% mygamma(G), hastype(G, projl(pair(E,4)), char).
%% mygamma(G), hastype(G, projl(pair(E,F)), int).

%% mygamma(G), hastype(G, projr(pair(4,v(y))), char).
%% mygamma(G), hastype(G, projr(pair(4,4)), X).
%% mygamma(G), hastype(G, projr(pair(4,x)), X).
%% mygamma(G), hastype(G, projr(pair(4,E)), char).
%% mygamma(G), hastype(G, projr(pair(E,F)), char).

%% mygamma(G), hastype(G, lambda(v(x),4), arrow(int, int)).
%% mygamma(G), hastype(G, lambda(v(y),4), arrow(int, int)).
%% hastype(G, lambda(a,b), arrow(int, float)).
%% mygamma(G), hastype(G, lambda(v(y),4), X).
%% mygamma(G), hastype(G, lambda(v(y),x), X).
%% hastype(G, lambda(A,B), arrow(double,string)).


%% mygamma(G), hastype(G, apply(lambda(v(x),v(y)),4), int).
%% mygamma(G), hastype(G, apply(lambda(v(x),v(y)),4), char).
%% mygamma(G), hastype(G, apply(lambda(v(x),v(y)),x), char).
%% mygamma(G), hastype(G, apply(lambda(v(y),v(y)),4), char).
%% hastype(G, apply(lambda(v(x),v(y)),4), char).
%% hastype(G, apply(lambda(v(x),v(y)),x), char).

%% mygamma(G), hastype(G, inl(4), or(int, float)).
%% mygamma(G), hastype(G, inl(v(y)), or(int, char)).
%% hastype(G, inl(v(y)), or(int, char)).
%% mygamma(G), hastype(G, X, or(int, char)). %% causes infinite loop
%% mygamma(G), hastype(G, inl(X), or(char, float)).

%% mygamma(G), hastype(G, inr(4), or(int, float)).
%% mygamma(G), hastype(G, inr(v(y)), or(int, char)).
%% hastype(G, inr(v(y)), or(int, char)).
%% mygamma(G), hastype(G, inr(X), or(char, float)). %% causes infinite loop
%% mygamma(G), hastype(G, inr(X), or(float, int)).
%% mygamma(G), hastype(G, inr(X), or(float, char)).

%% mygamma(G), hastype(G, case(v(y),4,x,v(x),v(x)), int). % should use second conditon of case
%% mygamma(G), hastype(G, case(4,4,x,v(x),v(x)), int). % should use first condition of case
%% mygamma(G), hastype(G, case(E,4,x,v(x),v(x)), int).
%% mygamma(G), hastype(G, case(E,4,x,v(y),v(y)), char).
%% mygamma(G), hastype(G, case(E,4,x,v(y),v(y)), Z).
