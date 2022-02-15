minimo([X],X).
minimo([X|Xs],M) :-
	minimo(Xs,T),
	M is min(X,T).
