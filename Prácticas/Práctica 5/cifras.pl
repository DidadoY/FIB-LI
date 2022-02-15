

camino(E,E,C,C).
camino(EstadoActual,EstadoFinal,CaminoHastaAhora,CaminoTotal):-
        unPaso(EstadoActual,EstSiguiente),
        \+member(EstSiguiente,CaminoHastaAhora),
        camino(EstSiguiente,EstadoFinal,[EstSiguiente|CaminoHastaAhora],
	    CaminoTotal).


nat(0).
nat(N):- nat(N1), N is N1 + 1.

solucionOptima:-
	nat(N),
	camino([3,3,0,0,0],[0,0,3,3,1],[[3,3,0,0,0]],C),
	length(C,N),
	write(C).