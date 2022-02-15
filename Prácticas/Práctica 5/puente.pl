deletelist([], _, []).     
deletelist([X|Xs], Y, Z) :- member(X, Y), deletelist(Xs, Y, Z), !. 
deletelist([X|Xs], Y, [X|Zs]) :- deletelist(Xs, Y, Zs). 

%%izquierda->derecha
unPaso([PersonasII, PersonasDI, 0, 0], [PersonasIF, PersonasDF, 1, Coste]):-
    write('llego'),
    member(P, PersonasII),
    
    member(P2, PersonasII),
    
    append([P], PersonasDI, List),
    
    append([P2], List, NewList),
    
    append(PersonasDF, L2, PersonasDF),
    
    deletelist(PersonasII, [P,P2], L),
    
    deletelist(PersonasIF, PersonasIF, L2),
    
    append(PersonasII, PersonasIF, PersonasIF),
    
    Coste is 0,
    write(PersonasIF + PersonasDF + 1 + Coste).

camino(E,E,C,C).
camino(EstadoActual,EstadoFinal,CaminoHastaAhora,CaminoTotal):-
        write(EstadoActual),
        
        unPaso(EstadoActual,EstSiguiente),
        write('llego2'),
        \+member(EstSiguiente,CaminoHastaAhora),
        camino(EstSiguiente,EstadoFinal,[EstSiguiente|CaminoHastaAhora],
	    CaminoTotal).

solucionOptima:-
    camino([[1,2,3,4],[],0,0],[[1,2],[3,4],1,0],[[[1,2,3,4],[],0,0]],C),
    write(C).
