% &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
% &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
% &&&&&&&&&&&&&&&&                       &&&&&&&&&&&&&&&&
% &&&&&&&&&&&&&&&&          LI           &&&&&&&&&&&&&&&&
% &&&&&&&&&&&&&&&&       PRÀCTICA 2      &&&&&&&&&&&&&&&&
% &&&&&&&&&&&&&&&&                       &&&&&&&&&&&&&&&&
% &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
% &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
% BY MARC NEBOT MOYANO


% EJERCICIO DE PRUEBA, FACTORIAL
fact(0,1).
fact(N, F) :- N1 is N-1, fact(N1, F1), F is N*F1.

natural(0).
natural(N):- natural(N1), N1 is N + 1.

% "append"
 concat([], L, L).
 concat([X|L1], L2, [X|L3]):- concat(L1, L2, L3).
% concat(L1, L2, [a,b,c]), write([L1, L2]), nl, fail.

% EJERCICIO 1
% &&&&&&&&&&&&
% Escribe un predicado prod(L,P) que signifique: “P es el producto de los elementos de la lista
% de enteros dada L”. Debe poder generar la P y tambi ́en comprobar una P dada.
% &&&&&&&&&&&&
prod([], 1).
prod([X|XS], P) :- prod(XS, P1), P is X*P1.

% EJERCICIO 2
% &&&&&&&&&&&&
% Escribe un predicado pescalar(L1,L2,P) que signifique: “P es el producto escalar de los
% vectores L1 y L2”, donde los vectores se representan como listas de enteros. El predicado debe
% fallar si los dos vectores tienen longitudes distintas.
% &&&&&&&&&&&&
pescalar([],[],0)
pescalar([L1|List1], [L2|List2], S):- pescalar(List1, List2, R1), R is R1 + L1*L2.

% EJERCICIO 3
% &&&&&&&&&&&&
% Representando conjuntos con listas sin repeticiones, escribe predicados para las operaciones de
% interseccion y union de conjuntos dados
% &&&&&&&&&&&&
union([], L, L).
union([X|L1], L2, L3):- member(X, L2), !, union(L1, L2, L3). % Aquí sha de posar el "!" perquè sinó també entrarà a la següent opció.
union([X|L1], L2, [X|L3]):- union(L1, L2, L3).

interseccion([], _, []).
interseccion([X|L1], L2, [X|L3]):- member(X, L2), !, remove_element(X, L2, NewL2), interseccion(L1, NewL2, L3).
interseccion([_|L1], L2, L3):- interseccion(L1, L2, L3).

remove_element(X, L, NewL):- append(L1, [X|L2], L), append(L1, L2, NewL).

% EJERCICIO 4
% &&&&&&&&&&&&
% Usando concat, escribe un predicado para calcular el  ́ultimo elemento de una lista dada, y otro
% para calcular la lista inversa de una lista dada.
% &&&&&&&&&&&&
lastElement(L, X):- append(_, [X], L).

reverseList([], []).
reverseList(L, [X|RL]):- lastElement(L, X), removeLast(L, NewL), reverseList(NewL, RL).

removeLast(L, NewL):- append(NewL, [_], L).

% EJERCICIO 5
% &&&&&&&&&&&&
% Escribe un predicado fib(N,F) que signifique: “F es el N- ́esimo n ́umero de Fibonacci para la
% N dada”. Estos n ́umeros se definen as ́ı: fib(1) = 1, fib(2) = 1, y si N > 2 entonces fib(N ) =
% fib(N −1) + fib(N −2)
% &&&&&&&&&&&&
fib(1, 1):- !.
fib(2, 1):- !.
fib(N, F):- N1 is N - 1, N2 is N - 2,
            fib(N1, F1), fib(N2, F2),
            F is F1 + F2.

% EJERCICIO 6
% &&&&&&&&&&&&
% Escribe un predicado dados(P,N,L) que signifique: “la lista L expresa una manera de sumar
% P puntos lanzando N dados”. Por ejemplo: si P es 5 y N es 2, una soluci ́on ser ́ıa [1,4] (n ́otese
% que la longitud de L es N). Tanto P como N vienen instanciados. El predicado debe ser capaz de
% generar todas las soluciones posibles.
% &&&&&&&&&&&&
dados(0, 0, []).
dados(P, N, [X|L]):- N > 0,  member(X, [1,2,3,4,5,6]), N1 is N - 1, P1 is P - X, dados(P1, N1, L).

sum([], 0).
sum([L|L1], P):- sum(L1, P1), P is P1 + L.

% EJERCICIO 7
% &&&&&&&&&&&&
% Escribe un predicado suma demas(L) que, dada una lista de enteros L, se satisface si existe alg ́un
% elemento en L que es igual a la suma de los dem ́as elementos de L, y falla en caso contrario
% &&&&&&&&&&&&
suma_demas(L):- append(L1, [X|L2], L), append(L1, L2, NewL), sum(NewL, X).

% EJERCICIO 8
% &&&&&&&&&&&&
% Escribe un predicado suma ants(L) que, dada una lista de enteros L, se satisface si existe alg ́un
% elemento en L que es igual a la suma de los elementos anteriores a  ́el en L, y falla en caso
% contrario.
% &&&&&&&&&&&&
suma_ants(L):- append(L1, [X|_], L), sum(L1, X).

% EJERCICIO 9
% &&&&&&&&&&&&
% Escribe un predicado card(L) que, dada una lista de enteros L, escriba la lista que, para cada
% elemento de L, dice cu ́antas veces aparece este elemento en L. Por ejemplo, si hacemos la consulta
% card( [1,2,1,5,1,3,3,7] ) el interprete escribira:
% [[1,3],[2,1],[5,1],[3,2],[7,1]].
% &&&&&&&&&&&&
card(List):- cards(List, Res), write(Res).

cards([], []).
cards([L|List], [[L, NumL]|Res]):- cards(List, ResList),
              card_in_list([L, NewNumL], ResList, Res), !,
              NumL is NewNumL+1.
cards([L|List], [[L, 1]|Res]):- cards(List, Res).

% FinalL = List - [L]
card_in_list(L, List, FinalL):- append(L1, [L|L2], List),
               append(L1, L2, FinalL).


% EJERCICIO 10
% &&&&&&&&&&&&
% Escribe un predicado esta ordenada(L) que signifique: “la lista L de n ́umeros enteros est ́a
% ordenada de menor a mayor”. Por ejemplo, a la consulta:
% ?-esta ordenada([3,45,67,83]).
% el interprete responde yes, y a la consulta:
% ?-esta ordenada([3,67,45]).
% responde no
% &&&&&&&&&&&&
esta_ordenada([]).
esta_ordenada([_]).
esta_ordenada([L1, L2|List]):- L1 < L2, esta_ordenada([L2|List]).

% EJERCICIO 11
% &&&&&&&&&&&&
% Escribe un predicado ord(L1,L2) que signifique: “L2 es la lista de enteros L1 ordenada de
% menor a mayor”. Por ejemplo: si L1 es [4,5,3,3,2] entonces L2 ser ́a [2,3,3,4,5]. Hazlo en
% una l ́ınea, usando s ́olo los predicados permutacion y esta ordenada.
% &&&&&&&&&&&&
  ordenacion(L, SL):- permutation(L, SL), esta_ordenada(SL).

% EJERCICIO 12
% &&&&&&&&&&&&
% Escribe un predicado diccionario(A,N) que, dado un alfabeto A de s ́ımbolos y un natural N,
% escriba todas las palabras de N s ́ımbolos, por orden alfab ́etico (el orden alfab ́etico es seg ́un el
% alfabeto A dado). Por ejemplo, diccionario( [ga,chu,le],2) escribir ́a:
% gaga gachu gale chuga chuchu chule lega lechu lele.
% &&&&&&&&&&&&
diccionario(A,N):- nmembers(A,N,L), write_dic(L), fail.
diccionario(_,_).

nmembers(_,0,[]):-!.
nmembers(A,N,[X|L]):-
    pert(X,A),
    N1 is N-1,
    nmembers(A,N1,L).

write_dic([]):- write(' '), nl, !.
write_dic([X|L]):-
    write(X),
    write_dic(L).


% EJERCICIO 13
% &&&&&&&&&&&&
% Escribe un predicado palindromos(L) que, dada una lista de letras L, escriba todas las per-
% mutaciones de sus elementos que sean pal ́ındromos (capic ́uas). Por ejemplo, con la consulta
% palindromos([a,a,c,c]) se escribe [a,c,c,a] y [c,a,a,c].
% &&&&&&&&&&&&
palindromos(L):-
    permutacion(L,L1),
    es_palindromo(L1),
    write(L1), nl, fail.
palindromos(_).

% palindromos(L) :- setof(L1, (permutation(L,L1), es_palindromo(L1)), Res), write(Res).
es_palindromo(L):-
    reverse(L,L).

% EJERCICIO 14
% &&&&&&&&&&&&
% Encuentra mediante un programa Prolog, usando el predicado permutaci ́on, qu ́e 8 d ́ıgitos difer-
% entes tenemos que asignar a las letras S, E, N, D, M, O, R, Y, de manera que se cumpla la suma
% siguiente:
% S E N D
% + M O R E
% ---------------
% M O N E Y
% &&&&&&&&&&&&
suma([],[],[],C,C).
suma([X1|L1],[X2|L2],[X3|L3],Cin,Cout) :-
	X3 is (X1 + X2 + Cin) mod 10,
	C  is (X1 + X2 + Cin) //  10,
	suma(L1,L2,L3,C,Cout).


send_more_money :-

	L = [S, E, N, D, M, O, R, Y, _, _],
	permutacion(L, [0,1,2,3,4,5,6,7,8,9]),
	suma([D, N, E, S], [E, R, O, M], [Y, E, N, O], 0, M),

	write('S = '), write(S), nl,
	write('E = '), write(E), nl,
	write('N = '), write(N), nl,
	write('D = '), write(D), nl,
	write('M = '), write(M), nl,
	write('O = '), write(O), nl,
	write('R = '), write(R), nl,
	write('Y = '), write(Y), nl,
	write('  '), write([S,E,N,D]), nl,
	write('  '), write([M,O,R,E]), nl,
	write('-------------------'), nl,
	write([M,O,N,E,Y]), nl.

% EJERCICIO 15
% &&&&&&&&&&&&
% Escribe un predicado simplifica que pueda usarse en combinaci ́on con el programa de calcular
% derivadas.
% &&&&&&&&&&&&

% EJERCICIO 16
% &&&&&&&&&&&&
% Queremos obtener en Prolog un predicado dom(L) que, dada una lista L de fichas de domin ́o (en
% el formato de abajo), escriba una cadena de domin ́o usando todas las fichas de L, o escriba “no
% hay cadena” si no es posible. Por ejemplo,
% ?- dom( [ f(3,4), f(2,3), f(1,6), f(2,2), f(4,2), f(2,1) ] ).
% escribe la cadena correcta:
% [ f(2,3), f(3,4), f(4,2), f(2,2), f(2,1), f(1,6) ].
% Tambien podemos girar alguna ficha como f(N,M), reemplaz ́andola por f(M,N). As ́ı, para:
% ?- dom ([ f(4,3), f(2,3), f(1,6), f(2,2), f(2,4), f(2,1) ]).
% solo hay cadena si se gira alguna ficha (por ejemplo, hay la misma cadena que antes).
% El siguiente programa Prolog a ́un no tiene en cuenta los posibles giros de fichas, ni tiene imple-
% mentado el predicado ok(P), que significa: “P es una cadena de domin ́o correcta (tal cual, sin
% necesidad ya de girar ninguna ficha)”:
% p([],[]).
% p(L,[X|P]) :- select(X,L,R), p(R,P).
% dom(L) :- p(L,P), ok(P), write(P), nl.
% dom( ) :- write(’no hay cadena’), nl.
% (a) ¿Que significa el predicado p(L,P) para una lista L dada?
% (b) Escribe el predicado ok(P) que falta.
% (c) Extiende el predicado p para que el programa tambi ́en pueda hacer cadenas girando alguna
% de las fichas de la entrada.
% &&&&&&&&&&&&

p([],[]).
p(L,[[X1, X2]|P]) :- select([X1, X2],L,R), p(R,P).
p(L,[[X2,X1]|P]) :- select([X1, X2],L,R), p(R,P).


ok([]).
ok([[_,_]|[]]).
ok([[_,Y],[Y,Z]|Resto]) :- ok([[Y,Z]|Resto]).


dom(L) :- p(L,P), ok(P), !, write(P), nl.
dom(_) :- write("no hay cadena"), nl.

% EJERCICIO 17
% &&&&&&&&&&&&
% Complete the following backtracking procedure for SAT in Prolog. Program everything, except
% the predicate readclauses(F), which reads a list of clauses, where each clause is a list of integers.
% For example, p3∨¬p6∨p2is represented by [3,-6,2]. Do things as simple as possible
% p:- readclauses(F), sat([],F).
% p:- write(’UNSAT’),nl.
% sat(I,[]):- write(’IT IS SATISFIABLE. Model: ’), write(I),nl,!.
% sat(I,F):-
% decision lit(F,Lit), % Select unit clause if any; otherwise, an arbitrary one.
% simplif(Lit,F,F1), % Simplifies F. Warning: may fail and cause backtracking
% sat( ... , ... ).
% &&&&&&&&&&&&

% EJERCICIO 18
% &&&&&&&&&&&&
% Consider two groups of 10 people each. In the first group, as expected, the percentage of people
% with lung cancer among smokers is higher than among non-smokers. In the second group, the
% same is the case. But if we consider the 20 people of the two groups together, then the situation
% is the opposite: the proportion of people with lung cancer is higher among non-smokers than
% among smokers! Can this be true? Write a little Prolog program to find it out.
% &&&&&&&&&&&&

% EJERCICIO 19
% &&&&&&&&&&&&
% Supongamos que tenemos una maquina que dispone de monedas de valores [X1,...Xn] y tiene
% que devolver una cantidad C de cambio utilizando el m ́ınimo n ́umero de monedas. Escribe un
% programa Prolog maq(L,C,M) que, dada la lista de monedas L y la cantidad C, genere en M la
% lista de monedas a devolver de cada tipo. Por ejemplo, si L es [1,2,5,13,17,35,157], y C es
% 361, entonces una respuesta es [0,0,0,1,2,0,2] (5 monedas).
% Note: greedy algorithms (starting with the largest coin, etc.) do not always work!
% &&&&&&&&&&&&


% EJERCICIO 20
% &&&&&&&&&&&&
% Write in Prolog a predicate flatten(L,F) that “flattens” (cast.: “aplana”) the list F as in the
% example:
% ?-flatten( [a,b,[c,[d],e,[]],f,[g,h]], F ).
% F=[a,b,c,d,e,f,g,h]?
% &&&&&&&&&&&&
flatten([],[]) :- !.
flatten([L|LS],F) :- !,flatten(L,F1), flatten(LS,F2), append(F1,F2,F).
flatten(L, [L]).


% EJERCICIO 21
% &&&&&&&&&&&&
% Escribe un predicado Prolog log(B,N,L) que calcula la parte entera L del logaritmo en base B
% de N, donde B y N son naturales positivos dados. Por ejemplo, ?- log(2,1020,L). escribe L=9?
% Pod ́eis usar la exponenciaci ́on, como en 125 is 5**3. El programa (completo) no debe ocupar
% mas de 3 lineas
% &&&&&&&&&&&&

logo(B, N, L):- N > 0, B > 1, L is truncate(log10(N)/log10(B)).

% EJERCICIO 22
% &&&&&&&&&&&&
% Supongamos que N estudiantes (identificados por un n ́umero entre 1 y N) se quieren matricular
% de LI, pero s ́olo hay espacio para M, con M < N. Adem ́as nos dan una lista L de pares de estos
% estudiantes que son incompatibles entre s ́ı (por ejemplo, porque siempre se copian). Queremos
% obtener un programa Prolog li(N,M,L,S) que, para N, M y L dados, genere un subconjunto S
% con M de los N estudiantes tal que si [x, y] ∈ L entonces {x, y}6⊆ S. Por ejemplo, una soluci ́on de
% li( 20, 16, [[8,11],[8,15],[11,6],[4,9],[18,13],[7,9],[16,8],[18,10],[6,17],[8,20]], S )
% es [20,19,17,16,15,14,13,12,11,10,7,5,4,3,2,1] .
% Escribe una version lo mas sencilla que puedas, aunque sea ineficiente, del estilo “generar una
% solucion (total) y despues comprobar si es correcta”
% &&&&&&&&&&&&