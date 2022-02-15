% A matrix which contains zeroes and ones gets "x-rayed" vertically and
% horizontally, giving the total number of ones in each row and column.
% The problem is to reconstruct the contents of the matrix from this
% information. Sample run:
%
%    ?- p.
%        0 0 7 1 6 3 4 5 2 7 0 0
%     0
%     0
%     8      * * * * * * * *
%     2      *             *
%     6      *   * * * *   *
%     4      *   *     *   *
%     5      *   *   * *   *
%     3      *   *         *
%     7      *   * * * * * *
%     0
%     0
%

:- use_module(library(clpfd)).

ejemplo1( [0,0,8,2,6,4,5,3,7,0,0], [0,0,7,1,6,3,4,5,2,7,0,0] ).
ejemplo2( [10,4,8,5,6], [5,3,4,0,5,0,5,2,2,0,1,5,1] ).
ejemplo3( [11,5,4], [3,2,3,1,1,1,1,2,3,2,1] ).


p:-    ejemplo2(RowSums,ColSums),
    length(RowSums,NumRows),
    length(ColSums,NumCols),
    NVars is NumRows*NumCols,
    listVars(NVars,L),  % generate a list of Prolog vars (their names do not matter)
    matrixByRows(L,NumCols,MatrixByRows),
    transpose(MatrixByRows, TransposedMatrix),
    declareConstraints(L, MatrixByRows, TransposedMatrix, RowSums, ColSums),
    pretty_print(RowSums,ColSums,MatrixByRows).


listVars(Nvas, L):- length(L, Nvas).

declareConstraints(L, MatrixByRows, TransposedMatrix, RowSums, ColSums):-
  L ins 0..1,
  correcto(MatrixByRows, RowSums),
  correcto(TransposedMatrix, ColSums),
  label(L).


correcto([], []):- !.
correcto([X|Matrix], [R|Sumas]) :- sumatorio(X, ResX), ResX #= R, correcto(Matrix, Sumas).


sumatorio([X], X).
sumatorio([X|L], R+X):- sumatorio(L,R).



matrixByRows(L, NumberColumns, MatrixByRows):-
  length(L, N),           %TENEMOS N ELEMENTOS
  NumberRows is N div NumberColumns,     %NUMERO FILAS = ELEMENTOS / COLUMNAS
  length(MatrixByRows, NumberRows),   %TAMAÑO MATRIZ POR LIEAS = LINEAS
  maplist(checklength(NumberColumns), MatrixByRows),  %CADA LINEA TIENE TAMAÑO COLUMNS
  append(MatrixByRows, L).  %SI JUNTAS TODAS LAS LINEAS SE FORMA L

checklength(N, L):- length(L, N).

%matrixByRows(S,L,Rows) :-
    %maplist(checklength(S), Rows),
    %append(Rows, L).
  
%checklength(S,Row):- length(Row, S).

%constraintsFromSubLists([]):- !.
%constraintsFromSubLists([Row|Rows]):- all_different(Row), constraintsFromSubLists(Rows).

pretty_print(_,ColSums,_):- write('     '), member(S,ColSums), writef('%2r ',[S]), fail.
pretty_print(RowSums,_,M):- nl,nth1(N,M,Row), nth1(N,RowSums,S), nl, writef('%3r   ',[S]), member(B,Row), wbit(B), fail.
pretty_print(_,_,_):- nl.
wbit(1):- write('*  '),!.
wbit(0):- write('   '),!.