%% Write a Prolog predicate eqSplit(L,S1,S2) that, given a list of
%% integers L, splits it into two disjoint subsets S1 and S2 such that
%% the sum of the numbers in S1 is equal to the sum of S2. It should
%% behave as follows:
%%
%% ?- eqSplit([1,5,2,3,4,7],S1,S2), write(S1), write('    '), write(S2), nl, fail.
%%
%% [1,5,2,3]    [4,7]
%% [1,3,7]    [5,2,4]
%% [5,2,4]    [1,3,7]
%% [4,7]    [1,5,2,3]

subset([], []).
subset([E|Tail], [E|NTail]):- subset(Tail, NTail).
subset([_|Tail], NTail):- subset(Tail, NTail).

difference([],_,[]).
difference([X|L],K,M) :- member(X,K), difference(L,K,M).
difference([X|L],K,[X|M]) :- not(member(X,K)), difference(L,K,M).

eqSum(S1, S2):- sum_list(S1, R), sum_list(S2, R2), R = R2.

eqSplit([],_,_).
eqSplit(L,S1,S2):- subset(L, S1), difference(L, S1, S2), eqSum(S1, S2).