:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pair([X,Y]) --> number(X), blanks, number(Y), eol.

pairs([P|Ps]) --> pair(P), sequence(pair, Ps).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pair_distance([X,Y], Abs) :-
    Abs #= abs(X-Y).

solve1(S) :-
    phrase_from_file(pairs(Pairs), 'input.txt'),
    transpose(Pairs, [List1, List2]),
    sort(0, @=<, List1, SortedList1),
    sort(0, @=<, List2, SortedList2),
    transpose([SortedList1, SortedList2], MatchedPairs),
    maplist(pair_distance, MatchedPairs, Distances),
    sum_list(Distances, S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eq(X, X, 1).
eq(X, Y, 0) :- dif(X,Y).

nb_eq(Lst, X, Nb) :-
    maplist(eq(X), Lst, Eqs),
    sum_list(Eqs, Nb).

similarity(RightLst, X, Sim) :-
    nb_eq(RightLst, X, NbEq),
    Sim is X * NbEq.

solve2(S) :-
    phrase_from_file(pairs(Pairs), 'input.txt'),
    transpose(Pairs, [List1, List2]),
    maplist(similarity(List2), List1, Sims),
    sum_list(Sims, S).
