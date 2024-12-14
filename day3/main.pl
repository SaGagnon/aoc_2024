:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(lists)).
:- use_module(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mul(mul(X, Y)) --> `mul(`, number(X), `,`, number(Y), `)`.
muls([M|Ms]) -->
    (   mul(M)
    ;   enable(M)
    ;   disable(M)
    ),
    !,
    muls(Ms).
muls(Ms) --> [_], !, muls(Ms).
muls([]) --> [].

enable(enable) --> `do()`.
disable(disable) --> `don't()`.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mul(enable, 0).
mul(disable, 0).
mul(mul(X,Y), Z) :- Z is X*Y.

solve1(S) :-
    phrase_from_file(muls(Ms0), 'input.txt'),
    maplist(mul, Ms0, Ms),
    sum_list(Ms, S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mul2(enable, _-Sum, enable-Sum).
mul2(disable, _-Sum, disable-Sum).
mul2(mul(X,Y), enable-Sum0, enable-Sum) :- Sum is Sum0 + X*Y.
mul2(mul(_, _), disable-Sum, disable-Sum).

solve2(S) :-
    phrase_from_file(muls(Ms), 'input.txt'),
    foldl(mul2, Ms, enable-0, _-S).
