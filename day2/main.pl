:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(lists)).
:- use_module(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

report([L|Ls]) --> number(L), ` `, sequence(number, ` `, Ls), eol, !.
reports([R|Rs]) --> report(R), sequence(report,  Rs), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

good_step(Cur, Last, Cur) :- Step is abs(Last-Cur), Step>0, Step<4.
good_steps([L|Ls]) :- foldl(good_step, Ls, L, _).

safe(Ls) :-
    good_steps(Ls),
    (   chain(Ls, #<)
    ->  true
    ;   chain(Ls, #>)
    ).

solve1(L) :-
    phrase_from_file(reports(Rs0), 'input.txt'), 
    include(safe, Rs0, Rs),
    length(Rs, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

safe2(Ls) :- safe(Ls).
safe2(Ls) :-
    select(_, Ls, Ls1),
    safe(Ls1),
    !.

solve2(L) :-
    phrase_from_file(reports(Rs0), 'input.txt'),
    include(safe2, Rs0, Rs),
    length(Rs, L).


