:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(arrays)).
:- use_module(library(comprehension)).
:- use_module(library(aggregate)).
:- use_module(library(assoc)).
:- use_module(library(yall)).
:- use_module(library(pairs)).

:- op(950, fy, *).
*(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ordering_rule(before(X,Y)) --> number(X), `|`, number(Y), eol.
all_ordering_rules([R|Rs]) --> ordering_rule(R), sequence(ordering_rule, Rs), !.

update([X|Xs]) --> number(X), `,`, sequence(number, `,`, Xs), eol, !.
all_updates([U|Us]) --> update(U), sequence(update, Us), !.

problem(OrderingRules, Updates) -->
    all_ordering_rules(OrderingRules),
    eol,
    all_updates(Updates).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_var_assoc(Key, Assoc0, A-Value, Assoc) :-
    (   get_assoc(Key, Assoc0, A-Value)
    ->  Assoc=Assoc0
    ;   put_assoc(Key, Assoc0, A-Value, Assoc)
    ).

add_constraint(before(X,Y), Assoc0, Assoc) :-
    get_var_assoc(X, Assoc0, AX-VX, Assoc1),
    get_var_assoc(Y, Assoc1, AY-VY, Assoc),
    when((ground(AX), ground(AY)), VX #< VY).

constrained_positions(ORs, Ps) :-
    empty_assoc(Ps0),
    foldl(add_constraint, ORs, Ps0, Ps).

update_ok(Ps, U, Middle) :-
    maplist({Ps}/[Key,Val]>>(get_assoc(Key, Ps, Val)), U, Pairs),
    pairs_keys_values(Pairs, As, Vars),
    length(U, L),
    As ins 1,
    Vars ins 1..L,
    chain(Vars, #<),
    MidIdx is ceiling(L/2),
    nth1(MidIdx, U, Middle).

solve1(M) :-
    phrase_from_file(problem(ORs, Us), 'input.txt'),
    constrained_positions(ORs, Ps),
    findall(M, (member(U, Us), update_ok(Ps, U, M)), Ms),
    sum_list(Ms, M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

corrected_order(Ps, U, Middle) :-
    (   update_ok(Ps, U, Middle)
    ->  false
    ;   maplist({Ps}/[Key,Val]>>(get_assoc(Key, Ps, Val)), U, Pairs),
        pairs_keys_values(Pairs, As, Vars),
        length(U, L),
        As ins 1,
        Vars ins 1..L,
        once(label(Vars)),
        MidIdx is ceiling(L/2),
        nth1(UIdx, Vars, MidIdx),
        nth1(UIdx, U, Middle)
    ).

solve2(M) :-
     phrase_from_file(problem(ORs, Us), 'input.txt'),
    constrained_positions(ORs, Ps),
    findall(M, (member(U, Us), corrected_order(Ps, U, M)), Ms),
    sum_list(Ms, M).
