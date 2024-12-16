:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(lists)).
:- use_module(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

line([C|Cs]) -->
    string_without(`\n`, [C|Cs]),
    eol.

lines([L|Ls]) --> line(L), sequence(line, Ls), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lines_i_j_diag(_, IN-IN, _-_, []) :- !.
lines_i_j_diag(_, _-_, JN-JN, []) :- !.
lines_i_j_diag(Ls, I0-IN, J0-JN, [D|Ds]) :-
    nth0(J0, Ls, L),
    nth0(I0, L, D),
    I is I0+1,
    J is J0+1,
    lines_i_j_diag(Ls, I-IN, J-JN, Ds).

lines_diag([L|Ls], Diag) :-
    length([L|Ls], JN),
    length(L, IN),
    (   J=0,
        between(0, IN, I)
    ;   I=0,
        between(1, JN, J)
    ),
    lines_i_j_diag([L|Ls], I-IN, J-JN, Diag).

lines_diags(Ls, Diags) :-
    findall(Diag, lines_diag(Ls, Diag), Diags).

seq_nbxmas(Seq, N) :-
    seq_nbxmas(Seq, 0, N).

seq_nbxmas([], N, N).
seq_nbxmas([S|Ss], N0, N) :-
    (   append(`XMAS`, _, [S|Ss])
    ->  N1 is N0 + 1
    ;   N1 = N0
    ),
    seq_nbxmas(Ss, N1, N).

nb_xmas_for_orientation(Ls, N) :-
    lines_diags(Ls, Diags),
    append(Ls, Diags, Seqs),
    maplist(seq_nbxmas, Seqs, Ns),
    sum_list(Ns, N).

flip(Ls0, Ls) :-
    transpose(Ls0, Ls1),
    maplist(reverse, Ls1, Ls).

nb_xmas_for_all_orientation(Ls, N) :-
    flip(Ls, Ls1),
    flip(Ls1, Ls2),
    flip(Ls2, Ls3),
    maplist(nb_xmas_for_orientation, [Ls, Ls1, Ls2, Ls3], Ns),
    sum_list(Ns, N).

solve1(N) :-
    phrase_from_file(lines(Ls), 'input.txt'),
    nb_xmas_for_all_orientation(Ls, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

good(`MSAMS`).
good(`MMASS`).
good(`SMASM`).
good(`SSAMM`).

mat_elem(Arrs, I-J, Elem) :-
    nth1(J, Arrs, Arr),
    arg(I, Arr, Elem).

good_i_j(Arrs, I, J) :-
    good(G),
    J1 is J+1, I1 is I+1,
    J2 is J+2, I2 is I+2,
    maplist(mat_elem(Arrs),
            [I-J, I2-J, I1-J1, I-J2, I2-J2],
            G),
    !.

list_array(L, Arr) :-
    Arr =.. [arr|L].

lists_arrays(Ls, Arrs) :-
    maplist(list_array, Ls, Arrs).

valid(Arrs, I, J) :-
    length(Arrs, JN),
    Arrs = [Arr|_],
    functor(Arr, _, IN),
    valid_(Arrs, I, J, IN, JN).

valid_(Arrs, I, J, IN, JN) :-
    I in 1..IN,
    J in 1..JN,
    label([I,J]),
    good_i_j(Arrs, I, J).

nb_valid(Arrs, Nb) :-
    findall(1, valid(Arrs, _, _), Nbs),
    sum_list(Nbs, Nb).

solve2(Nb) :-
    phrase_from_file(lines(Ls), 'input.txt'),
    lists_arrays(Ls, Arrs),
    nb_valid(Arrs, Nb).
