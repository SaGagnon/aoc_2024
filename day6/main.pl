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

guard(^) --> `^`.
free_cell(.) --> `.`.
obstacle(#) --> `#`.

elem(X) --> ( guard(X) ; free_cell(X) ; obstacle(X) ), !.

line([X|Xs]) --> elem(X), sequence(elem, Xs), eol, !.
lines([L|Ls]) --> line(L), sequence(line, Ls), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

next_i_j(Dir, I0, J0, I, J) :-
    (   Dir=up    ->  I=I0, succ(J, J0)
    ;   Dir=down  ->  I=I0, succ(J0, J)
    ;   Dir=left  ->  J=J0, succ(I, I0)
    ;   Dir=right ->  J=J0, succ(I0, I)
    ).

next_dir(up, right).
next_dir(right, down).
next_dir(down, left).
next_dir(left, up).

free(^).
free(.).

out_of_bound(I, J, Mat) :-
    functor(Mat, _, NJ),
    arg(1, Mat, First),
    functor(First, _, NI),
    (   I < 0
    ;   I > NI
    ;   J < 0
    ;   J > NJ
    ), !.

path(Mat, state(Dir0, I0, J0, Path0), Path) :-
    next_i_j(Dir0, I0, J0, I1, J1),
    (
        out_of_bound(I1, J1, Mat)
    ->
        Path=[I0-J0|Path0]
    ;   
        (   cell(Mat, [J1, I1], Elem),
            (
                Elem='#'
            ->
                next_dir(Dir0, Dir1),
                path(Mat, state(Dir1, I0, J0, Path0), Path)
            ;
                free(Elem)
            ->
                path(Mat, state(Dir0, I1, J1, [I0-J0|Path0]), Path)
            )
        )
    ).

path(Mat, Path) :-
    array(Mat, [JN, IN]),
    list_of([I in 1..IN, J in 1..JN] where Mat[J,I]=='^', I-J, [I-J]),
    path(Mat, state(up, I, J, []), Path).
    

solve1(L) :-
    phrase_from_file(lines(Ls), 'input.txt'),
    array_lists(Mat, Ls),
    path(Mat, Path0),
    sort(Path0, Path),
    length(Path, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve2(_) :-
    phrase_from_file(_, 'small_input.txt').
