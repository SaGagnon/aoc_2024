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

path(Mat, state(Dir0, I0, J0, Path0, NbIter0), Path) :-
    next_i_j(Dir0, I0, J0, I1, J1),
    succ(NbIter0, NbIter),
    (
        (   out_of_bound(I1, J1, Mat)
        ;   NbIter > 30000 % Hack
        )
    ->
        Path=[I0-J0|Path0]
    ;   
        (   cell(Mat, [J1, I1], Elem),
            (
                Elem='#'
            ->
                next_dir(Dir0, Dir1),
                path(Mat, state(Dir1, I0, J0, Path0, NbIter), Path)
            ;
                free(Elem)
            ->
                path(Mat, state(Dir0, I1, J1, [I0-J0|Path0], NbIter), Path)
            )
        )
    ).

path(Mat, Path) :-
    path(Mat, state(up, 63, 60, [], 0), Path).

solve1(L) :-
    phrase_from_file(lines(Ls), 'input.txt'),
    array_lists(Mat, Ls),
    path(Mat, Path0),
    sort(Path0, Path),
    length(Path, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

looping([IJ1, IJ2|IJs]) :-
    looping(IJ1, IJ2, IJs).

looping(IJ1, IJ2, [IJ3, IJ4 | IJs]) :-
    (   IJ1=IJ3, IJ2=IJ4
    ->  true
    ;   looping(IJ1, IJ2, [IJ4|IJs])
    ).

looping_starting_at(Mat, I, J) :-
    % Puzzle original pour tester les obstacles
    path(Mat, Path0),
    sort(Path0, Path1),
    member(I-J, Path1),
    % On place l'obstacle
    print([I,J]),nl,
    arg(J, Mat, JRow),
    setarg(I, JRow, '#'),
    path(Mat, Path),
    looping(Path).

nb_looping(Mat, Count) :-
    aggregate_all(count, looping_starting_at(Mat, _, _), Count).


solve2(Count) :-
    phrase_from_file(lines(Ls), 'input.txt'),
    array_lists(Mat, Ls),
    nb_looping(Mat, Count).
    
