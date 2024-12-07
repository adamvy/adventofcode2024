:- use_module(library(clpz)).
:- use_module(library(pio)).
:- use_module(library(charsio)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- set_prolog_flag(double_quotes, chars).
:- use_module(library(pairs)).
:- use_module(library(format)).
:- use_module(library(tabling)).
:- use_module(library(reif)).

state(S), [S] --> [S].
state(S0, S), [S] --> [S0].
seq([])     --> [].
seq([E|Es]) --> [E], seq(Es).

digit_chars([]) --> [].
digit_chars([D|Ds]) -->
    [D],
    { char_type(D, numeric) },
    digit_chars(Ds).

number(N) -->
    digit_chars(D),
    { catch(number_chars(N, D), error(syntax_error(_), _), false) }.

msort(Ls0, Ls) :-
        pairs_keys(Pairs0, Ls0),
        keysort(Pairs0, Pairs),
        pairs_keys(Pairs, Ls).


lines([]) --> [].
lines([L|Ls]) --> seq(L), "\n", lines(Ls).


% cell(0, 0, 'A')

ingest_([Col|Cols], NRow, NCol) :-
    assertz(cell(NRow, NCol, Col)),
    NCol1 is NCol + 1,
    ingest_(Cols, NRow, NCol1).
ingest_([], _, _).

ingest([Row|Rows], NRow) :-
    ingest_(Row, NRow, 0),
    NRow1 is NRow + 1,
    ingest(Rows, NRow1).
ingest([], _).


word(v(X, Y), v(DX, DY), [W|Ws]) :-
    cell(X, Y, W),
    X1 #= X + DX,
    Y1 #= Y + DY,
    word(v(X1, Y1), v(DX, DY), Ws).
word(_, _, []).

direction(v(0, 1)).
direction(v(0, -1)).
direction(v(1, 0)).
direction(v(-1, 0)).
direction(v(1, 1)).
direction(v(1, -1)).
direction(v(-1, -1)).
direction(v(-1, 1)).

loadexample :-
    retractall(cell(_, _, _)),
    phrase_from_file(lines(Ls), "day4example.txt"), ingest(Ls, 0).

loadreal :-
    retractall(cell(_, _, _)),
    phrase_from_file(lines(Ls), "day4.txt"), ingest(Ls, 0).


part1 :-
    setof(w(P, V), (direction(V), word(P, V, "XMAS")), R),
    length(R, L),
    format("~w~n", [L]).


% part, looking for a cross
% a word cross implies a word going across on one diagonal (in either direction)
% and thes ame word going across on the other diagonal in either direction
%

d1(v(1, 1)).
d1(v(-1, -1)).
d2(v(1, -1)).
d2(v(-1, 1)).


cross(P1, V1, W) :-
    d1(V1),
    d2(V2),
    length(W, L),
    reflect(P1, V1, P2, V2, L),
    word(P1, V1, W),
    word(P2, V2, W).

reflect(v(X1, Y), v(DX1, DY), v(X2, Y), v(DX2, DY), L) :-
    X2 #= X1 + ((L - 1) * DX1),
    DX2 #= DX1 * -1.

reflect(v(X, Y1), v(DX, DY1), v(X, Y2), v(DX, DY2), L) :-
    Y2 #= Y1 + ((L - 1) * DY1),
    DY2 #= DY1 * -1.




part2 :-
    setof(c(P1, V1, P2, V2), cross(P1, V1, P2, V2, "MAS"), R),
    length(R, L),
    format("~w~n", [L]).


