
:- use_module(library(clpz)).
:- use_module(library(pio)).
:- use_module(library(charsio)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- set_prolog_flag(double_quotes, chars).
:- use_module(library(pairs)).
:- use_module(library(format)).
:- use_module(library(tabling)).

msort(Ls0, Ls) :-
        pairs_keys(Pairs0, Ls0),
        keysort(Pairs0, Pairs),
        pairs_keys(Pairs, Ls).

ws --> [W], { char_type(W, whitespace) }, ws.
ws --> [].

number(N) -->
    digit_chars(D),
    { catch(number_chars(N, D), error(syntax_error(_), _), false) }.

digit_chars([D|Ds]) -->
    [D],
    { char_type(D, numeric) },
    digit_chars(Ds).
digit_chars([]) --> [].

line(A, B) -->
    number(A),
    ws,
    number(B),
    "\n".

lines([A|As], [B|Bs]) -->
    line(A, B),
    lines(As, Bs).
lines([], []) --> [].

distance([], [], 0).
distance([A|As], [B|Bs], C) :-
    C #= C1 + abs(A - B),
    distance(As, Bs, C1).

part1 :-
    phrase_from_file(lines(A, B), "day1.txt"),
    msort(A, Asorted),
    msort(B, Bsorted),
    distance(Asorted,Bsorted, C),
    format("~w~n", [C]).

freq(_, [], 0).
freq(N, [A|As], C) :-
    (N = A -> C #= C1 + 1 ; C #= C1),
    freq(N, As, C1).

:- table freq/3.

similarity_score([], _, 0).
similarity_score([A|As], Bs, C) :-
    C #= (A * C1) + C2,
    freq(A, Bs, C1),
    similarity_score(As, Bs, C2).

part2 :-
    phrase_from_file(lines(A, B), "day1.txt"),
    similarity_score(A, B, C),
    format("~w~n", [C]).
