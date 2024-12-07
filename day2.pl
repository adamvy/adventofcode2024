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

msort(Ls0, Ls) :-
        pairs_keys(Pairs0, Ls0),
        keysort(Pairs0, Pairs),
        pairs_keys(Pairs, Ls).

ws --> [W], { char_type(W, whitespace) }, ws.
ws --> [].

number(N) -->
    digit_chars(D),
    { catch(number_chars(N, D), error(syntax_error(_), _), false) }.

digit_chars([]) --> [].
digit_chars([D|Ds]) -->
    [D],
    { char_type(D, numeric) },
    digit_chars(Ds).

numbers(_) --> [].
numbers([A|As]) --> number(A), " ", numbers(As).
numbers([A]) --> number(A).

line(A) --> numbers(A), "\n".

lines([]) --> [].
lines([A|As]) --> line(A), lines(As).

% a row is safe iff
%   its empty
%   the delta between two elements is 1 <= delta <= 3
%   the delta is a consistent sign throughout the whole row
safe_([], _).
safe_([_], _).
safe_([A1, A2|As], Increasing) :-
    Delta #= A1 - A2,
    abs(Delta) #< 4,
    abs(Delta) #> 0,
    Delta #> 0 #<==> Increasing,
    safe_([A2|As], Increasing).

safe(L) :- safe_(L, _).

count(L, S0, S) :-
    safe(L),
    S #= S0 + 1.
count(_, S, S).

part1(File) :-
    phrase_from_file(lines(Input), File),
    foldl(count, Input, 0, C),
    format("~w~n", [C]).

sequence([]) --> [].
sequence([A|As]) --> [A], sequence(As).

drop_one(Bs) --> [_], sequence(Bs).
drop_one([B|Bs]) --> [B], drop_one(Bs).

safe2(L) :-
    phrase(drop_one(L2), L), safe(L2).


count2(L, S0, S) :-
    safe2(L),
    S #= S0 + 1.
count2(_, S, S).

part2(File) :-
    phrase_from_file(lines(Input), File),
    foldl(count2, Input, 0, C),
    format("~w~n", [C]).

go :-
    part1("day2example.txt"),
    part1("day2.txt"),
    part2("day2example.txt"),
    part2("day2.txt").
