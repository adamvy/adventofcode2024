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

msort(Ls0, Ls) :-
        pairs_keys(Pairs0, Ls0),
        keysort(Pairs0, Pairs),
        pairs_keys(Pairs, Ls).

% xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))


digit_chars([]) --> [].
digit_chars([D|Ds]) -->
    [D],
    { char_type(D, numeric) },
    digit_chars(Ds).

number(N) -->
    digit_chars(D),
    { catch(number_chars(N, D), error(syntax_error(_), _), false) }.

mul, [R] --> "mul(", number(A), ",", number(B), ")", { R #= A * B }.

any --> [_].

line(S) --> mul, [A], line(B), { S #= A + B }.
line(S) --> any, line(S).
line(0) --> [].


part1(File) :-
    phrase_from_file(line(R), File),
    format("~w~n", [R]).


docmd --> "do()".
dontcmd --> "don't()".

enabled(S) --> mul, [A], enabled(S0), { S #= S0 + A }.
enabled(S) --> dontcmd, disabled(S).
enabled(S) --> any, enabled(S).
enabled(0) --> [].

disabled(S) --> docmd, enabled(S).
disabled(S) --> any, disabled(S).
disabled(0) --> [].

part2(File) :-
    phrase_from_file(enabled(R), File),
    format("~w~n", [R]).

go :-
    part2("day3.txt").
