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


n_queens(N, Qs) :-
    length(Qs, N),
    Qs in 1..N,
    safe_queens(Qs).

safe_queens()
safe_queens([Q|Qs]) :-
