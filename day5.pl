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

bubblesort(L1, L2, Compare) :-
    append(Left, [A,B|Right], L1), call(Compare, A, B) ->
    append(Left, [B,A|Right], L3),
    bubblesort(L3, L2, Compare)
    ; L1 = L2.

middle(L, M) :-
    append(Head, [M|Tail], L),
    same_length(Head, Tail).

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

data(Rules, Requests) --> rules(Rules), "\n", requests(Requests).

rules([R]) --> order(R).
rules([R|Rs]) --> order(R), rules(Rs).
order(page_order(A,B)) --> number(A), "|", number(B), "\n".

requests([R]) --> request(R).
requests([R|Rs]) --> request(R), requests(Rs).

request([N]) --> number(N), "\n".
request([N|Ns]) --> number(N), ",", request(Ns).

valid([_]).
valid([A,B|Ps]) :-
    page_order(A, B),
    valid([B|Ps]).


request_weight(Request, Weight) :-
    valid(Request),
    middle(Request, Weight).
request_weight(Request, 0) :-
    \+ valid(Request).

part1(File, S) :-
    phrase_from_file(data(Rules, Requests), File),
    retractall(page_order(_, _)),
    maplist(assertz, Rules),
    maplist(request_weight, Requests, Weights),
    sum_list(Weights, S).

request_weight2(Request, 0) :-
    valid(Request).

request_weight2(Request, Weight) :-
    \+ valid(Request),
    bubblesort(Request, Sorted, page_order),
    middle(Sorted, Weight).

part2(File, S) :-
    phrase_from_file(data(Rules, Requests), File),
    retractall(page_order(_, _)),
    maplist(assertz, Rules),
    maplist(request_weight2, Requests, Weights),
    sum_list(Weights, S).

