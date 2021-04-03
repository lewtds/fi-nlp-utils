:- module(time, [time_text/4]).
:- use_module(library(clpfd)).
:- use_module(number).

% Full hour
time_text(Hour, 0) --> "tasan ", hour_text(Hour).
time_text(Hour, 0) --> hour_text(Hour).

% Half hour
time_text(Hour, 30) --> "puoli ", {next_24h_wrapped(Hour, NextHour)}, hour_text(NextHour).

% First half
time_text(Hour, Min) --> {Min #> 0, Min #< 30}, first_half_minute_text(Min), " yli ", hour_text(Hour).

% Second half
time_text(Hour, Min) --> {Min #> 30, Min #< 60, MinLeft #= 60 - Min}, second_half_minute_text(MinLeft), " vaille ", hour_text(Hour).

first_half_minute_text(15) --> "vartin".
first_half_minute_text(M) --> {valid_minute(M)}, cardinal_number({nominative, singular}, M).

second_half_minute_text(M) --> {valid_minute(M)}, cardinal_number({partitive, singular}, M).

next_24h_wrapped(23, 0) :- !.
next_24h_wrapped(H, N) :- N #= H + 1.

next_12h_wrapped(12, 1) :- !.
next_12h_wrapped(H, N) :- N #= H + 1.

hour_text(H) --> {valid_24h(H)}, cardinal_number({nominative, singular}, H).

valid_12h(H) :- H in 1..12.
valid_24h(H) :- H in 0..23.
valid_minute(M) :- M in 0..59.