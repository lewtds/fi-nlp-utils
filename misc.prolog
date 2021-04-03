:- use_module(time).
:- use_module(number).

print_current_time :-
  get_time(T),
  stamp_date_time(T, D, local),
  date_time_value(hour, D, Hour),
  date_time_value(minute, D, Min),
  phrase(time_text(Hour, Min), Phrase),
  format("~s~n", [Phrase]).


print_test_cases :-
   setof({H, M, T}, (phrase(time_text(H, M), T)), Cases),
   maplist(print_test_case, Cases).

print_test_case({H, M, T}) :-
   format("test('~d:~d', [nondet]) :- phrase(time_text(~d, ~d), `~s`).~n", [H, M, H, M, T]).

train_number(Low, High) :-
  % get a random number
  random(Low, High, R),
  format("~d~n", R),

  % phrase it (there might be multiple forms)
  phrase(number:cardinal_number(Form, R), T),

  % print that phrase and say it out loud
  format("~w~n", [Form]),
  get_single_char(_),

  format(atom(A), "say -v Satu '~s'", [T]),
  format("~s~n", [T]),
  shell(A, []).
