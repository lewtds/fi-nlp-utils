:- module(number, [number/5]).
:- use_module(library(clpfd)).
:- use_module(inflect).

% Type: cardinal|ordinal
% Form: {Case, Plurality}
% Case: nominative|partitive|genitive
% Plurality: singular|plural
number(Type, Form, N) -->
  { N in 0..10 },
  primitive_number(Type, Form, N).

number(Type, Form, N) -->
  { N #= 10 + N0, N0 in 1..9 },
  non_unit_primitive_number(Type, Form, N0), "toista".

number(Type, Form, N) -->
  {
    N in 20..99,
    N #= 10*N1 + N0, N1 in 2..9, N0 in 0..9,
    tens_form(Type, Form, TensForm)
  },
  non_unit_primitive_number(Type, Form, N1),
  non_unit_primitive_number(Type, TensForm, 10),
  mid_number(Type, Form, 1, N0).

number(Type, Form, N) -->
  {
    N #>= 100,

    N12 #= N div 1_000_000_000_000,
    R12 #= N mod 1_000_000_000_000,

    N9 #= R12 div 1_000_000_000,
    R9 #= R12 mod 1_000_000_000,

    N6 #= R9 div 1_000_000,
    R6 #= R9 mod 1_000_000,

    N3 #= R6 div 1_000,
    R3 #= R6 mod 1_000,

    N2 #= R3 div 1_00,
    N1_0 #= R3 mod 1_00
  },

  mid_number(Type, Form, 1_000_000_000_000, N12),
  mid_number(Type, Form, 1_000_000_000, N9),
  mid_number(Type, Form, 1_000_000, N6),
  mid_number(Type, Form, 1_000, N3),
  mid_number(Type, Form, 1_00, N2),
  number(Type, Form, N1_0).

% the columns with just 0
mid_number(_, _, _, 0) --> "".

% the unit in numbers in 20..99
mid_number(Type, Form, 1, N) --> { N #> 0 }, primitive_number(Type, Form, N).

% the single sata, tuhat, etc.
mid_number(Type, Form, Scale, 1) --> { Scale #> 1 }, non_unit_primitive_number(Type, Form, Scale).

% kahdes sadas
mid_number(Type, Form, Scale, N) --> {
    Scale #> 1,
    N in 2..10,
    tens_form(Type, Form, TensForm)
  },
  non_unit_leq_ten_number(Type, Form, N),
  non_unit_primitive_number(Type, TensForm, Scale).

% kahdes toista sadas
mid_number(Type, Form, Scale, N) --> {
    Scale #> 1,
    N #> 10,
    tens_form(Type, Form, TensForm)
  },
  number(Type, Form, N),
  non_unit_primitive_number(Type, TensForm, Scale).

% Only with nominative, singular cardinal numbers, the units
% (tens, hundreds, thousands, etc.) are in partitive singular
tens_form(Type, Form, TensForm) :-
  (Type = cardinal, Form = {nominative, singular}) ->
    TensForm = {partitive, singular};
    TensForm = Form.

non_unit_leq_ten_number(Type, Form, N) -->
  { N in 0..10 },
  non_unit_primitive_number(Type, Form, N).

% only for ordinal numbers, the number "2" is pronounced "toinen" only in the unit position
% otherwise it's kahdes
non_unit_primitive_number(Type, Form, N) --> { Type = cardinal; N #\= 2 }, primitive_number(Type, Form, N).

non_unit_primitive_number(ordinal, Form, 2) --> inflect(Form, "kahdes").

primitive_number(cardinal, Form, 0) --> inflect(Form, "nolla").
primitive_number(cardinal, Form, 1) --> inflect(Form, "yksi").
primitive_number(cardinal, Form, 2) --> inflect(Form, "kaksi").
primitive_number(cardinal, Form, 3) --> inflect(Form, "kolme").
primitive_number(cardinal, Form, 4) --> inflect(Form, "neljä").
primitive_number(cardinal, Form, 5) --> inflect(Form, "viisi").
primitive_number(cardinal, Form, 6) --> inflect(Form, "kuusi").
primitive_number(cardinal, Form, 7) --> inflect(Form, "seitsemän").
primitive_number(cardinal, Form, 8) --> inflect(Form, "kahdeksan").
primitive_number(cardinal, Form, 9) --> inflect(Form, "yhdeksän").
primitive_number(cardinal, Form, 10) --> inflect(Form, "kymmenen").
primitive_number(cardinal, Form, 100) --> inflect(Form, "sata").
primitive_number(cardinal, Form, 1_000) --> inflect(Form, "tuhat").
primitive_number(cardinal, Form, 1_000_000) --> inflect(Form, "miljoona").
primitive_number(cardinal, Form, 1_000_000_000) --> inflect(Form, "miljardi").
primitive_number(cardinal, Form, 1_000_000_000_000) --> inflect(Form, "biljoona").

primitive_number(ordinal, Form, 0) --> inflect(Form, "nollas").
primitive_number(ordinal, Form, 1) --> inflect(Form, "ensimmäinen").
primitive_number(ordinal, Form, 2) --> inflect(Form, "toinen").
primitive_number(ordinal, Form, 3) --> inflect(Form, "kolmas").
primitive_number(ordinal, Form, 4) --> inflect(Form, "neljäs").
primitive_number(ordinal, Form, 5) --> inflect(Form, "viides").
primitive_number(ordinal, Form, 6) --> inflect(Form, "kuudes").
primitive_number(ordinal, Form, 7) --> inflect(Form, "seitsemäs").
primitive_number(ordinal, Form, 8) --> inflect(Form, "kahdeksas").
primitive_number(ordinal, Form, 9) --> inflect(Form, "yhdeksäs").
primitive_number(ordinal, Form, 10) --> inflect(Form, "kymmenes").
primitive_number(ordinal, Form, 100) --> inflect(Form, "sadas").
primitive_number(ordinal, Form, 1_000) --> inflect(Form, "tuhannes").
primitive_number(ordinal, Form, 1_000_000) --> inflect(Form, "miljoonas").
primitive_number(ordinal, Form, 1_000_000_000) --> inflect(Form, "miljardis").
primitive_number(ordinal, Form, 1_000_000_000_000) --> inflect(Form, "biljoonas").
