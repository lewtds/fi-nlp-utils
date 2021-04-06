:- module(number, [number/5]).
:- use_module(library(clpfd)).

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

non_unit_primitive_number(ordinal, {nominative, singular}, 2) --> "kahdes".
non_unit_primitive_number(ordinal, {genitive, singular}, 2) --> "kahdennen".
non_unit_primitive_number(ordinal, {partitive, singular}, 2) --> "kahdetta".
non_unit_primitive_number(ordinal, {nominative, plural}, 2) --> "kahdennet".
non_unit_primitive_number(ordinal, {genitive, plural}, 2) --> "kahdensien".
non_unit_primitive_number(ordinal, {partitive, plural}, 2) --> "kahdensia".

primitive_number(cardinal, {nominative, singular}, 0) --> "nolla".
primitive_number(cardinal, {genitive, singular}, 0) --> "nollan".
primitive_number(cardinal, {partitive, singular}, 0) --> "nollaa".
primitive_number(cardinal, {nominative, plural}, 0) --> "nollat".
primitive_number(cardinal, {genitive, plural}, 0) --> "nollien".
primitive_number(cardinal, {partitive, plural}, 0) --> "nollia".

primitive_number(cardinal, {nominative, singular}, 1) --> "yksi".
primitive_number(cardinal, {genitive, singular}, 1) --> "yhden".
primitive_number(cardinal, {partitive, singular}, 1) --> "yktä".
primitive_number(cardinal, {nominative, plural}, 1) --> "yhdet".
primitive_number(cardinal, {genitive, plural}, 1) --> "yksien".
primitive_number(cardinal, {partitive, plural}, 1) --> "yksiä".

primitive_number(cardinal, {nominative, singular}, 2) --> "kaksi".
primitive_number(cardinal, {genitive, singular}, 2) --> "kahden".
primitive_number(cardinal, {partitive, singular}, 2) --> "kahta".
primitive_number(cardinal, {nominative, plural}, 2) --> "kahdet".
primitive_number(cardinal, {genitive, plural}, 2) --> "kaksien".
primitive_number(cardinal, {partitive, plural}, 2) --> "kahsia".

primitive_number(cardinal, {nominative, singular}, 3) --> "kolme".
primitive_number(cardinal, {genitive, singular}, 3) --> "kolmen".
primitive_number(cardinal, {partitive, singular}, 3) --> "kolmea".
primitive_number(cardinal, {nominative, plural}, 3) --> "kolmet".
primitive_number(cardinal, {genitive, plural}, 3) --> "kolmien".
primitive_number(cardinal, {partitive, plural}, 3) --> "kolmia".

primitive_number(cardinal, {nominative, singular}, 4) --> "neljä".
primitive_number(cardinal, {genitive, singular}, 4) --> "neljän".
primitive_number(cardinal, {partitive, singular}, 4) --> "neljää".
primitive_number(cardinal, {nominative, plural}, 4) --> "neljät".
primitive_number(cardinal, {genitive, plural}, 4) --> "neljien".
primitive_number(cardinal, {genitive, plural}, 4) --> "neljäin".
primitive_number(cardinal, {partitive, plural}, 4) --> "neljiä".

primitive_number(cardinal, {nominative, singular}, 5) --> "viisi".
primitive_number(cardinal, {genitive, singular}, 5) --> "viiden".
primitive_number(cardinal, {partitive, singular}, 5) --> "viittä".
primitive_number(cardinal, {nominative, plural}, 5) --> "viidet".
primitive_number(cardinal, {genitive, plural}, 5) --> "viisien".
primitive_number(cardinal, {genitive, plural}, 5) --> "viitten".
primitive_number(cardinal, {partitive, plural}, 5) --> "viisiä".

primitive_number(cardinal, {nominative, singular}, 6) --> "kuusi".
primitive_number(cardinal, {genitive, singular}, 6) --> "kuuden".
primitive_number(cardinal, {partitive, singular}, 6) --> "kuutta".
primitive_number(cardinal, {nominative, plural}, 6) --> "kuudet".
primitive_number(cardinal, {genitive, plural}, 6) --> "kuusien".
primitive_number(cardinal, {partitive, plural}, 6) --> "kuusia".

primitive_number(cardinal, {nominative, singular}, 7) --> "seitsemän".
primitive_number(cardinal, {genitive, singular}, 7) --> "seitsemän".
primitive_number(cardinal, {partitive, singular}, 7) --> "seitsemää".
primitive_number(cardinal, {nominative, plural}, 7) --> "seitsemät".
primitive_number(cardinal, {genitive, plural}, 7) --> "seitsemien".
primitive_number(cardinal, {partitive, plural}, 7) --> "seitsemiä".

primitive_number(cardinal, {nominative, singular}, 8) --> "kahdeksan".
primitive_number(cardinal, {genitive, singular}, 8) --> "kahdeksan".
primitive_number(cardinal, {partitive, singular}, 8) --> "kahdeksaa".
primitive_number(cardinal, {nominative, plural}, 8) --> "kahdeksat".
primitive_number(cardinal, {genitive, plural}, 8) --> "kahdeksien".
primitive_number(cardinal, {genitive, plural}, 8) --> "kahdeksain".
primitive_number(cardinal, {partitive, plural}, 8) --> "kahdeksia".

primitive_number(cardinal, {nominative, singular}, 9) --> "yhdeksän".
primitive_number(cardinal, {genitive, singular}, 9) --> "yhdeksän".
primitive_number(cardinal, {partitive, singular}, 9) --> "yhdeksää".
primitive_number(cardinal, {nominative, plural}, 9) --> "yhdeksät".
primitive_number(cardinal, {genitive, plural}, 9) --> "yhdeksien".
primitive_number(cardinal, {partitive, plural}, 9) --> "yhdeksiä".

primitive_number(cardinal, {nominative, singular}, 10) --> "kymmenen".
primitive_number(cardinal, {genitive, singular}, 10) --> "kymmenen".
primitive_number(cardinal, {partitive, singular}, 10) --> "kymmentä".
primitive_number(cardinal, {nominative, plural}, 10) --> "kymmenet".
primitive_number(cardinal, {genitive, plural}, 10) --> "kymmenien".
primitive_number(cardinal, {genitive, plural}, 10) --> "kymmenten".
primitive_number(cardinal, {partitive, plural}, 10) --> "kymmeniä".

primitive_number(cardinal, {nominative, singular}, 100) --> "sata".
primitive_number(cardinal, {genitive, singular}, 100) --> "sadan".
primitive_number(cardinal, {partitive, singular}, 100) --> "sataa".
primitive_number(cardinal, {nominative, plural}, 100) --> "sadat".
primitive_number(cardinal, {genitive, plural}, 100) --> "satojen".
primitive_number(cardinal, {partitive, plural}, 100) --> "satoja".

primitive_number(cardinal, {nominative, singular}, 1000) --> "tuhat".
primitive_number(cardinal, {genitive, singular}, 1000) --> "tuhannen".
primitive_number(cardinal, {partitive, singular}, 1000) --> "tuhatta".
primitive_number(cardinal, {nominative, plural}, 1000) --> "tuhannet".
primitive_number(cardinal, {genitive, plural}, 1000) --> "tuhansien".
primitive_number(cardinal, {partitive, plural}, 1000) --> "tuhansia".

primitive_number(cardinal, {nominative, singular}, 1_000_000) --> "miljoona".
primitive_number(cardinal, {genitive, singular}, 1_000_000) --> "miljoonan".
primitive_number(cardinal, {partitive, singular}, 1_000_000) --> "miljoonaa".
primitive_number(cardinal, {nominative, plural}, 1_000_000) --> "miljoonat".
primitive_number(cardinal, {genitive, plural}, 1_000_000) --> "miljoonien".
primitive_number(cardinal, {partitive, plural}, 1_000_000) --> "miljoonia".

primitive_number(cardinal, {nominative, singular}, 1_000_000_000) --> "miljardi".
primitive_number(cardinal, {genitive, singular}, 1_000_000_000) --> "miljardin".
primitive_number(cardinal, {partitive, singular}, 1_000_000_000) --> "miljardia".
primitive_number(cardinal, {nominative, plural}, 1_000_000_000) --> "miljardit".
primitive_number(cardinal, {genitive, plural}, 1_000_000_000) --> "miljardien".
primitive_number(cardinal, {partitive, plural}, 1_000_000_000) --> "miljardeja".

primitive_number(cardinal, {nominative, singular}, 1_000_000_000_000) --> "biljoona".
primitive_number(cardinal, {genitive, singular}, 1_000_000_000_000) --> "biljoonan".
primitive_number(cardinal, {partitive, singular}, 1_000_000_000_000) --> "biljoonaa".
primitive_number(cardinal, {nominative, plural}, 1_000_000_000_000) --> "biljoonat".
primitive_number(cardinal, {genitive, plural}, 1_000_000_000_000) --> "biljoonien".
primitive_number(cardinal, {partitive, plural}, 1_000_000_000_000) --> "biljoonia".

primitive_number(ordinal, {nominative, singular}, 0) --> "nollas".
primitive_number(ordinal, {genitive, singular}, 0) --> "nollannen".
primitive_number(ordinal, {partitive, singular}, 0) --> "nollatta".
primitive_number(ordinal, {nominative, plural}, 0) --> "nollannet".
primitive_number(ordinal, {genitive, plural}, 0) --> "nollansien".
primitive_number(ordinal, {partitive, plural}, 0) --> "nollansia".


primitive_number(ordinal, {nominative, singular}, 1) --> "ensimmäinen".
primitive_number(ordinal, {genitive, singular}, 1) --> "ensimmäisen".
primitive_number(ordinal, {partitive, singular}, 1) --> "ensimmäistä".
primitive_number(ordinal, {nominative, plural}, 1) --> "ensimmäiset".
primitive_number(ordinal, {genitive, plural}, 1) --> "ensimmäisten".
primitive_number(ordinal, {genitive, plural}, 1) --> "ensimmäisien".
primitive_number(ordinal, {partitive, plural}, 1) --> "ensimmäisiä".


primitive_number(ordinal, {nominative, singular}, 2) --> "toinen".
primitive_number(ordinal, {genitive, singular}, 2) --> "toisen".
primitive_number(ordinal, {partitive, singular}, 2) --> "toista".
primitive_number(ordinal, {nominative, plural}, 2) --> "toiset".
primitive_number(ordinal, {genitive, plural}, 2) --> "toisten".
primitive_number(ordinal, {genitive, plural}, 2) --> "toisien".
primitive_number(ordinal, {partitive, plural}, 2) --> "toisia".


primitive_number(ordinal, {nominative, singular}, 3) --> "kolmas".
primitive_number(ordinal, {genitive, singular}, 3) --> "kolmannen".
primitive_number(ordinal, {partitive, singular}, 3) --> "kolmatta".
primitive_number(ordinal, {nominative, plural}, 3) --> "kolmannet".
primitive_number(ordinal, {genitive, plural}, 3) --> "kolmansien".
primitive_number(ordinal, {partitive, plural}, 3) --> "kolmansia".

primitive_number(ordinal, {nominative, singular}, 4) --> "neljäs".
primitive_number(ordinal, {genitive, singular}, 4) --> "neljännen".
primitive_number(ordinal, {partitive, singular}, 4) --> "neljättä".
primitive_number(ordinal, {nominative, plural}, 4) --> "neljännet".
primitive_number(ordinal, {genitive, plural}, 4) --> "neljänsien".
primitive_number(ordinal, {partitive, plural}, 4) --> "neljänsiä".

primitive_number(ordinal, {nominative, singular}, 5) --> "viides".
primitive_number(ordinal, {genitive, singular}, 5) --> "viidennen".
primitive_number(ordinal, {partitive, singular}, 5) --> "viidettä".
primitive_number(ordinal, {nominative, plural}, 5) --> "viidennet".
primitive_number(ordinal, {genitive, plural}, 5) --> "viidensien".
primitive_number(ordinal, {partitive, plural}, 5) --> "viidensiä".

primitive_number(ordinal, {nominative, singular}, 6) --> "kuudes".
primitive_number(ordinal, {genitive, singular}, 6) --> "kuudennen".
primitive_number(ordinal, {partitive, singular}, 6) --> "kuudetta".
primitive_number(ordinal, {nominative, plural}, 6) --> "kuudennet".
primitive_number(ordinal, {genitive, plural}, 6) --> "kuudensien".
primitive_number(ordinal, {partitive, plural}, 6) --> "kuudensia".

primitive_number(ordinal, {nominative, singular}, 7) --> "seitsemäs".
primitive_number(ordinal, {genitive, singular}, 7) --> "seitsemännen".
primitive_number(ordinal, {partitive, singular}, 7) --> "seitsemättä".
primitive_number(ordinal, {nominative, plural}, 7) --> "seitsemännet".
primitive_number(ordinal, {genitive, plural}, 7) --> "seitsemänsien".
primitive_number(ordinal, {partitive, plural}, 7) --> "seitsemänsiä".

primitive_number(ordinal, {nominative, singular}, 8) --> "kahdeksas".
primitive_number(ordinal, {genitive, singular}, 8) --> "kahdeksannen".
primitive_number(ordinal, {partitive, singular}, 8) --> "kahdeksatta".
primitive_number(ordinal, {nominative, plural}, 8) --> "kahdeksannet".
primitive_number(ordinal, {genitive, plural}, 8) --> "kahdeksansien".
primitive_number(ordinal, {genitive, plural}, 8) --> "yhdeksänsien".
primitive_number(ordinal, {partitive, plural}, 8) --> "kahdeksansia".

primitive_number(ordinal, {nominative, singular}, 9) --> "yhdeksäs".
primitive_number(ordinal, {genitive, singular}, 9) --> "yhdeksännen".
primitive_number(ordinal, {partitive, singular}, 9) --> "yhdeksättä".
primitive_number(ordinal, {nominative, plural}, 9) --> "yhdeksännet".
primitive_number(ordinal, {nominative, plural}, 9) --> "yhdeksännet".
primitive_number(ordinal, {partitive, plural}, 9) --> "yhdeksänsiä".

primitive_number(ordinal, {nominative, singular}, 10) --> "kymmenes".
primitive_number(ordinal, {genitive, singular}, 10) --> "kymmenennen".
primitive_number(ordinal, {partitive, singular}, 10) --> "kymmenettä".
primitive_number(ordinal, {nominative, plural}, 10) --> "kymmenennet".
primitive_number(ordinal, {genitive, plural}, 10) --> "kymmenensien".
primitive_number(ordinal, {partitive, plural}, 10) --> "kymmenensiä".

primitive_number(ordinal, {nominative, singular}, 100) --> "sadas".
primitive_number(ordinal, {genitive, singular}, 100) --> "sadannen".
primitive_number(ordinal, {partitive, singular}, 100) --> "sadatta".
primitive_number(ordinal, {nominative, plural}, 100) --> "sadannet".
primitive_number(ordinal, {genitive, plural}, 100) --> "sadansien".
primitive_number(ordinal, {partitive, plural}, 100) --> "sadansia".

primitive_number(ordinal, {nominative, singular}, 1000) --> "tuhannes".
primitive_number(ordinal, {genitive, singular}, 1000) --> "tuhannennen".
primitive_number(ordinal, {partitive, singular}, 1000) --> "tuhannetta".
primitive_number(ordinal, {nominative, plural}, 1000) --> "tuhannennet".
primitive_number(ordinal, {genitive, plural}, 1000) --> "tuhannensien".
primitive_number(ordinal, {partitive, plural}, 1000) --> "tuhannensia".

primitive_number(ordinal, {nominative, singular}, 1_000_000) --> "miljoonas".
primitive_number(ordinal, {genitive, singular}, 1_000_000) --> "miljoonannen".
primitive_number(ordinal, {partitive, singular}, 1_000_000) --> "miljoonatta".
primitive_number(ordinal, {nominative, plural}, 1_000_000) --> "miljoonannet".
primitive_number(ordinal, {genitive, plural}, 1_000_000) --> "miljoonansien".
primitive_number(ordinal, {partitive, plural}, 1_000_000) --> "miljoonansia".

primitive_number(ordinal, {nominative, singular}, 1_000_000_000) --> "miljardis".
primitive_number(ordinal, {genitive, singular}, 1_000_000_000) --> "miljardinnen".
primitive_number(ordinal, {partitive, singular}, 1_000_000_000) --> "miljarditta".
primitive_number(ordinal, {nominative, plural}, 1_000_000_000) --> "miljardinnet".
primitive_number(ordinal, {genitive, plural}, 1_000_000_000) --> "miljardinsien".
primitive_number(ordinal, {partitive, plural}, 1_000_000_000) --> "miljardinsia".

primitive_number(ordinal, {nominative, singular}, 1_000_000_000_000) --> "biljoonas".
primitive_number(ordinal, {genitive, singular}, 1_000_000_000_000) --> "biljoonannen".
primitive_number(ordinal, {partitive, singular}, 1_000_000_000_000) --> "biljoonatta".
primitive_number(ordinal, {nominative, plural}, 1_000_000_000_000) --> "biljoonannet".
primitive_number(ordinal, {genitive, plural}, 1_000_000_000_000) --> "biljoonansien".
primitive_number(ordinal, {partitive, plural}, 1_000_000_000_000) --> "biljoonansia".
