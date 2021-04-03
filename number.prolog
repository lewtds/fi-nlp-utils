:- module(number, [cardinal_number/4]).
:- use_module(library(clpfd)).

cardinal_number(Form, N) -->
  { N in 0..10 },
  primitive_cardinal_number(Form, N).

cardinal_number(Form, N) -->
  {
    N #= 10 + N0, N0 in 1..9
  },
  primitive_cardinal_number(Form, N0), "toista".

cardinal_number(Form, N) -->
  {
    N #>= 20,

    % Fastest but not reversible
    % divmod(N,   1_000_000_000_000, N12, R12),
    % divmod(R12, 1_000_000_000, N9, R9),
    % divmod(R9,  1_000_000, N6, R6),
    % divmod(R6,  1_000, N3, R3),
    % divmod(R3,  1_00, N2, R2),
    % divmod(R2,  1_0, N1, N0)

    % About 5 times slower than plain divmod
    N12 #= N div 1_000_000_000_000,
    R12 #= N mod 1_000_000_000_000,

    N9 #= R12 div 1_000_000_000,
    R9 #= R12 mod 1_000_000_000,

    N6 #= R9 div 1_000_000,
    R6 #= R9 mod 1_000_000,

    N3 #= R6 div 1_000,
    R3 #= R6 mod 1_000,

    N2 #= R3 div 1_00,
    R2 #= R3 mod 1_00,

    N1 #= R2 div 1_0,
    N0 #= R2 mod 1_0

    % Looks the nicest but about 4 times slower than clpfd div mod and 20 times slower than plain divmod
    % N #= 1_000_000_000_000*N12 + 1_000_000_000*N9 + 1_000_000*N6 + 1_000*N3 + 1_00*N2 + 1_0*N1 + N0,
    % N12 in 0..sup,
    % N9 in 0..999,
    % N6 in 0..999,
    % N3 in 0..999,
    % N2 in 0..9,
    % N1 in 0..9,
    % N0 in 0..9
  },

  mid_cardinal_number(Form, 1_000_000_000_000, N12),
  mid_cardinal_number(Form, 1_000_000_000, N9),
  mid_cardinal_number(Form, 1_000_000, N6),
  mid_cardinal_number(Form, 1_000, N3),
  mid_cardinal_number(Form, 1_00, N2),
  mid_cardinal_number(Form, 1_0, N1),
  mid_cardinal_number(Form, 1, N0).

mid_cardinal_number(_, _, 0) --> "".
mid_cardinal_number(Form, Scale, 1) --> {Scale #> 1}, primitive_cardinal_number(Form, Scale).
mid_cardinal_number(Form, 1, N) --> {N #> 0}, primitive_cardinal_number(Form, N).
mid_cardinal_number({nominative, singular}, Scale, N) --> { Scale #> 1, N #> 1}, cardinal_number({nominative, singular}, N), primitive_cardinal_number({partitive, singular}, Scale).
mid_cardinal_number(Form, Scale, N) --> { Form \= {nominative, singular}, Scale #> 1, N #> 1}, cardinal_number(Form, N), primitive_cardinal_number(Form, Scale).

primitive_cardinal_number({nominative, singular}, 0) --> "nolla".
primitive_cardinal_number({nominative, singular}, 1) --> "yksi".
primitive_cardinal_number({nominative, singular}, 2) --> "kaksi".
primitive_cardinal_number({nominative, singular}, 3) --> "kolme".
primitive_cardinal_number({nominative, singular}, 4) --> "neljä".
primitive_cardinal_number({nominative, singular}, 5) --> "viisi".
primitive_cardinal_number({nominative, singular}, 6) --> "kuusi".
primitive_cardinal_number({nominative, singular}, 7) --> "seitsemän".
primitive_cardinal_number({nominative, singular}, 8) --> "kahdeksan".
primitive_cardinal_number({nominative, singular}, 9) --> "yhdeksän".
primitive_cardinal_number({nominative, singular}, 10) --> "kymmenen".

primitive_cardinal_number({genitive, singular}, 0) --> "nollan".
primitive_cardinal_number({genitive, singular}, 1) --> "yhden".
primitive_cardinal_number({genitive, singular}, 2) --> "kahden".
primitive_cardinal_number({genitive, singular}, 3) --> "kolmen".
primitive_cardinal_number({genitive, singular}, 4) --> "neljän".
primitive_cardinal_number({genitive, singular}, 5) --> "viiden".
primitive_cardinal_number({genitive, singular}, 6) --> "kuuden".
primitive_cardinal_number({genitive, singular}, 7) --> "seitsemän".
primitive_cardinal_number({genitive, singular}, 8) --> "kahdeksan".
primitive_cardinal_number({genitive, singular}, 9) --> "yhdeksän".
primitive_cardinal_number({genitive, singular}, 10) --> "kymmenen".

primitive_cardinal_number({partitive, singular}, 0) --> "nollaa".
primitive_cardinal_number({partitive, singular}, 1) --> "yktä".
primitive_cardinal_number({partitive, singular}, 2) --> "kahta".
primitive_cardinal_number({partitive, singular}, 3) --> "kolmea".
primitive_cardinal_number({partitive, singular}, 4) --> "neljää".
primitive_cardinal_number({partitive, singular}, 5) --> "viittä".
primitive_cardinal_number({partitive, singular}, 6) --> "kuutta".
primitive_cardinal_number({partitive, singular}, 7) --> "seitsemää".
primitive_cardinal_number({partitive, singular}, 8) --> "kahdeksaa".
primitive_cardinal_number({partitive, singular}, 9) --> "yhdeksää".
primitive_cardinal_number({partitive, singular}, 10) --> "kymmentä".

primitive_cardinal_number({nominative, plural}, 0) --> "nollat".
primitive_cardinal_number({nominative, plural}, 1) --> "yhdet".
primitive_cardinal_number({nominative, plural}, 2) --> "kahdet".
primitive_cardinal_number({nominative, plural}, 3) --> "kolmet".
primitive_cardinal_number({nominative, plural}, 4) --> "neljät".
primitive_cardinal_number({nominative, plural}, 5) --> "viidet".
primitive_cardinal_number({nominative, plural}, 6) --> "kuudet".
primitive_cardinal_number({nominative, plural}, 7) --> "seitsemät".
primitive_cardinal_number({nominative, plural}, 8) --> "kahdeksat".
primitive_cardinal_number({nominative, plural}, 9) --> "yhdeksät".
primitive_cardinal_number({nominative, plural}, 10) --> "kymmenet".

primitive_cardinal_number({genitive, plural}, 0) --> "nollien".
primitive_cardinal_number({genitive, plural}, 1) --> "yksien".
primitive_cardinal_number({genitive, plural}, 2) --> "kaksien".
primitive_cardinal_number({genitive, plural}, 3) --> "kolmien".
primitive_cardinal_number({genitive, plural}, 4) --> "neljien".
primitive_cardinal_number({genitive, plural}, 4) --> "neljäin".
primitive_cardinal_number({genitive, plural}, 5) --> "viisien".
primitive_cardinal_number({genitive, plural}, 5) --> "viitten".
primitive_cardinal_number({genitive, plural}, 6) --> "kuusien".
primitive_cardinal_number({genitive, plural}, 7) --> "seitsemien".
primitive_cardinal_number({genitive, plural}, 8) --> "kahdeksien".
primitive_cardinal_number({genitive, plural}, 8) --> "kahdeksain".
primitive_cardinal_number({genitive, plural}, 9) --> "yhdeksien".
primitive_cardinal_number({genitive, plural}, 10) --> "kymmenien".
primitive_cardinal_number({genitive, plural}, 10) --> "kymmenten".

primitive_cardinal_number({partitive, plural}, 0) --> "nollia".
primitive_cardinal_number({partitive, plural}, 1) --> "yksiä".
primitive_cardinal_number({partitive, plural}, 2) --> "kahsia".
primitive_cardinal_number({partitive, plural}, 3) --> "kolmia".
primitive_cardinal_number({partitive, plural}, 4) --> "neljiä".
primitive_cardinal_number({partitive, plural}, 5) --> "viisiä".
primitive_cardinal_number({partitive, plural}, 6) --> "kuusia".
primitive_cardinal_number({partitive, plural}, 7) --> "seitsemiä".
primitive_cardinal_number({partitive, plural}, 8) --> "kahdeksia".
primitive_cardinal_number({partitive, plural}, 9) --> "yhdeksiä".
primitive_cardinal_number({partitive, plural}, 10) --> "kymmeniä".

primitive_cardinal_number({nominative, singular}, 100) --> "sata".
primitive_cardinal_number({genitive, singular}, 100) --> "sadan".
primitive_cardinal_number({partitive, singular}, 100) --> "sataa".
primitive_cardinal_number({nominative, plural}, 100) --> "sadat".
primitive_cardinal_number({genitive, plural}, 100) --> "satojen".
primitive_cardinal_number({partitive, plural}, 100) --> "satoja".

primitive_cardinal_number({nominative, singular}, 1000) --> "tuhat".
primitive_cardinal_number({genitive, singular}, 1000) --> "tuhannen".
primitive_cardinal_number({partitive, singular}, 1000) --> "tuhatta".
primitive_cardinal_number({nominative, plural}, 1000) --> "tuhannet".
primitive_cardinal_number({genitive, plural}, 1000) --> "tuhansien".
primitive_cardinal_number({partitive, plural}, 1000) --> "tuhansia".

primitive_cardinal_number({nominative, singular}, 1_000_000) --> "miljoona".
primitive_cardinal_number({genitive, singular}, 1_000_000) --> "miljoonan".
primitive_cardinal_number({partitive, singular}, 1_000_000) --> "miljoonaa".
primitive_cardinal_number({nominative, plural}, 1_000_000) --> "miljoonat".
primitive_cardinal_number({genitive, plural}, 1_000_000) --> "miljoonien".
primitive_cardinal_number({partitive, plural}, 1_000_000) --> "miljoonia".

primitive_cardinal_number({nominative, singular}, 1_000_000_000) --> "miljardi".
primitive_cardinal_number({genitive, singular}, 1_000_000_000) --> "miljardin".
primitive_cardinal_number({partitive, singular}, 1_000_000_000) --> "miljardia".
primitive_cardinal_number({nominative, plural}, 1_000_000_000) --> "miljardit".
primitive_cardinal_number({genitive, plural}, 1_000_000_000) --> "miljardien".
primitive_cardinal_number({partitive, plural}, 1_000_000_000) --> "miljardeja".

primitive_cardinal_number({nominative, singular}, 1_000_000_000_000) --> "biljoona".
primitive_cardinal_number({genitive, singular}, 1_000_000_000_000) --> "biljoonan".
primitive_cardinal_number({partitive, singular}, 1_000_000_000_000) --> "biljoonaa".
primitive_cardinal_number({nominative, plural}, 1_000_000_000_000) --> "biljoonat".
primitive_cardinal_number({genitive, plural}, 1_000_000_000_000) --> "biljoonien".
primitive_cardinal_number({partitive, plural}, 1_000_000_000_000) --> "biljoonia".
