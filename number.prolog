:- module(number, [cardinal_number/4]).
:- use_module(library(clpfd)).

% 0 -> 10
cardinal_number({nominative, singular}, 0) --> "nolla".
cardinal_number({nominative, singular}, 1) --> "yksi".
cardinal_number({nominative, singular}, 2) --> "kaksi".
cardinal_number({nominative, singular}, 3) --> "kolme".
cardinal_number({nominative, singular}, 4) --> "neljä".
cardinal_number({nominative, singular}, 5) --> "viisi".
cardinal_number({nominative, singular}, 6) --> "kuusi".
cardinal_number({nominative, singular}, 7) --> "seitsemän".
cardinal_number({nominative, singular}, 8) --> "kahdeksan".
cardinal_number({nominative, singular}, 9) --> "yhdeksän".
cardinal_number({nominative, singular}, 10) --> "kymmenen".

cardinal_number({genitive, singular}, 0) --> "nollan".
cardinal_number({genitive, singular}, 1) --> "yhden".
cardinal_number({genitive, singular}, 2) --> "kahden".
cardinal_number({genitive, singular}, 3) --> "kolmen".
cardinal_number({genitive, singular}, 4) --> "neljän".
cardinal_number({genitive, singular}, 5) --> "viiden".
cardinal_number({genitive, singular}, 6) --> "kuuden".
cardinal_number({genitive, singular}, 7) --> "seitsemän".
cardinal_number({genitive, singular}, 8) --> "kahdeksan".
cardinal_number({genitive, singular}, 9) --> "yhdeksän".
cardinal_number({genitive, singular}, 10) --> "kymmenen".

cardinal_number({partitive, singular}, 0) --> "nollaa".
cardinal_number({partitive, singular}, 1) --> "yktä".
cardinal_number({partitive, singular}, 2) --> "kahta".
cardinal_number({partitive, singular}, 3) --> "kolmea".
cardinal_number({partitive, singular}, 4) --> "neljää".
cardinal_number({partitive, singular}, 5) --> "viittä".
cardinal_number({partitive, singular}, 6) --> "kuutta".
cardinal_number({partitive, singular}, 7) --> "seitsemää".
cardinal_number({partitive, singular}, 8) --> "kahdeksaa".
cardinal_number({partitive, singular}, 9) --> "yhdeksää".
cardinal_number({partitive, singular}, 10) --> "kymmentä".

cardinal_number({nominative, plural}, 0) --> "nollat".
cardinal_number({nominative, plural}, 1) --> "yhdet".
cardinal_number({nominative, plural}, 2) --> "kahdet".
cardinal_number({nominative, plural}, 3) --> "kolmet".
cardinal_number({nominative, plural}, 4) --> "neljät".
cardinal_number({nominative, plural}, 5) --> "viidet".
cardinal_number({nominative, plural}, 6) --> "kuudet".
cardinal_number({nominative, plural}, 7) --> "seitsemät".
cardinal_number({nominative, plural}, 8) --> "kahdeksat".
cardinal_number({nominative, plural}, 9) --> "yhdeksät".
cardinal_number({nominative, plural}, 10) --> "kymmenet".

cardinal_number({genitive, plural}, 0) --> "nollien".
cardinal_number({genitive, plural}, 1) --> "yksien".
cardinal_number({genitive, plural}, 2) --> "kaksien".
cardinal_number({genitive, plural}, 3) --> "kolmien".
cardinal_number({genitive, plural}, 4) --> "neljien".
cardinal_number({genitive, plural}, 4) --> "neljäin".
cardinal_number({genitive, plural}, 5) --> "viisien".
cardinal_number({genitive, plural}, 5) --> "viitten".
cardinal_number({genitive, plural}, 6) --> "kuusien".
cardinal_number({genitive, plural}, 7) --> "seitsemien".
cardinal_number({genitive, plural}, 8) --> "kahdeksien".
cardinal_number({genitive, plural}, 8) --> "kahdeksain".
cardinal_number({genitive, plural}, 9) --> "yhdeksien".
cardinal_number({genitive, plural}, 10) --> "kymmenien".
cardinal_number({genitive, plural}, 10) --> "kymmenten".

cardinal_number({partitive, plural}, 0) --> "nollia".
cardinal_number({partitive, plural}, 1) --> "yksiä".
cardinal_number({partitive, plural}, 2) --> "kahsia".
cardinal_number({partitive, plural}, 3) --> "kolmia".
cardinal_number({partitive, plural}, 4) --> "neljiä".
cardinal_number({partitive, plural}, 5) --> "viisiä".
cardinal_number({partitive, plural}, 6) --> "kuusia".
cardinal_number({partitive, plural}, 7) --> "seitsemiä".
cardinal_number({partitive, plural}, 8) --> "kahdeksia".
cardinal_number({partitive, plural}, 9) --> "yhdeksiä".
cardinal_number({partitive, plural}, 10) --> "kymmeniä".

cardinal_number({nominative, singular}, 100) --> "sata".
cardinal_number({genitive, singular}, 100) --> "sadan".
cardinal_number({partitive, singular}, 100) --> "sataa".
cardinal_number({nominative, plural}, 100) --> "sadat".
cardinal_number({genitive, plural}, 100) --> "satojen".
cardinal_number({partitive, plural}, 100) --> "satoja".

cardinal_number({nominative, singular}, 1000) --> "tuhat".
cardinal_number({genitive, singular}, 1000) --> "tuhannen".
cardinal_number({partitive, singular}, 1000) --> "tuhatta".
cardinal_number({nominative, plural}, 1000) --> "tuhannet".
cardinal_number({genitive, plural}, 1000) --> "tuhansien".
cardinal_number({partitive, plural}, 1000) --> "tuhansia".

cardinal_number({nominative, singular}, 1_000_000) --> "miljoona".
cardinal_number({genitive, singular}, 1_000_000) --> "miljoonan".
cardinal_number({partitive, singular}, 1_000_000) --> "miljoonaa".
cardinal_number({nominative, plural}, 1_000_000) --> "miljoonat".
cardinal_number({genitive, plural}, 1_000_000) --> "miljoonien".
cardinal_number({partitive, plural}, 1_000_000) --> "miljoonia".

cardinal_number({nominative, singular}, 1_000_000_000) --> "miljardi".
cardinal_number({genitive, singular}, 1_000_000_000) --> "miljardin".
cardinal_number({partitive, singular}, 1_000_000_000) --> "miljardia".
cardinal_number({nominative, plural}, 1_000_000_000) --> "miljardit".
cardinal_number({genitive, plural}, 1_000_000_000) --> "miljardien".
cardinal_number({partitive, plural}, 1_000_000_000) --> "miljardeja".

cardinal_number({nominative, singular}, 1_000_000_000_000) --> "biljoona".
cardinal_number({genitive, singular}, 1_000_000_000_000) --> "biljoonan".
cardinal_number({partitive, singular}, 1_000_000_000_000) --> "biljoonaa".
cardinal_number({nominative, plural}, 1_000_000_000_000) --> "biljoonat".
cardinal_number({genitive, plural}, 1_000_000_000_000) --> "biljoonien".
cardinal_number({partitive, plural}, 1_000_000_000_000) --> "biljoonia".


% 11 -> 19
cardinal_number(Form, N) -->
  {
    N #= 10 + B, B in 1..9
  },
  cardinal_number(Form, B), "-toista".

% 20, 30, 40, 50, 60, 70, 80, 90
cardinal_number({Case, Plurality}, N) -->
  {
    N #= A * 10, A in 2..9
  },
  cardinal_number({Case, Plurality}, A), "-",
  cardinal_number({partitive, Plurality}, 10).

% 21 -> 99
cardinal_number({Case, Plurality}, N) -->
  {
    N #= A * 10 + B, A in 2..9, B in 1..9
  },
  cardinal_number({Case, Plurality}, A), "-",
  cardinal_number({partitive, Plurality}, 10), "-",
  cardinal_number({Case, Plurality}, B).

cardinal_number(Form, N) -->
  { N #= 100 + B, B in 1..99 },
  cardinal_number(Form, 100), "-",
  cardinal_number(Form, B).

cardinal_number({Case, Plurality}, N) -->
  { N #= A * 100, A in 2..9 },
  cardinal_number({Case, Plurality}, A), "-",
  cardinal_number({partitive, Plurality}, 100).

cardinal_number({Case, Plurality}, N) -->
  { N #= A * 100 + B, A in 2..9, B in 1..99 },
  cardinal_number({Case, Plurality}, A), "-",
  cardinal_number({partitive, Plurality}, 100), "-",
  cardinal_number({Case, Plurality}, B).

cardinal_number(Form, N) -->
  { N #= 1000 + B, B in 1..999 },
  cardinal_number(Form, 1000), "-",
  cardinal_number(Form, B).

cardinal_number({Case, Plurality}, N) -->
  { N #= A * 1000, A in 2..999 },
  cardinal_number({Case, Plurality}, A), "-",
  cardinal_number({partitive, Plurality}, 1000).

cardinal_number({Case, Plurality}, N) -->
  { N #= A * 1000 + B, A in 2..999, B in 1..999 },
  cardinal_number({Case, Plurality}, A), "-",
  cardinal_number({partitive, Plurality}, 1000), "-",
  cardinal_number({Case, Plurality}, B).

cardinal_number(Form, N) -->
  { N #= 1000000 + B, B in 1..999999 },
  cardinal_number(Form, 1000000), "-",
  cardinal_number(Form, B).

cardinal_number({Case, Plurality}, N) -->
  { N #= A * 1000000, A in 2..999 },
  cardinal_number({Case, Plurality}, A), "-",
  cardinal_number({partitive, Plurality}, 1000000).

cardinal_number({Case, Plurality}, N) -->
  { N #= A * 1000000 + B, A in 2..999, B in 1..999999 },
  cardinal_number({Case, Plurality}, A), "-",
  cardinal_number({partitive, Plurality}, 1000000), "-",
  cardinal_number({Case, Plurality}, B).

cardinal_number(Form, N) -->
  { N #= 1_000_000_000 + B, B in 1..999999999 },
  cardinal_number(Form, 1_000_000_000), "-",
  cardinal_number(Form, B).

cardinal_number({Case, Plurality}, N) -->
  { N #= A * 1000000, A in 2..999 },
  cardinal_number({Case, Plurality}, A), "-",
  cardinal_number({partitive, Plurality}, 1_000_000_000).

cardinal_number({Case, Plurality}, N) -->
  { N #= A * 1_000_000_000 + B, A in 2..999, B in 1..999999999 },
  cardinal_number({Case, Plurality}, A), "-",
  cardinal_number({partitive, Plurality}, 1_000_000_000), "-",
  cardinal_number({Case, Plurality}, B).

cardinal_number(Form, N) -->
  { N #= 1_000_000_000_000 + B, B in 1..999999999999 },
  cardinal_number(Form, 1_000_000_000_000), "-",
  cardinal_number(Form, B).

cardinal_number({Case, Plurality}, N) -->
  { N #= A * 1000000, A in 2..999 },
  cardinal_number({Case, Plurality}, A), "-",
  cardinal_number({partitive, Plurality}, 1_000_000_000_000).

cardinal_number({Case, Plurality}, N) -->
  { N #= A * 1_000_000_000_000 + B, A in 2..999, B in 1..999999999999 },
  cardinal_number({Case, Plurality}, A), "-",
  cardinal_number({partitive, Plurality}, 1_000_000_000_000), "-",
  cardinal_number({Case, Plurality}, B).
