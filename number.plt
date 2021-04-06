:- begin_tests(number).
:- use_module(number).

test('123456789123456789123456789', [nondet]) :-
  phrase(number(cardinal, {nominative, singular}, 123_456_789_123_456_789_123_456_789),
    `satakaksikymmentäkolmebiljoonaaneljäsataaviisikymmentäkuusimiljardiaseitsemänsataakahdeksankymmentäyhdeksänmiljoonaasatakaksikymmentäkolmetuhattaneljäsataaviisikymmentäkuusibiljoonaaseitsemänsataakahdeksankymmentäyhdeksänmiljardiasatakaksikymmentäkolmemiljoonaaneljäsataaviisikymmentäkuusituhattaseitsemänsataakahdeksankymmentäyhdeksän`).

test('teens', [nondet]) :-
  phrase(number(cardinal, {nominative, singular}, 11), `yksitoista`),
  phrase(number(cardinal, {nominative, singular}, 211), `kaksisataayksitoista`).

test('tens', [nondet]) :-
  phrase(number(cardinal, {nominative, singular}, 20), `kaksikymmentä`),
  phrase(number(cardinal, {nominative, singular}, 220), `kaksisataakaksikymmentä`).

% The partitive unit only happens in the nominative+singular case
test('{nominative, singular}', [nondet]) :-
  phrase(number(cardinal, {nominative, singular}, 225), `kaksisataakaksikymmentäviisi`).

test('{nominative, plural}', [nondet]) :-
  phrase(number(cardinal, {nominative, plural}, 225), `kahdetsadatkahdetkymmenetviidet`).

test('{genitive, singular}', [nondet]) :-
  phrase(number(cardinal, {genitive, singular}, 225), `kahdensadankahdenkymmenenviiden`).

test('{genitive, plural}', [nondet]) :-
  phrase(number(cardinal, {genitive, plural}, 225), `kaksiensatojenkaksienkymmenienviisien`).

test('{partitive, plural}', [nondet]) :-
  phrase(number(cardinal, {partitive, plural}, 225), `kahsiasatojakahsiakymmeniäviisiä`).

test('ordinal 2', [nondet]) :-
  phrase(number(ordinal, {nominative, singular}, 2), `toinen`).

test('ordinal 12', [nondet]) :-
  phrase(number(ordinal, {nominative, singular}, 12), `kahdestoista`).

test('ordinal 22', [nondet]) :-
  phrase(number(ordinal, {nominative, singular}, 22), `kahdeskymmenestoinen`).

test('ordinal 202', [nondet]) :-
  phrase(number(ordinal, {nominative, singular}, 202), `kahdessadastoinen`).

test('ordinal 212', [nondet]) :-
  phrase(number(ordinal, {nominative, singular}, 212), `kahdessadaskahdestoista`).

test('ordinal 222', [nondet]) :-
  phrase(number(ordinal, {nominative, singular}, 222), `kahdessadaskahdeskymmenestoinen`).

test('ordinal 1942', [nondet]) :-
  phrase(number(ordinal, {genitive, singular}, 1942), `tuhannennenyhdeksännensadannenneljännenkymmenennentoisen`).

test('bad ordinal 1942', [fail]) :-
  phrase(number(ordinal, {genitive, singular}, 1942), `ensimmäisentuhannennenyhdeksännensadannenneljännenkymmenennentoisen`).

:- end_tests(number).
