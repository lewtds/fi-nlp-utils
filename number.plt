:- begin_tests(number).
:- use_module(number).

test('123456789123456789123456789', [nondet]) :-
  phrase(cardinal_number({nominative, singular}, 123_456_789_123_456_789_123_456_789),
    `satakaksikymmentäkolmebiljoonaaneljäsataaviisikymmentäkuusimiljardiaseitsemänsataakahdeksankymmentäyhdeksänmiljoonaasatakaksikymmentäkolmetuhattaneljäsataaviisikymmentäkuusibiljoonaaseitsemänsataakahdeksankymmentäyhdeksänmiljardiasatakaksikymmentäkolmemiljoonaaneljäsataaviisikymmentäkuusituhattaseitsemänsataakahdeksankymmentäyhdeksän`).

test('teens', [nondet]) :-
  phrase(cardinal_number({nominative, singular}, 11), `yksitoista`).

test('tens', [nondet]) :-
  phrase(cardinal_number({nominative, singular}, 20), `kaksikymmentä`).

% The partitive unit only happens in the nominative+singular case
test('{nominative, singular}', [nondet]) :-
  phrase(cardinal_number({nominative, singular}, 225), `kaksisataakaksikymmentäviisi`).

test('{nominative, plural}', [nondet]) :-
  phrase(cardinal_number({nominative, plural}, 225), `kahdetsadatkahdetkymmenetviidet`).

test('{genitive, singular}', [nondet]) :-
  phrase(cardinal_number({genitive, singular}, 225), `kahdensadankahdenkymmenenviiden`).

test('{genitive, plural}', [nondet]) :-
  phrase(cardinal_number({genitive, plural}, 225), `kaksiensatojenkaksienkymmenienviisien`).

test('{partitive, plural}', [nondet]) :-
  phrase(cardinal_number({partitive, plural}, 225), `kahsiasatojakahsiakymmeniäviisiä`).

:- end_tests(number).
