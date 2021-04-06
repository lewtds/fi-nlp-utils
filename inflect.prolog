:- module(inflect, [inflect/4]).

inflect({nominative, singular}, "nolla") --> "nolla".
inflect({genitive, singular}, "nolla") --> "nollan".
inflect({partitive, singular}, "nolla") --> "nollaa".
inflect({nominative, plural}, "nolla") --> "nollat".
inflect({genitive, plural}, "nolla") --> "nollien".
inflect({partitive, plural}, "nolla") --> "nollia".

inflect({nominative, singular}, "yksi") --> "yksi".
inflect({genitive, singular}, "yksi") --> "yhden".
inflect({partitive, singular}, "yksi") --> "yktä".
inflect({nominative, plural}, "yksi") --> "yhdet".
inflect({genitive, plural}, "yksi") --> "yksien".
inflect({partitive, plural}, "yksi") --> "yksiä".

inflect({nominative, singular}, "kaksi") --> "kaksi".
inflect({genitive, singular}, "kaksi") --> "kahden".
inflect({partitive, singular}, "kaksi") --> "kahta".
inflect({nominative, plural}, "kaksi") --> "kahdet".
inflect({genitive, plural}, "kaksi") --> "kaksien".
inflect({partitive, plural}, "kaksi") --> "kahsia".

inflect({nominative, singular}, "kolme") --> "kolme".
inflect({genitive, singular}, "kolme") --> "kolmen".
inflect({partitive, singular}, "kolme") --> "kolmea".
inflect({nominative, plural}, "kolme") --> "kolmet".
inflect({genitive, plural}, "kolme") --> "kolmien".
inflect({partitive, plural}, "kolme") --> "kolmia".

inflect({nominative, singular}, "neljä") --> "neljä".
inflect({genitive, singular}, "neljä") --> "neljän".
inflect({partitive, singular}, "neljä") --> "neljää".
inflect({nominative, plural}, "neljä") --> "neljät".
inflect({genitive, plural}, "neljä") --> "neljien".
inflect({genitive, plural}, "neljä") --> "neljäin".
inflect({partitive, plural}, "neljä") --> "neljiä".

inflect({nominative, singular}, "viisi") --> "viisi".
inflect({genitive, singular}, "viisi") --> "viiden".
inflect({partitive, singular}, "viisi") --> "viittä".
inflect({nominative, plural}, "viisi") --> "viidet".
inflect({genitive, plural}, "viisi") --> "viisien".
inflect({genitive, plural}, "viisi") --> "viitten".
inflect({partitive, plural}, "viisi") --> "viisiä".

inflect({nominative, singular}, "kuusi") --> "kuusi".
inflect({genitive, singular}, "kuusi") --> "kuuden".
inflect({partitive, singular}, "kuusi") --> "kuutta".
inflect({nominative, plural}, "kuusi") --> "kuudet".
inflect({genitive, plural}, "kuusi") --> "kuusien".
inflect({partitive, plural}, "kuusi") --> "kuusia".

inflect({nominative, singular}, "seitsemän") --> "seitsemän".
inflect({genitive, singular}, "seitsemän") --> "seitsemän".
inflect({partitive, singular}, "seitsemän") --> "seitsemää".
inflect({nominative, plural}, "seitsemän") --> "seitsemät".
inflect({genitive, plural}, "seitsemän") --> "seitsemien".
inflect({partitive, plural}, "seitsemän") --> "seitsemiä".

inflect({nominative, singular}, "kahdeksan") --> "kahdeksan".
inflect({genitive, singular}, "kahdeksan") --> "kahdeksan".
inflect({partitive, singular}, "kahdeksan") --> "kahdeksaa".
inflect({nominative, plural}, "kahdeksan") --> "kahdeksat".
inflect({genitive, plural}, "kahdeksan") --> "kahdeksien".
inflect({genitive, plural}, "kahdeksan") --> "kahdeksain".
inflect({partitive, plural}, "kahdeksan") --> "kahdeksia".

inflect({nominative, singular}, "yhdeksän") --> "yhdeksän".
inflect({genitive, singular}, "yhdeksän") --> "yhdeksän".
inflect({partitive, singular}, "yhdeksän") --> "yhdeksää".
inflect({nominative, plural}, "yhdeksän") --> "yhdeksät".
inflect({genitive, plural}, "yhdeksän") --> "yhdeksien".
inflect({partitive, plural}, "yhdeksän") --> "yhdeksiä".

inflect({nominative, singular}, "kymmenen") --> "kymmenen".
inflect({genitive, singular}, "kymmenen") --> "kymmenen".
inflect({partitive, singular}, "kymmenen") --> "kymmentä".
inflect({nominative, plural}, "kymmenen") --> "kymmenet".
inflect({genitive, plural}, "kymmenen") --> "kymmenien".
inflect({genitive, plural}, "kymmenen") --> "kymmenten".
inflect({partitive, plural}, "kymmenen") --> "kymmeniä".

inflect({nominative, singular}, "sata") --> "sata".
inflect({genitive, singular}, "sata") --> "sadan".
inflect({partitive, singular}, "sata") --> "sataa".
inflect({nominative, plural}, "sata") --> "sadat".
inflect({genitive, plural}, "sata") --> "satojen".
inflect({partitive, plural}, "sata") --> "satoja".

inflect({nominative, singular}, "tuhat") --> "tuhat".
inflect({genitive, singular}, "tuhat") --> "tuhannen".
inflect({partitive, singular}, "tuhat") --> "tuhatta".
inflect({nominative, plural}, "tuhat") --> "tuhannet".
inflect({genitive, plural}, "tuhat") --> "tuhansien".
inflect({partitive, plural}, "tuhat") --> "tuhansia".

inflect({nominative, singular}, "miljoona") --> "miljoona".
inflect({genitive, singular}, "miljoona") --> "miljoonan".
inflect({partitive, singular}, "miljoona") --> "miljoonaa".
inflect({nominative, plural}, "miljoona") --> "miljoonat".
inflect({genitive, plural}, "miljoona") --> "miljoonien".
inflect({partitive, plural}, "miljoona") --> "miljoonia".

inflect({nominative, singular}, "miljardi") --> "miljardi".
inflect({genitive, singular}, "miljardi") --> "miljardin".
inflect({partitive, singular}, "miljardi") --> "miljardia".
inflect({nominative, plural}, "miljardi") --> "miljardit".
inflect({genitive, plural}, "miljardi") --> "miljardien".
inflect({partitive, plural}, "miljardi") --> "miljardeja".

inflect({nominative, singular}, "biljoona") --> "biljoona".
inflect({genitive, singular}, "biljoona") --> "biljoonan".
inflect({partitive, singular}, "biljoona") --> "biljoonaa".
inflect({nominative, plural}, "biljoona") --> "biljoonat".
inflect({genitive, plural}, "biljoona") --> "biljoonien".
inflect({partitive, plural}, "biljoona") --> "biljoonia".

% ---------------------------------------------------------- %

inflect({nominative, singular}, "nollas") --> "nollas".
inflect({genitive, singular}, "nollas") --> "nollannen".
inflect({partitive, singular}, "nollas") --> "nollatta".
inflect({nominative, plural}, "nollas") --> "nollannet".
inflect({genitive, plural}, "nollas") --> "nollansien".
inflect({partitive, plural}, "nollas") --> "nollansia".

inflect({nominative, singular}, "ensimmäinen") --> "ensimmäinen".
inflect({genitive, singular}, "ensimmäinen") --> "ensimmäisen".
inflect({partitive, singular}, "ensimmäinen") --> "ensimmäistä".
inflect({nominative, plural}, "ensimmäinen") --> "ensimmäiset".
inflect({genitive, plural}, "ensimmäinen") --> "ensimmäisten".
inflect({genitive, plural}, "ensimmäinen") --> "ensimmäisien".
inflect({partitive, plural}, "ensimmäinen") --> "ensimmäisiä".

inflect({nominative, singular}, "kahdes") --> "kahdes".
inflect({genitive, singular}, "kahdes") --> "kahdennen".
inflect({partitive, singular}, "kahdes") --> "kahdetta".
inflect({nominative, plural}, "kahdes") --> "kahdennet".
inflect({genitive, plural}, "kahdes") --> "kahdensien".
inflect({partitive, plural}, "kahdes") --> "kahdensia".

inflect({nominative, singular}, "toinen") --> "toinen".
inflect({genitive, singular}, "toinen") --> "toisen".
inflect({partitive, singular}, "toinen") --> "toista".
inflect({nominative, plural}, "toinen") --> "toiset".
inflect({genitive, plural}, "toinen") --> "toisten".
inflect({genitive, plural}, "toinen") --> "toisien".
inflect({partitive, plural}, "toinen") --> "toisia".

inflect({nominative, singular}, "kolmas") --> "kolmas".
inflect({genitive, singular}, "kolmas") --> "kolmannen".
inflect({partitive, singular}, "kolmas") --> "kolmatta".
inflect({nominative, plural}, "kolmas") --> "kolmannet".
inflect({genitive, plural}, "kolmas") --> "kolmansien".
inflect({partitive, plural}, "kolmas") --> "kolmansia".

inflect({nominative, singular}, "neljäs") --> "neljäs".
inflect({genitive, singular}, "neljäs") --> "neljännen".
inflect({partitive, singular}, "neljäs") --> "neljättä".
inflect({nominative, plural}, "neljäs") --> "neljännet".
inflect({genitive, plural}, "neljäs") --> "neljänsien".
inflect({partitive, plural}, "neljäs") --> "neljänsiä".

inflect({nominative, singular}, "viides") --> "viides".
inflect({genitive, singular}, "viides") --> "viidennen".
inflect({partitive, singular}, "viides") --> "viidettä".
inflect({nominative, plural}, "viides") --> "viidennet".
inflect({genitive, plural}, "viides") --> "viidensien".
inflect({partitive, plural}, "viides") --> "viidensiä".

inflect({nominative, singular}, "kuudes") --> "kuudes".
inflect({genitive, singular}, "kuudes") --> "kuudennen".
inflect({partitive, singular}, "kuudes") --> "kuudetta".
inflect({nominative, plural}, "kuudes") --> "kuudennet".
inflect({genitive, plural}, "kuudes") --> "kuudensien".
inflect({partitive, plural}, "kuudes") --> "kuudensia".

inflect({nominative, singular}, "seitsemäs") --> "seitsemäs".
inflect({genitive, singular}, "seitsemäs") --> "seitsemännen".
inflect({partitive, singular}, "seitsemäs") --> "seitsemättä".
inflect({nominative, plural}, "seitsemäs") --> "seitsemännet".
inflect({genitive, plural}, "seitsemäs") --> "seitsemänsien".
inflect({partitive, plural}, "seitsemäs") --> "seitsemänsiä".

inflect({nominative, singular}, "kahdeksas") --> "kahdeksas".
inflect({genitive, singular}, "kahdeksas") --> "kahdeksannen".
inflect({partitive, singular}, "kahdeksas") --> "kahdeksatta".
inflect({nominative, plural}, "kahdeksas") --> "kahdeksannet".
inflect({genitive, plural}, "kahdeksas") --> "kahdeksansien".
inflect({genitive, plural}, "kahdeksas") --> "yhdeksänsien".
inflect({partitive, plural}, "kahdeksas") --> "kahdeksansia".

inflect({nominative, singular}, "yhdeksäs") --> "yhdeksäs".
inflect({genitive, singular}, "yhdeksäs") --> "yhdeksännen".
inflect({partitive, singular}, "yhdeksäs") --> "yhdeksättä".
inflect({nominative, plural}, "yhdeksäs") --> "yhdeksännet".
inflect({nominative, plural}, "yhdeksäs") --> "yhdeksännet".
inflect({partitive, plural}, "yhdeksäs") --> "yhdeksänsiä".

inflect({nominative, singular}, "kymmenes") --> "kymmenes".
inflect({genitive, singular}, "kymmenes") --> "kymmenennen".
inflect({partitive, singular}, "kymmenes") --> "kymmenettä".
inflect({nominative, plural}, "kymmenes") --> "kymmenennet".
inflect({genitive, plural}, "kymmenes") --> "kymmenensien".
inflect({partitive, plural}, "kymmenes") --> "kymmenensiä".

inflect({nominative, singular}, "sadas") --> "sadas".
inflect({genitive, singular}, "sadas") --> "sadannen".
inflect({partitive, singular}, "sadas") --> "sadatta".
inflect({nominative, plural}, "sadas") --> "sadannet".
inflect({genitive, plural}, "sadas") --> "sadansien".
inflect({partitive, plural}, "sadas") --> "sadansia".

inflect({nominative, singular}, "tuhannes") --> "tuhannes".
inflect({genitive, singular}, "tuhannes") --> "tuhannennen".
inflect({partitive, singular}, "tuhannes") --> "tuhannetta".
inflect({nominative, plural}, "tuhannes") --> "tuhannennet".
inflect({genitive, plural}, "tuhannes") --> "tuhannensien".
inflect({partitive, plural}, "tuhannes") --> "tuhannensia".

inflect({nominative, singular}, "miljoonas") --> "miljoonas".
inflect({genitive, singular}, "miljoonas") --> "miljoonannen".
inflect({partitive, singular}, "miljoonas") --> "miljoonatta".
inflect({nominative, plural}, "miljoonas") --> "miljoonannet".
inflect({genitive, plural}, "miljoonas") --> "miljoonansien".
inflect({partitive, plural}, "miljoonas") --> "miljoonansia".

inflect({nominative, singular}, "miljardis") --> "miljardis".
inflect({genitive, singular}, "miljardis") --> "miljardinnen".
inflect({partitive, singular}, "miljardis") --> "miljarditta".
inflect({nominative, plural}, "miljardis") --> "miljardinnet".
inflect({genitive, plural}, "miljardis") --> "miljardinsien".
inflect({partitive, plural}, "miljardis") --> "miljardinsia".

inflect({nominative, singular}, "biljoonas") --> "biljoonas".
inflect({genitive, singular}, "biljoonas") --> "biljoonannen".
inflect({partitive, singular}, "biljoonas") --> "biljoonatta".
inflect({nominative, plural}, "biljoonas") --> "biljoonannet".
inflect({genitive, plural}, "biljoonas") --> "biljoonansien".
inflect({partitive, plural}, "biljoonas") --> "biljoonansia".
