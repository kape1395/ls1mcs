-module(ls1mcs_ax25).
-export([bitstuff/1, bitdestuff/1]).


bitstuff(<<2#11111:5, Rest/bitstring>>) ->
    RestStuffed = bitstuff(Rest),
    <<2#111110:6, RestStuffed/bitstring>>;

bitstuff(<<Head:1, Rest/bitstring>>) ->
    RestStuffed = bitstuff(Rest),
    <<Head:1, RestStuffed/bitstring>>;

bitstuff(<<>>) ->
    <<>>.


bitdestuff(<<2#111110:6, Rest/bitstring>>) ->
    RestDestuffed = bitdestuff(Rest),
    <<2#11111:5, RestDestuffed/bitstring>>;

bitdestuff(<<Head:1, Rest/bitstring>>) ->
    RestDestuffed = bitdestuff(Rest),
    <<Head:1, RestDestuffed/bitstring>>;

bitdestuff(<<>>) ->
    <<>>.

