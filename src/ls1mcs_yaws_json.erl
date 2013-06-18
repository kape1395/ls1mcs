-module(ls1mcs_yaws_json).
-export([encode_list/1, encode/1]).
-include("ls1mcs.hrl").


%%
%%
%%
encode_list(List) ->
    [ encode(E) || E <- List ].


%%
%%
%%
encode(#command_address{desc = Desc, addr = Addr}) ->
    {[
        {desc, Desc},
        {addr, Addr}
    ]};

encode(#command_spec{desc = Desc, addr = Addr, port = Port, ack = Ack, comp = Comp, params = Params}) ->
    {[
        {desc, Desc},
        {addr, Addr},
        {port, Port},
        {ack, Ack},
        {comp, Comp},
        {params, encode_list(Params)}
    ]};

encode(#cmd_param_spec{name = Name, desc = Desc, type = Type, enum = Enum}) ->
    EnumEncoded = case Enum of
        undefined -> [];
        _ -> [ {enum, encode_list(Enum)} ]
    end,
    {[
        {name, Name},
        {desc, Desc},
        {type, Type}
    ] ++ EnumEncoded};

encode(#cmd_enum_spec{desc = Desc, value = Value}) ->
    {[
        {desc, Desc},
        {value, Value}
    ]}.


