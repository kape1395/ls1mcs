-module(ls1mcs).
-export([start/0]).


%%
%%  Starts application (manually).
%%
start() ->
    application:start(dthread),
    application:start(uart),
    application:start(gproc),
    application:start(ls1mcs).

