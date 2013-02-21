%% A handy script to start app by erl from shell.
-module(run).
-export([start/1]).

start(Args) ->
    application:start(hd(Args)).
