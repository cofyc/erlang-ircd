-module(tidy).
-export([start/0, my_formatter/2]).

start() ->
    erl_tidy:dir(["./src"], [{backups, false}, {printer, fun ?MODULE:my_formatter/2}]).

my_formatter(A, B) ->
    erl_prettypr:format(A, B).
