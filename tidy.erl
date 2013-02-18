-module(tidy).
-export([start/0, my_formatter/2, hook/3]).

start() ->
    erl_tidy:dir(["./src"], [{backups, false}, {ribbon, 80}, {hook, fun
                    ?MODULE:hook/3}, {printer, fun ?MODULE:my_formatter/2}]).

my_formatter(A, B) ->
    erl_prettypr:format(A, B).

hook(A, B, C) -> C(A, B).
