%% IRCd Application
%%
%% vim: tabstop=8
%%

-module(ircd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Test
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) -> ircd_sup:start_link().

stop(_State) -> ok.

%% Test
-ifdef(TEST).

simple_test() ->
    ok = application:start(ircd),
    ?assertNot(undefined == whereis(ircd_sup)).

-endif.
