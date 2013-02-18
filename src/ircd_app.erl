%% IRCd Application
%%
%% vim: tabstop=8
%%

-module(ircd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) -> ircd_sup:start_link().

stop(_State) -> ok.
