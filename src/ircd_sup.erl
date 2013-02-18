-module(ircd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SockServSpec = {ircd_sockserv,
		    {ircd_sockserv, start_link, ['127.0.0.1', 6667]}, permanent,
		    5000, worker, []},
    ControllerSpec = {ircd_system, {ircd_system, start_link, []}, permanent,
		      5000, worker, []},
    {ok, {{one_for_one, 5, 10}, [SockServSpec, ControllerSpec]}}.
