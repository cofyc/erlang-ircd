%% IRCd socket server.

-module(ircd_sockserv).

-behavior(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

%% gen_server state
-record(state, {host, port, lsock}).

%% macros
-define(TCP_OPTIONS,
	[list, {packet, line}, {active, true},
	 {reuseaddr, true}]).

start_link(Host, Port) ->
    State = #state{host = Host, port = Port},
    gen_server:start_link({local, ?MODULE}, ?MODULE, State,
			  []).

init(State = #state{host = Host, port = Port}) ->
    case gen_tcp:listen(Port,
			[{ip, host_to_ip(Host)}] ++ (?TCP_OPTIONS))
	of
      {ok, LSock} ->
	  NewState = State#state{lsock = LSock},
	  spawn_link(fun () -> acceptor(LSock) end),
	  {ok, NewState};
      {error, Reason} -> {stop, Reason}
    end.

handle_cast(_Msg, State) -> {noreply, State}.

handle_call(_Msg, _Caller, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, #state{lsock = LSock}) ->
    gen_tcp:close(LSock), ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% Private functions

acceptor(LSock) ->
    case gen_tcp:accept(LSock) of
      {ok, Sock} ->
	  {ok, Pid} = gen_server:start_link(ircd_agent, [Sock],
					    []),
	  gen_tcp:controlling_process(Sock, Pid);
      {error, Reason} -> exit({error, Reason})
    end,
    acceptor(LSock).

host_to_ip(Host) ->
    {ok, IP} = inet:getaddr(Host, inet), IP.
