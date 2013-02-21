%% IRCd system
%%
%% vim: tabstop=8
%%
%% shared ets:
%%  ircd_channels
%%  ircd_agents
%%

-module(ircd_system).

-behavior(gen_server).

-include("ircd.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	 terminate/2]).

%% gen_server state
-record(state, {}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    _ = ets:new(ircd_agents, [set, named_table, {keypos, #irc_agent.pid}]),
    _ = ets:new(ircd_channels, [set, named_table]),
    {ok, #state{}}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_call({login, [Nick, User, Host]}, {AgentPid, _AgentRef}, State) ->
    ets:insert(ircd_agents,
	       #irc_agent{pid = AgentPid, nick = Nick, user = User,
			  host = Host}),
    {reply, ok, State};
handle_call({nick_change, [Nick]}, _Caller, State) -> {reply, Nick, State};
handle_call({join, [Channels, _Keys]}, {AgentPid, _AgentRef}, State) ->
    case ets:lookup(ircd_agents, AgentPid) of
      [#irc_agent{pid = AgentPid, nick = Nick}] ->
	  {reply,
	   [begin
	      Pid = get_channel(Channel),
	      gen_server:call(Pid, {join, Nick, AgentPid}),
	      {_, Nicks} = gen_server:call(Pid, info),
	      {Channel, Nicks, "Topics not yet supported"}
	    end
	    || Channel <- Channels],
	   State};
      [] -> {reply, {error, exception}, State}
    end;
handle_call({privmsg, [Channels, Text]}, {AgentPid, _AgentRef}, State) ->
    case ets:lookup(ircd_agents, AgentPid) of
      [#irc_agent{pid = AgentPid, nick = _Nick, user = _User}] ->
	  _ = [begin
		 Pid = get_channel(Channel),
		 gen_server:call(Pid, {privmsg, AgentPid, Text})
	       end
	       || Channel <- Channels, is_channel_name(Channel)],
	  {reply, ok, State};
      [] -> {reply, {error, exception}, State}
    end;
handle_call({part, [Channels]}, {AgentPid, _AgentRef}, State) ->
    case ets:lookup(ircd_agents, AgentPid) of
      [#irc_agent{pid = AgentPid, nick = _Nick, user = _User}] ->
	  _ = [begin
		 Pid = get_channel(Channel),
		 gen_server:call(Pid, {part, AgentPid})
	       end
	       || Channel <- Channels, is_channel_name(Channel)],
	  {reply, ok, State};
      [] -> {reply, {error, exception}, State}
    end;
handle_call(_Msg, _Caller, State) -> {reply, {error, ignored_call}, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(OldVsn, State, Extra) ->
    error_logger:info_msg("code_change, oldvsn:~p state:~p extra:~p~n",
			  [OldVsn, State, Extra]),
    {ok, State}.

%% Private functions

get_channel(Name) ->
    case ets:lookup(ircd_channels, Name) of
      [{Name, Pid}] -> Pid;
      [] ->
	  {ok, Pid} = ircd_channel:start_link([Name]),
	  ets:insert_new(ircd_channels, {Name, Pid}),
	  Pid
    end.

is_channel_name("#" ++ _) -> true;
is_channel_name(_) -> false.
