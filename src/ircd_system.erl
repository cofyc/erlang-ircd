-module(ircd_system).

-behavior(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

%% gen_server state
-record(state, {agents}).

-record(agent, {nick, user}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
			  []).

init([]) ->
    ircd_channels = ets:new(ircd_channels,
			    [set, named_table]),
    {ok,
     #state{agents = ets:new(agents, [set, named_table])}}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_call({login, [Nick, User]},
	    {AgentPid, _AgentRef},
	    State = #state{agents = Agents}) ->
    ets:insert(Agents,
	       {AgentPid, #agent{nick = Nick, user = User}}),
    {reply, ok, State#state{agents = Agents}};
handle_call({nick_change, [Nick]}, _Caller,
	    State = #state{}) ->
    {reply, Nick, State};
handle_call({join, [Channels, _Keys]},
	    {AgentPid, _AgentRef},
	    State = #state{agents = Agents}) ->
    case ets:lookup(Agents, AgentPid) of
      [{AgentPid, #agent{nick = Nick, user = _User}}] ->
	  {reply,
	   [begin
	      Pid = get_channel(Channel),
	      io:format("Channel pid: ~p~n", [Pid]),
	      gen_server:call(Pid, {join, Nick, AgentPid}),
	      %%{Name, Members} = gen_server:call(Pid, members),
	      {Channel, [], "Topics not yet supported"}
	    end
	    || Channel <- Channels],
	   State};
      [] -> {reply, {error, exception}, State}
    end;
handle_call({privmsg, [Targets, Text]},
	    {AgentPid, _AgentRef},
	    State = #state{agents = Agents}) ->
    case ets:lookup(Agents, AgentPid) of
      [{AgentPid, #agent{nick = Nick, user = _User}}] ->
	  [begin
	     Pid = get_channel(Target),
	     gen_server:call(Pid, {privmsg, Nick, Text})
	   end
	   || Target <- Targets, is_channel_name(Target)],
	  {reply, ok, State};
      [] -> {reply, {error, exception}, State}
    end;
handle_call(_Msg, _Caller, State) ->
    {reply, {error, ignored_call}, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%% Private functions

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
