%% Channel server.
%% Each channel is handled by a separate server process.

-module(ircd_channel).

-behavior(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-record(member, {nick, pid, ref}).

-record(state, {name, members}).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args,
			  []).

init([Name]) ->
    io:format("Channel name: ~p~n", [Name]),
    {ok, #state{name = Name, members = []}}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_call({join, Nick, Pid}, _Caller,
	    State = #state{name = _Name, members = Members}) ->
    Ref = erlang:monitor(process, Pid),
    NewMembers = [#member{nick = Nick, pid = Pid, ref = Ref}
		  | lists:keydelete(Nick, #member.nick, Members)],
    NewState = State#state{members = NewMembers},
    ok = broadcast(NewState, {join, Nick}),
    {reply, ok, NewState};
handle_call({privmsg, Nick, Text}, _Caller,
	    State = #state{}) ->
    ok = broadcast(State, {privmsg, Nick, Text}),
    {reply, ok, State};
handle_call({members}, _Caller,
	    State = #state{name = Name, members = Members}) ->
    {reply, {Name, Members}, State};
handle_call(_Msg, _Caller, State) -> {reply, ok, State}.

handle_info({'DOWN', Ref, process, _Pid, _ExitReason},
	    State = #state{members = Members}) ->
    [ok = broadcast(State, {part, Nick})
     || #member{nick = Nick, ref = R} <- Members, R = Ref],
    NewMembers = lists:keydelete(Ref, #member.ref, Members),
    {noreply, State#state{members = NewMembers}};
handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%% Private functions

broadcast(#state{name = Name, members = Members},
	  Event) ->
    Message = {channel_event, Name, Event},
    error_logger:info_report("broadcast message: ~p~n",
			     {broadcast, Name, Members, Message}),
    [begin
       io:format("send to ~p~n", [Pid]),
       gen_server:cast(Pid, Message)
     end
     || #member{pid = Pid} <- Members],
    ok.
