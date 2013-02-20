%% Channel server.
%%
%% Each channel is handled by a separate server process.
%%
%% vim: tabstop=8
%%
%% protected ets tables:
%%   channel_members
%%

-module(ircd_channel).

-behavior(gen_server).

-include("ircd.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	 terminate/2]).

%% gen_server state
-record(state, {name, members_tabid}).
-record(member, {agent_pid, agent_ref, agent_nick}).

start_link(Args) -> gen_server:start_link(?MODULE, Args, []).

init([Name]) ->
    error_logger:info_msg("Channel ~p created.~n", [Name]),
    {ok, #state{
            name = Name,
            members_tabid = ets:new(channel_members, [set, {keypos,
                        #member.agent_pid}])
        }
    }.

handle_cast(_Msg, State) -> {noreply, State}.

handle_call({join, Nick, AgentPid}, _Caller,
	    State = #state{members_tabid = MembersTabId}) ->
    Ref = erlang:monitor(process, AgentPid),
    true = ets:insert(MembersTabId, #member{agent_pid = AgentPid, agent_ref =
            Ref, agent_nick = Nick}),
    ok = broadcast(State, {join, AgentPid}),
    {reply, ok, State};
handle_call({privmsg, AgentPid, Text}, _Caller, State = #state{}) ->
    ok = broadcast(State, AgentPid, {privmsg, AgentPid, Text}), {reply, ok, State};
handle_call({part, AgentPid}, _Caller, State = #state{members_tabid = MembersTabId}) ->
    case ets:lookup(MembersTabId, AgentPid) of
      [#member{agent_pid = AgentPid, agent_ref = Ref}] ->
          erlang:demonitor(Ref),
          ets:delete(MembersTabId, AgentPid);
      [] -> throw(internal_error)
    end,
    ok = broadcast(State, {part, AgentPid}),
    {reply, ok, State};
handle_call(info, _Caller, State = #state{name = Name, members_tabid =
        MembersTabId}) ->
    {reply, {Name, [Nick || #member{agent_nick = Nick} <-
                ets:tab2list(MembersTabId)]}, State};
handle_call(_Msg, _Caller, State) -> {reply, ok, State}.

handle_info({'DOWN', Ref, process, Pid, _ExitReason},
	    State = #state{members_tabid = MembersTabId}) ->
    error_logger:info_msg("agent: ~p down~n", Ref),
    case ets:lookup(MembersTabId, Pid) of
      [#member{agent_pid = AgentPid, agent_ref = Ref}] ->
          erlang:demonitor(Ref),
          ets:delete(MembersTabId, AgentPid);
      [] -> throw(internal_error)
    end,
    {noreply, State};
handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% Private functions

% broadcast/2
%
% Broadcast message to all members of this channel.
%
broadcast(#state{name = Name, members_tabid = MembersTabId}, Event) ->
    Message = {channel_event, Name, Event},
    error_logger:info_msg("[~s] broadcast message: ~p~n", [Name, Message]),
    _ = [ gen_server:cast(Pid, Message) || #member{agent_pid = Pid} <-
        ets:tab2list(MembersTabId)],
    ok.

% broadcast/3
%
% Broadcast message to all members of this channel except given one.
%
broadcast(#state{name = Name, members_tabid = MembersTabId}, AgentPid, Event) ->
    Message = {channel_event, Name, Event},
    error_logger:info_msg("[~s] broadcast message: ~p~n", [Name, Message]),
    _ = [gen_server:cast(Pid, Message)
	 || #member{agent_pid = Pid} <- ets:tab2list(MembersTabId), AgentPid =/= Pid],
    ok.
