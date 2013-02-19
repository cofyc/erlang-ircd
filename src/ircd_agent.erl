%%
%% IRCd client agent.
%%
%% Each client is handled by a separate agent process.
%%
%% vim: tabstop=8
%%

-module(ircd_agent).

-behavior(gen_server).

-include("ircd.hrl").

%% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	 terminate/2]).

%% gen_server state
-record(state, {sock, nick, user}).

init([Sock]) -> {ok, #state{sock = Sock}}.

handle_cast({channel_event, Name, {join, Nick}}, State) ->
    send(State,
	 #irc_message{prefix = Nick ++ "@localhost", command = "JOIN", params = [Name],
		      trailing = false}),
    {noreply, State};
handle_cast({channel_event, Name, {privmsg, Nick, Text}}, State) ->
    send(State,
	 #irc_message{prefix = Nick, command = "PRIVMSG", params = [Name],
		      trailing = Text}),
    {noreply, State};
handle_cast({channel_event, Name, {part, Nick}}, State) ->
    send(State,
	 #irc_message{prefix = Nick, command = "PART", params = [],
		      trailing = Name}),
    {noreply, State};
handle_cast(Message = #irc_message{}, State) ->
    send(State, Message), {noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.

handle_call(_Msg, _Caller, State) -> {noreply, State}.

handle_info({tcp, Sock, Line}, State = #state{sock = Sock}) ->
    error_logger:info_msg("input: ~p~n", [Line]),
    try handle_irc_message(ircd_protocol:parse(Line), State) catch
      Reason ->
	  error_logger:error_msg("invalid input: ~p, reason: ~p~n",
				 [Line, Reason])
    end;
handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%% Private functions

handle_irc_message(#irc_message{command = "NICK", params = [Nick]}, State) ->
    {noreply,
     call_system1(maybe_login(State#state{nick = Nick}), nick_change, [Nick])};
handle_irc_message(#irc_message{command = "USER", params = [U, H, S],
				trailing = R},
		   State) ->
    User = #irc_user{username = U, hostname = H, servername = S, realname = R},
    {noreply, maybe_login(State#state{user = User})};
handle_irc_message(#irc_message{command = "QUIT"}, State) ->
    {stop, normal, disconnect(State)};
handle_irc_message(#irc_message{command = "JOIN",
				params = [ChannelString | MaybeKeys]},
		   State = #state{}) ->
    Channels = string:tokens(ChannelString, ","),
    Keys = case MaybeKeys of
	     [_Keys] -> string:tokens(_Keys, ",");
	     _ -> []
	   end,
    {ChannelInfos, NewState} = call_system(State, join, [Channels, Keys]),
    _ = [begin
       reply(State, 'RPL_NAMREPLY', [Channel, Names]),
       reply(State, 'RPL_ENDOFNAMES', [Channel]),
       reply(State, 'RPL_TOPIC', [Channel, Topic])
     end
     || {Channel, Names, Topic} <- ChannelInfos],
    {noreply, NewState};
handle_irc_message(#irc_message{command = "PRIVMSG", params = [Targets],
				trailing = Text},
		   State) ->
    {noreply,
     call_system1(State, privmsg, [string:tokens(Targets, ","), Text])};
handle_irc_message(#irc_message{command = "PART", params = [ChannelString]},
		   State) ->
    {noreply, call_system1(State, part, [string:tokens(ChannelString, ",")])};
handle_irc_message(Msg, State) ->
    error_logger:info_report({ignored_message, Msg}), {noreply, State}.

disconnect(State = #state{sock = Sock}) ->
    case Sock of
      undefined -> ok;
      _ -> gen_tcp:close(Sock)
    end,
    State.

call_system(State = #state{}, Command, Args) ->
    case gen_server:call(ircd_system, {Command, Args}) of
      {error, Reason} -> throw(Reason);
      Response -> {Response, State}
    end.

call_system1(State = #state{}, Command, Args) ->
    {_, State} = call_system(State, Command, Args), State.

maybe_login(State = #state{nick = N, user = U})
    when N =/= undefined andalso U =/= undefined ->
    %% motd
    {ok, Server} = application:get_env(server),
    {ok, Description} = application:get_env(description),
    reply(State, 'RPL_MOTDSTART', [Server]),
    reply(State, 'RPL_MOTD', [Description]),
    reply(State, 'RPL_ENDOFMOTD', []),
    %% login
    {_, State} = call_system(State, login, [N, U]),
    State;
maybe_login(State) -> State.

reply(#state{nick = Nick}, Type, Params) ->
    gen_server:cast(self(), ircd_protocol:reply(Type, Nick, Params)).

send(#state{sock = Sock, nick=Nick}, Message) ->
    Data = ircd_protocol:compose(Message),
    error_logger:info_msg("reply to ~p: ~p~n", [Nick, lists:flatten(Data)]),
    ok = gen_tcp:send(Sock, Data),
    ok.
