-record(irc_message, {prefix = false, command, params, trailing = false}).
-record(irc_user, {username, hostname, servername, realname}).
-record(irc_agent, {pid, nick, user, host}).

% Max length of user name 
-define(CLIENT_USER_MAXLEN, 9).

% Max length of a channel name.
-define(CHANNEL_NAME_MAXLEN, 50).
