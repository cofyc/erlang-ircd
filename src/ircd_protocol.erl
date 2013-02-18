%% IRCd protocol
%%
%% http://www.irchelp.org/irchelp/rfc/
%%
%% This file is modified from https://github.com/tonyg/erlang-ircd.
%%
%% vim: tabstop=8
%%

-module(ircd_protocol).

-export([compose/1, parse/1]).

-export([reply/3, reply_format/3, reply_format/4]).

-include("ircd.hrl").

%% The BNF representation for this is:
%%
%% <message>      ::= [ ':' <prefix> <SPACE> ] <command> <params> <crlf>
%% <prefix>       ::= <servername> | <nick> [ '!' <user> ] [ '@' <host> ]
%% <command>      ::= ' ' { ' ' }
%% <params>       ::= <SPACE> [ ':' <trailing> | <middle> <params> ]
%% <middle>       ::= <Any *non-empty* sequence of octets not including SPACE or NUL
%%                   or CR or LF, the first of which may not be ':'>
%% <trailing>     ::= <Any, possibly *empty*, sequence of octets not including NUL
%%                   or CR or LF>
%% <crlf>         ::= CR LF
%%
%% Additional sematicas and syntax for the extracted parameter strings:
%%
%% <target>       ::= <to> [ "," <target> ]
%% <to>           ::= <channel> | <user> '@' <servername> | <nick> | <mask>
%% <channel>      ::= ('#' | '&') <chstring>
%% <servername>   ::= <host>
%% <host>         ::= see RFC 952 [DNS:4] for details on allowed hostnames
%% <nick>         ::= <letter> { <letter> | <number> | <special> }
%% <mask>         ::= ('#' | '$') <chstring>
%% <chstring>     ::= <any 8bit code expect SPACE, BELL, NULL, CR, LF and comma
%%                    (',')>
%% Other parameter syntaxes are:
%%
%% <user>         ::= <nonwhite> { <nonwhite> }
%% <letter>       ::= 'a' ... 'z' | 'A' ... 'Z'
%% <number>       ::= '0' ... '9'
%% <special>      ::= '-' | '[' | ']' | '\' | '`' | '^' | '{' | '}'
%% <nonwhite>     ::= <any 8bit code expect SPACE (0x20), NUL (0x0), CR (0xd),
%%                    and LF (0xa)>

%% parse/1

parse(Line) -> parse_words(string:tokens(clean(Line), " ")).

%% compose/1

compose(M = #irc_message{prefix = false}) -> compose_message(M);
compose(M = #irc_message{prefix = Prefix}) ->
    [":", Prefix, " ", compose_message(M)].

%% reply/3

reply('ERR_NOSUCHNICK', Target, [Nick]) ->
    reply_format(401, [Target, Nick], "No such nick/channel");
reply('ERR_NOSUCHSERVER', Target, [ServerName]) ->
    reply_format(402, [Target, ServerName], "No such server");
reply('ERR_NOSUCHCHANNEL', Target, [ChannelName]) ->
    reply_format(403, [Target, ChannelName], "No such channel");
reply('ERR_CANNOTSENDTOCHAN', Target, [ChannelName]) ->
    reply_format(404, [Target, ChannelName], "Cannot send to channel");
reply('ERR_TOOMANYCHANNELS', Target, [ChannelName]) ->
    reply_format(405, [Target, ChannelName],
		 "You have joined too many channels");
reply('ERR_WASNOSUCHNICK', Target, [Nickname]) ->
    reply_format(406, [Target, Nickname], "There was no such nickname");
reply('ERR_TOOMANYTARGETS', Target, []) ->
    reply_format(407, [Target], "Duplicate recipients. No message delivered");
reply('ERR_NOORIGIN', Target, []) ->
    reply_format(409, [Target], "No origin specified");
reply('ERR_NORECIPIENT', Target, [Command]) ->
    reply_format(411, [Target], "No recipient given (~s)", [Command]);
reply('ERR_NOTEXTTOSEND', Target, []) ->
    reply_format(412, [Target], "No text to send");
reply('ERR_NOTOPLEVEL', Target, [Mask]) ->
    reply_format(413, [Target, Mask], "No toplevel domain specified");
reply('ERR_WILDTOPLEVEL', Target, [Mask]) ->
    reply_format(414, [Target, Mask], "Wildcard in toplevel domain");
reply('ERR_UNKNOWNCOMMAND', Target, [Command]) ->
    reply_format(421, [Target, Command], "Unknown command");
reply('ERR_NOMOTD', Target, []) ->
    reply_format(422, [Target], "MOTD File is missing");
reply('ERR_NOADMININFO', Target, [Server]) ->
    reply_format(423, [Target, Server], "No administrative info available");
reply('ERR_FILEERROR', Target, [FileOp, File]) ->
    reply_format(424, [Target], "File error doing ~s on ~s", [FileOp, File]);
reply('ERR_NONICKNAMEGIVEN', Target, []) ->
    reply_format(431, [Target], "No nickname given");
reply('ERR_ERRONEUSNICKNAME', Target, [Nick]) ->
    reply_format(432, [Target, Nick], "Erroneus nickname");
reply('ERR_NICKNAMEINUSE', Target, [Nick]) ->
    reply_format(433, [Target, Nick], "Nickname is already in use");
reply('ERR_NICKCOLLISION', Target, [Nick]) ->
    reply_format(436, [Target, Nick], "Nickname collision KILL");
reply('ERR_USERNOTINCHANNEL', Target, [Nick, Channel]) ->
    reply_format(441, [Target, Nick, Channel], "They aren't on that channel");
reply('ERR_NOTONCHANNEL', Target, [Channel]) ->
    reply_format(442, [Target, Channel], "You're not on that channel");
reply('ERR_USERONCHANNEL', Target, [User, Channel]) ->
    reply_format(443, [Target, User, Channel], "is already on channel");
reply('ERR_NOLOGIN', Target, [User]) ->
    reply_format(444, [Target, User], "User not logged in");
reply('ERR_SUMMONDISABLED', Target, []) ->
    reply_format(445, [Target], "SUMMON has been disabled");
reply('ERR_USERSDISABLED', Target, []) ->
    reply_format(446, [Target], "USERS has been disabled");
reply('ERR_NOTREGISTERED', Target, []) ->
    reply_format(451, [Target], "You have not registered");
reply('ERR_NEEDMOREPARAMS', Target, [Command]) ->
    reply_format(461, [Target, Command], "Not enough parameters");
reply('ERR_ALREADYREGISTRED', Target, []) ->
    reply_format(462, [Target], "You may not reregister");
reply('ERR_NOPERMFORHOST', Target, []) ->
    reply_format(463, [Target], "Your host isn't among the privileged");
reply('ERR_PASSWDMISMATCH', Target, []) ->
    reply_format(464, [Target], "Password incorrect");
reply('ERR_YOUREBANNEDCREEP', Target, []) ->
    reply_format(465, [Target], "You are banned from this server");
reply('ERR_KEYSET', Target, [Channel]) ->
    reply_format(467, [Target, Channel], "Channel key already set");
reply('ERR_CHANNELISFULL', Target, [Channel]) ->
    reply_format(471, [Target, Channel], "Cannot join channel (+l)");
reply('ERR_UNKNOWNMODE', Target, [Char]) ->
    reply_format(472, [Target, Char], "is unknown mode char to me");
reply('ERR_INVITEONLYCHAN', Target, [Channel]) ->
    reply_format(473, [Target, Channel], "Cannot join channel (+i)");
reply('ERR_BANNEDFROMCHAN', Target, [Channel]) ->
    reply_format(474, [Target, Channel], "Cannot join channel (+b)");
reply('ERR_BADCHANNELKEY', Target, [Channel]) ->
    reply_format(475, [Target, Channel], "Cannot join channel (+k)");
reply('ERR_NOPRIVILEGES', Target, []) ->
    reply_format(481, [Target],
		 "Permission Denied- You're not an IRC operator");
reply('ERR_CHANOPRIVSNEEDED', Target, [Channel]) ->
    reply_format(482, [Target, Channel], "You're not channel operator");
reply('ERR_CANTKILLSERVER', Target, []) ->
    reply_format(483, [Target], "You cant kill a server!");
reply('ERR_NOOPERHOST', Target, []) ->
    reply_format(491, [Target], "No O-lines for your host");
reply('ERR_UMODEUNKNOWNFLAG', Target, []) ->
    reply_format(501, [Target], "Unknown MODE flag");
reply('ERR_USERSDONTMATCH', Target, []) ->
    reply_format(502, [Target], "Cant change mode for other users");
reply('RPL_USERHOST', Target, Replies) ->
    reply_format(302, [Target], "[~s]", [string:join(Replies, " ")]);
reply('RPL_ISON', Target, Nicks) ->
    reply_format(303, [Target], "[~s]", [string:join(Nicks, " ")]);
reply('RPL_AWAY', Target, [Nick, AwayMessage]) ->
    reply_format(301, [Target, Nick], "~s", [AwayMessage]);
reply('RPL_UNAWAY', Target, []) ->
    reply_format(305, [Target], "You are no longer marked as being away");
reply('RPL_NOWAWAY', Target, []) ->
    reply_format(306, [Target], "You have been marked as being away");
reply('RPL_WHOISUSER', Target, [Nick, User, Host, RealName]) ->
    reply_format(311, [Target, Nick, User, Host, "*"], "~s", [RealName]);
reply('RPL_WHOISSERVER', Target, [Nick, Server, ServerInfo]) ->
    reply_format(312, [Target, Nick, Server], "~s", [ServerInfo]);
reply('RPL_WHOISOPERATOR', Target, [Nick]) ->
    reply_format(313, [Target, Nick], "is an IRC operator");
reply('RPL_WHOISIDLE', Target, [Nick, Integer]) ->
    reply_format(317, [Target, Nick, Integer], "seconds idle");
reply('RPL_ENDOFWHOIS', Target, [Nick]) ->
    reply_format(318, [Target, Nick], "End of /WHOIS list");
reply('RPL_WHOISCHANNELS', Target, [Nick, Channels]) ->
    reply_format(319, [Target, Nick], "~s", [string:join(Channels, " ")]);
reply('RPL_WHOWASUSER', Target, [Nick, User, Host, RealName]) ->
    reply_format(314, [Target, Nick, User, Host, "*"], "~s", [RealName]);
reply('RPL_ENDOFWHOWAS', Target, [Nick]) ->
    reply_format(369, [Target, Nick], "End of WHOWAS");
reply('RPL_LISTSTART', Target, []) ->
    reply_format(321, [Target], "Channel :Users  Name");
reply('RPL_LIST', Target, [Channel, NumVisible, Topic]) ->
    reply_format(322, [Target, Channel, NumVisible], "~s", [Topic]);
reply('RPL_LISTEND', Target, []) -> reply_format(323, [Target], "End of /LIST");
reply('RPL_CHANNELMODEIS', Target, [Channel, Mode, ModeParams]) ->
    reply_format(324, [Target, Channel, Mode, ModeParams], false);
reply('RPL_NOTOPIC', Target, [Channel]) ->
    reply_format(331, [Target, Channel], "No topic is set");
reply('RPL_TOPIC', Target, [Channel, Topic]) ->
    reply_format(332, [Target, Channel], "~s", [Topic]);
reply('RPL_INVITING', Target, [Channel, Nick]) ->
    reply_format(341, [Target, Channel, Nick], false);
reply('RPL_SUMMONING', Target, [User]) ->
    reply_format(342, [Target, User], "Summoning user to IRC");
reply('RPL_VERSION', Target, [Version, DebugLevel, Server, Comments]) ->
    reply_format(351, [Target, Version ++ "." ++ DebugLevel, Server], "~s",
		 [Comments]);
reply('RPL_WHOREPLY', Target,
      [Channel, User, Host, Server, Nick, Flags, HopCount, RealName]) ->
    reply_format(352, [Target, Channel, User, Host, Server, Nick, Flags],
		 "~b ~s", [HopCount, RealName]);
reply('RPL_ENDOFWHO', Target, [Name]) ->
    reply_format(315, [Target, Name], "End of /WHO list");
reply('RPL_NAMREPLY', Target, [Channel, Nicks]) ->
    reply_format(353, [Target, "=", Channel], "~s", [string:join(Nicks, " ")]);
reply('RPL_ENDOFNAMES', Target, [Channel]) ->
    reply_format(366, [Target, Channel], "End of /NAMES list");
reply('RPL_LINKS', Target, [Mask, Server, HopCount, ServerInfo]) ->
    reply_format(364, [Target, Mask, Server], "~b ~s", [HopCount, ServerInfo]);
reply('RPL_ENDOFLINKS', Target, [Mask]) ->
    reply_format(365, [Target, Mask], "End of /LINKS list");
reply('RPL_BANLIST', Target, [Channel, Banid]) ->
    reply_format(367, [Target, Channel, Banid], false);
reply('RPL_ENDOFBANLIST', Target, [Channel]) ->
    reply_format(368, [Target, Channel], "End of channel ban list");
reply('RPL_INFO', Target, [String]) ->
    reply_format(371, [Target], "~s", [String]);
reply('RPL_ENDOFINFO', Target, []) ->
    reply_format(374, [Target], "End of /INFO list");
reply('RPL_MOTDSTART', Target, [Server]) ->
    reply_format(375, [Target], "- ~s Message of the day - ", [Server]);
reply('RPL_MOTD', Target, [Text]) ->
    reply_format(372, [Target], "- ~s", [Text]);
reply('RPL_ENDOFMOTD', Target, []) ->
    reply_format(376, [Target], "End of /MOTD command");
reply('RPL_YOUREOPER', Target, []) ->
    reply_format(381, [Target], "You are now an IRC operator");
reply('RPL_REHASHING', Target, [ConfigFile]) ->
    reply_format(382, [Target, ConfigFile], "Rehashing");
reply('RPL_TIME', Target, [Server, ServerLocalTimeString]) ->
    reply_format(391, [Target, Server], "~s", [ServerLocalTimeString]);
reply('RPL_USERSSTART', Target, []) ->
    reply_format(392, [Target], "UserID   Terminal  Host");
reply('RPL_USERS', Target, [UserID, Terminal, Host]) ->
    reply_format(393, [Target], "~-8s ~-9s ~-8s", [UserID, Terminal, Host]);
reply('RPL_ENDOFUSERS', Target, []) ->
    reply_format(394, [Target], "End of users");
reply('RPL_NOUSERS', Target, []) ->
    reply_format(395, [Target], "Nobody logged in").

%% reply_format/3

reply_format(Code, Params, false) -> reply_format(Code, Params, false, []);
reply_format(Code, Params, Trailing) ->
    reply_format(Code, Params, "~s", [Trailing]).

%% reply_format/4

reply_format(Code, Params, false, []) ->
    #irc_message{prefix = "yechengfu.com", command = Code, params = Params,
		 trailing = false};
reply_format(Code, Params, FormatString, FormatArgs) ->
    #irc_message{prefix = "yechengfu.com", command = Code, params = Params,
		 trailing =
		     lists:flatten(io_lib:format(FormatString, FormatArgs))}.

%%% Private functions

compose_message(#irc_message{command = Command, params = Params,
			     trailing = Trailing}) ->
    [stringify(Command),
     case Params of
       [] -> "";
       _ -> [" ", string:join([stringify(P) || P <- Params], " ")]
     end,
     case Trailing of
       false -> "";
       _ -> [" :", Trailing]
     end,
     "\r\n"].

clean(Str) ->
    re:replace(Str, "(^\\s+)|(\\s+$)|([\\r\\n]+$)", "",
	       [global, {return, list}]).

parse_words([":" ++ Prefix | Rest]) ->
    P = parse_words(Rest), P#irc_message{prefix = Prefix};
parse_words([Command | Rest]) ->
    {Params, Trailing} = parse_params(Rest, []),
    #irc_message{prefix = false, command = Command, params = Params,
		 trailing = Trailing}.

parse_params([":" ++ FirstTrailing | Rest], ParamsRev) ->
    {lists:reverse(ParamsRev), string:join([FirstTrailing | Rest], " ")};
parse_params([], ParamsRev) -> {lists:reverse(ParamsRev), false};
parse_params([Param | Rest], ParamsRev) ->
    parse_params(Rest, [Param | ParamsRev]).

stringify(N) when is_integer(N) -> integer_to_list(N);
stringify(S) when is_list(S) -> S.
