-module(ircd_tests).

-include_lib("eunit/include/eunit.hrl").

-include("ircd.hrl").

parse_test_() ->
    { "ircd_protocol:parse"
    , [ ?_assertEqual(ircd_protocol:parse("  a b"), #irc_message{prefix=false,
                command="a", params=["b"], trailing=false})
      , ?_assertEqual(ircd_protocol:parse("  one two\r\n"),
          #irc_message{prefix=false, command="one", params=["two"],
              trailing=false})
      ]
    }.

compose_test_() ->
    { "ircd_protocol:parse"
    , [ ?_assertEqual("a b\r\n", lists:flatten(ircd_protocol:compose(#irc_message{prefix=false,
                            command="a", params=["b"], trailing=false})))
      , ?_assertEqual(":prefix one two :trailing\r\n",
          lists:flatten(ircd_protocol:compose(#irc_message{prefix="prefix",
                      command="one", params=["two"], trailing="trailing"})))
      ]
    }.
