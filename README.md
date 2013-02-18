Erlang IRCd
==========

Yet another IRC daemon service that implements the IRC protocol, enabling people to talk to each other via the Internet (exchanging textual messages in real time).

> Disclaimer: I wrote this only for learning purpose, it's not producation ready now.

How to use
==========

Install erlang & [rebar](https://github.com/rebar/rebar) first.
    
    # release
    $ make rel
    # start as daemon
    $ ./rel/ircd/bin/ircd start

References
==========

- https://github.com/rebar/rebar
- https://github.com/tonyg/erlang-ircd
- http://www.irchelp.org/irchelp/rfc/rfc.html
- http://en.wikipedia.org/wiki/Internet_Relay_Chat
