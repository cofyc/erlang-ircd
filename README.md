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

Development
===========

### CodingGuidelines

-  http://www.erlang.se/doc/programming_rules.shtml

### Check

Before committing:
    
- Run `make tidy` to tidy source code.
- Run `make test` to execute the test suite.
- Run `make xref` to check for [xref](http://www.erlang.org/doc/man/xref.html).
- Run `make dialyzer` to check [Dialyzer](http://www.erlang.org/doc/man/dialyzer.html) warnings.

Deployment
==========

### How to upgrade

See https://github.com/basho/rebar/wiki/Upgrades.

References
==========

- https://github.com/rebar/rebar
- https://github.com/tonyg/erlang-ircd
- http://ngircd.barton.de/index.php.en
- http://en.wikipedia.org/wiki/Internet_Relay_Chat
- http://tools.ietf.org/html/rfc1459
