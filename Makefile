all: deps compile

compile:
	rebar compile

deps:
	rebar get-deps

clean:
	rebar clean

run: rel
	./rel/ircd/bin/ircd console

test:
	rebar eunit

tidy:
	@erlc tidy.erl
	@erl -noshell -s tidy start -s init stop
	@rm -rf *.beam

rel: all relclean
	rebar compile generate

relclean:
	rm -rf rel/ircd

distclean: clean relclean
	rebar delete-deps

.PHONY: clean test deps
