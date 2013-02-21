all: deps compile

compile:
	rebar compile

deps:
	rebar get-deps

clean:
	rebar clean

run: rel
	./rel/ircd/bin/ircd console

xref: clean all
	rebar xref

dialyzer: clean all
	dialyzer -q -n ebin -Wunmatched_returns -Werror_handling \
	        -Wrace_conditions

test:
	rebar eunit

tidy:
	./tidy ./src

rel: all
	rebar -f generate

distclean: clean
	rm -rf rel/ircd
	rebar delete-deps

.PHONY: clean rel distclean test deps compile run xref dialyzer tidy all
