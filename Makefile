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

rel: VSN = $(shell ./get_vsn)
rel: all
	rebar generate
	rm -rf rel/ircd_$(VSN)
	mv rel/ircd rel/ircd_$(VSN)

distclean: clean
	rm -rf rel/ircd
	rebar delete-deps

.PHONY: clean rel distclean test deps compile run xref dialyzer tidy all
