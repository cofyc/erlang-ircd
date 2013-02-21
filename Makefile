all: deps compile

compile:
	rebar compile

deps:
	rebar get-deps

clean:
	rebar clean

run: all
	@erlc run.erl
	@erl -pa ebin/ -s run start ircd
	@rm *.beam
	

xref: clean all
	rebar xref

dialyzer: clean all
	dialyzer -q -n ebin -Wunmatched_returns -Werror_handling \
	        -Wrace_conditions

test: all
	rebar eunit

tidy:
	./tidy ./src

rel: VSN = $(shell ./get_vsn)
rel: all
	rebar -f generate
	rm -rf rel/ircd_$(VSN)
	cp -r rel/ircd rel/ircd_$(VSN)

distclean: clean
	rm -rf rel/ircd
	rebar delete-deps

.PHONY: clean rel distclean test deps compile run xref dialyzer tidy all
