ERL=`which erl`
REBAR=`which rebar`

all: clean deps compile

console: compile
	@$(ERL) -pa ebin -pa deps/*/ebin

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

docs:
	@$(REBAR) doc

test:
	@$(REBAR) eunit skip_deps=true suites=slacker_tests

clean:
	@$(REBAR) clean

distclean: clean
	@rm -f *.dump
	@rm -rf ebin
	@rm -rf deps
