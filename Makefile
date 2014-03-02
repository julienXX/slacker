ERL=`which erl`
REBAR=`which rebar`

all: clean deps compile doc

console: compile
	@$(ERL) -pa ebin -pa deps/*/ebin

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

compile-no-deps:
	@$(REBAR) compile skip_deps=true

doc:
	@$(REBAR) doc

test: compile-no-deps
	rm -rf .eunit
	@$(REBAR) eunit skip_deps=true

clean:
	@$(REBAR) clean

distclean: clean
	@rm -f *.dump
	@rm -rf ebin
	@rm -rf deps
