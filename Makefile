REBAR3=$(shell which rebar3)

.PHONY: all repl compile clean doc test distclean

all: compile doc

repl:
	@$(REBAR3) shell

compile:
	@$(REBAR3) compile

clean:
	@$(REBAR3) clean

doc:
	@$(REBAR3) edoc

test:
	@$(REBAR3) eunit

distclean:
	@rm -f *.dump
	@rm -rf _build
