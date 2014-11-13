REBAR=$(shell which rebar)

all: clean deps compile doc

repl:
	@$(REBAR) shell

deps:
	@$(REBAR) get-deps

compile: deps
	@$(REBAR) compile

compile-no-deps:
	@$(REBAR) compile skip_deps=true

doc:
	@$(REBAR) doc skip_deps=true

test: compile-no-deps
	rm -rf .eunit
	@$(REBAR) eunit skip_deps=true

clean:
	@$(REBAR) clean

distclean: clean
	@rm -f *.dump
	@rm -rf ebin
	@rm -rf deps
