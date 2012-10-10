REBAR=$(shell which rebar || echo ./rebar)

get-deps:
	@$(REBAR) get-deps

all: compile

clean:
	@$(REBAR) clean

compile:
	@$(REBAR) compile

eunit: compile
	@$(REBAR) eunit skip_deps=true