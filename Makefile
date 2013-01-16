REBAR=$(shell which rebar || echo ./rebar)

all: get-deps compile

get-deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

compile:
	@$(REBAR) compile

eunit: compile
	@$(REBAR) eunit skip_deps=true
