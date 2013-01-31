REBAR=$(shell which rebar || echo ./rebar)

all: get-deps compile

get-deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

compile:
	@$(REBAR) compile

run:
	erl -pa deps/*/ebin -pa ./ebin

eunit: compile
	@$(REBAR) eunit skip_deps=true

dialyzer: compile
	dialyzer --verbose --no_check_plt --no_native --fullpath \
		ebin/erlcloud_ddb1.beam ebin/erlcloud_aws.beam \
		-Wunmatched_returns \
		-Werror_handling
