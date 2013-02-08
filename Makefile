.PHONY: all get-deps clean compile run eunit dialyzer doc

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

dialyzer: eunit
# Lots of dialyzer warnings I don't want to fix - just check clean files for now
	dialyzer --verbose --no_check_plt --no_native --fullpath \
		ebin/erlcloud_ddb1.beam \
		ebin/erlcloud_ddb.beam \
		ebin/erlcloud_aws.beam \
		.eunit/erlcloud_ec2_tests.beam \
		.eunit/erlcloud_ddb_tests.beam \
		-Wunmatched_returns \
		-Werror_handling

doc:
	@$(REBAR) doc skip_deps=true

