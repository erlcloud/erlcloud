.PHONY: all get-deps clean compile run eunit check check-eunit doc

REBAR=$(shell which rebar || echo ./rebar)

# eventually this should be just ebin/*.beam, but there are a number
# of warnings in other files. Just check the clean files for now.
CHECK_FILES=\
	ebin/erlcloud_aws.beam \
	ebin/erlcloud_cloudtrail.beam \
	ebin/erlcloud_ddb.beam \
	ebin/erlcloud_ddb1.beam \
	ebin/erlcloud_ddb2.beam \
	ebin/erlcloud_ddb_impl.beam \
	ebin/erlcloud_ddb_util.beam \
	ebin/erlcloud_http.beam \
	ebin/erlcloud_httpc.beam \
	ebin/erlcloud_retry.beam \
	ebin/erlcloud_sts.beam \
	ebin/erlcloud_s3.beam \
	ebin/erlcloud_sns.beam

# Checks on the eunit files can help find bad specs and other issues,
# however there are some expected errors in some of the exception
# tests that should be ignored.
CHECK_EUNIT_FILES=\
	$(CHECK_FILES) \
	.eunit/erlcloud_ddb_tests.beam \
	.eunit/erlcloud_ddb2_tests.beam \
	.eunit/erlcloud_ddb_util_tests.beam \
	.eunit/erlcloud_ec2_tests.beam \
	.eunit/erlcloud_s3_tests.beam

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

check: compile
	dialyzer --verbose --no_check_plt --no_native --fullpath \
		$(CHECK_FILES) \
		-Wunmatched_returns \
		-Werror_handling

check-eunit: eunit
	dialyzer --verbose --no_check_plt --no_native --fullpath \
		$(CHECK_EUNIT_FILES) \
		-Wunmatched_returns \
		-Werror_handling

doc:
	@$(REBAR) doc skip_deps=true

