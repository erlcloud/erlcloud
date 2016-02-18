.PHONY: all get-deps clean compile run eunit check check-eunit doc

# determine which Rebar we want to be running
REBAR2=$(shell which rebar || echo ./rebar)
REBAR3=$(shell which rebar3)
ifeq ($(FORCE_REBAR2),true)
 REBAR=$(REBAR2)
 REBAR_VSN=2
else ifeq ($(REBAR3),)
 REBAR=$(REBAR2)
 REBAR_VSN=2
else
 REBAR=$(REBAR3)
 REBAR_VSN=3
endif

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
ifeq ($(REBAR_VSN),2)
	@$(REBAR) get-deps
endif

clean:
	@$(REBAR) clean

compile:
	@$(REBAR) compile

run:
ifeq ($(REBAR_VSN),2)
	erl -pa deps/*/ebin -pa ./ebin
else
	$(REBAR) shell
endif

eunit:
ifeq ($(REBAR_VSN),2)
	@$(REBAR) compile
	@$(REBAR) eunit skip_deps=true
else
	@$(REBAR) eunit
endif

check:
ifeq ($(REBAR_VSN),2)
	@$(REBAR) compile
	dialyzer --verbose --no_check_plt --no_native --fullpath \
		$(CHECK_FILES) \
		-Wunmatched_returns \
		-Werror_handling
else
	@$(REBAR) dialyzer
endif

check-eunit: eunit
ifeq ($(REBAR_VSN),2)
	dialyzer --verbose --no_check_plt --no_native --fullpath \
		$(CHECK_EUNIT_FILES) \
		-Wunmatched_returns \
		-Werror_handling
else
	@$(REBAR) dialyzer
endif

doc:
ifeq ($(REBAR_VSN),2)
	@$(REBAR) doc skip_deps=true
else
	@$(REBAR) edoc
endif

# The "install" step for Travis
travis-install:
ifeq ($(FORCE_REBAR2),true)
	rebar get-deps
else
	wget https://s3.amazonaws.com/rebar3/rebar3
	chmod a+x rebar3
endif
