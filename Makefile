.PHONY: all get-deps clean compile run eunit check check-eunit doc hex-publish rebar3-install

REBAR=$(shell which rebar3 || echo ./rebar3)

CHECK_FILES=\
	ebin/*.beam

CHECK_EUNIT_FILES=\
	.eunit/*.beam

all: compile

clean:
	@$(REBAR) clean

compile:
	@$(REBAR) compile

run:
	$(REBAR) shell

check_warnings:
	@$(REBAR) as warnings compile

warnings:
	@$(REBAR) as test compile

eunit:
	@ERL_FLAGS="-config $(PWD)/eunit" $(REBAR) eunit

.dialyzer_plt:
	dialyzer --build_plt -r _build/default \
		--apps erts kernel stdlib inets crypto public_key ssl xmerl \
		--fullpath \
		--output_plt .dialyzer_plt

check: .dialyzer_plt
	@$(REBAR) as test dialyzer

doc:
	@$(REBAR) edoc

hex-publish:
	@$(REBAR) hex publish

rebar3-install:
	wget https://s3.amazonaws.com/rebar3/rebar3
	chmod a+x rebar3
