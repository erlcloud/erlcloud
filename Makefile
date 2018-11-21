.PHONY: all get-deps clean compile run eunit check check-eunit doc

# determine which Rebar we want to be running
REBAR2=$(shell which rebar || echo ./rebar)
REBAR3=$(shell which rebar3 || echo ./rebar3)
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

CHECK_FILES=\
	ebin/*.beam

CHECK_EUNIT_FILES=\
	$(CHECK_FILES) \
	.eunit/*.beam


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

check_warnings:
ifeq ($(REBAR_VSN),2)
	@echo skip checking warnings
else
	@$(REBAR) as warnings compile
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

travis-publish:
	@echo Create directories
	mkdir -p ~/.hex
	mkdir -p ~/.config/rebar3

	@echo Decrypt secrets
	@openssl aes-256-cbc -K $encrypted_9abc06b32f03_key -iv $encrypted_9abc06b32f03_iv -in hex.config.enc -out ~/.hex/hex.config -d

	@echo Create global config
	echo '{plugins, [rebar3_hex]}.' > ~/.config/rebar3/rebar.config

	@echo Edit version tag in app.src
	vi -e -c '%s/{vsn, *.*}/{vsn, "'${TRAVIS_TAG}'"}/g|w|q' src/erlcloud.app.src

	@echo Publish to Hex
	echo 'Y' | ./rebar3 hex publish

	@echo Done
