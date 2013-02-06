REBAR=$(shell which rebar || echo ./rebar)

ROOT=/usr/local/lib/erlang
INSTALL_DIR=$(ROOT)/lib/erlcloud-0.4.1

get-deps:
	@$(REBAR) get-deps

all: compile

clean:
	@$(REBAR) clean

compile:
	@$(REBAR) compile

eunit: compile
	@$(REBAR) eunit skip_deps=true

install:
	install -d $(INSTALL_DIR)
	install -d $(INSTALL_DIR)/include
	install include/* $(INSTALL_DIR)/include
	install -d $(INSTALL_DIR)/ebin
	install ebin/* $(INSTALL_DIR)/ebin

