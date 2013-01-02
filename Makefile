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
	mkdir -p $(INSTALL_DIR)
	cp -r include $(INSTALL_DIR)/include
	cp -r src $(INSTALL_DIR)/src
	cp -r ebin $(INSTALL_DIR)/ebin
	cp -r test $(INSTALL_DIR)/test

