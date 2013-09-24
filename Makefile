REBAR=rebar
APP=ls1mcs
ENV=mcs
COMPILE_ENV=LDFLAGS=-lutil CFLAGS="-include string.h -Wno-deprecated-declarations"

all: compile-all

deps:
	$(REBAR) get-deps

compile:
	env $(COMPILE_ENV) $(REBAR) compile apps=$(APP)

compile-all:
	env $(COMPILE_ENV) $(REBAR) compile

check: test itest

test: compile
	$(REBAR) eunit apps=$(APP) verbose=1 

itest: compile
	$(REBAR) ct apps=$(APP)

doc:
	$(REBAR) doc

clean:
	$(REBAR) clean apps=$(APP)
	rm -f itest/*.beam
	rm -f doc/*.html doc/edoc-info
	rm -rf rel/ls1mcs

clean-all:
	$(REBAR) clean
	rm -f itest/*.beam
	rm -f doc/*.html doc/edoc-info

clean-deps:
	rm -rf deps

release-fresh: clean clean-deps deps compile-all release

release:
	cd rel && rm -rf $(APP) && $(REBAR) generate overlay_vars=vars/$(ENV).config


.PHONY: all deps compile compile-all check test itest doc clean clean-all clean-deps release-fresh release

