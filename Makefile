REBAR=rebar
APP=ls1mcs

all: compile

deps:
	$(REBAR) get-deps

compile:
	env LDFLAGS=-lutil CFLAGS="-include string.h -Wno-deprecated-declarations" $(REBAR) compile apps=ls1mcs

compile-all:
	env LDFLAGS=-lutil CFLAGS="-include string.h -Wno-deprecated-declarations" $(REBAR) compile

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

clean-all:
	$(REBAR) clean
	rm -f itest/*.beam
	rm -f doc/*.html doc/edoc-info

kateproject:
	echo "{ \"name\": \"${APP}\", \"files\": [ { \"git\": 1 } ] }" > .kateproject

.PHONY: all deps compile compile-all check test itest doc clean clean-all

