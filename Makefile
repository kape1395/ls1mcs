REBAR=rebar
APP=gen_ax25u

all: compile

deps:
	$(REBAR) get-deps

compile:
	env LDFLAGS=-lutil CFLAGS="-include /usr/include/string.h" $(REBAR) compile

check: test itest

test: compile
	$(REBAR) eunit apps=$(APP) verbose=1 

itest: compile
	$(REBAR) ct apps=$(APP)

doc:
	$(REBAR) doc

clean:
	$(REBAR) clean
	rm -f itest/*.beam
	rm -f doc/*.html doc/edoc-info

.PHONY: all deps compile check test itest doc clean

