ERL ?= erl
APP := egitsite

.PHONY: deps

all: deps
	@./rebar compile

test:
	@./rebar eunit

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
