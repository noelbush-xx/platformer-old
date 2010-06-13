ERL          ?= erl
EBIN_DIRS    := $(wildcard deps/*/ebin)
APP          := platformer

all: ebin/$(APP).app erl
	@rm -f deps/log4erl/ebin/mochinum.beam # Already provided by mochiweb

erl:
	./rebar compile

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

clean: 
	@echo "removing:"
	@rm -fv ebin/*.beam ebin/*.app

ebin/$(APP).app: src/$(APP).app
	@cp -v src/$(APP).app $@
