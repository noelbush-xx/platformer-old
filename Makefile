ERL ?= erl
APP := platformer
DOC_INCLUDES := "deps"
WM_DOCS := "<a href=\"http://webmachine.basho.com/resources.html\" target=\"_top\">Webmachine Resources</a>"

.PHONY: deps traceclean

test: all
	@(./rebar skip_deps=true eunit)

all: deps
	@./rebar compile

deps:
	@./rebar get-deps
	@rm -f deps/log4erl/ebin/mochinum.beam # Already provided by mochiweb

clean: traceclean
	@echo "removing:"
	@./rebar clean
	@rm -f test/*.pyc

distclean:
	@./rebar delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true}, {includes, [$(DOC_INCLUDES)]}, {exclude_packages, [external]}, {def, [{wmdocs, $(WM_DOCS)}]}]'
	@cp src/edoc.css doc/stylesheet.css

traceclean:
	@rm -rf /tmp/platformer/platformer*/*
