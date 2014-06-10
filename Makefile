REBAR=./rebar
PLTFILE=$(CURDIR)/.deps.plt

BUILD_PLT_INC=$(shell test -d deps && echo '-r deps')
DIALYZER_INC=$(shell test -d include && echo '-I include') $(shell test -d deps && echo '-I deps')

.PHONY: all clean test

all:
	@$(REBAR) compile

edoc:
	@$(REBAR) doc

compile:
	@$(REBAR) -C rebar.config skip_deps=true compile

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

plt:
	- dialyzer --build_plt --apps $(APP_DEPS) $(BUILD_PLT_INC) --output_plt $(PLTFILE)

dialyzer: compile $(PLTFILE)
	@dialyzer --fullpath --plt $(PLTFILE) $(DIALYZER_INC) -pa $(CURDIR)/ebin --src \
	src/cowboy_sessions.erl | \
	fgrep -v -f ./dialyzer.ignore-warnings
