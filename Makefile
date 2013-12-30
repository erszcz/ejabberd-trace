.PHONY: deps test

all: deps compile

compile: rebar
	./rebar compile

deps: rebar
	./rebar get-deps

clean: rebar
	./rebar clean

test: compile
	./rebar skip_deps=true eunit

rebar:
	wget http://cloud.github.com/downloads/basho/rebar/rebar && chmod u+x rebar

README.pdf: README.md
	pandoc $< -o $@

DEPS_PLT = $(CURDIR)/.deps_plt
DEPS = deps/exml erts kernel runtime_tools stdlib

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo
	dialyzer --output_plt $(DEPS_PLT) --build_plt --apps $(DEPS)

dialyzer: $(DEPS_PLT)
	dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./ebin

typer:
	typer -I deps -I include -I src --plt $(DEPS_PLT) -r ./src
