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
