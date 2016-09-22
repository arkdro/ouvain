REBAR=../rebar

all: deps compile_all

compile_all:
	$(REBAR) compile

compile:
	$(REBAR) compile skip_deps=true

deps:
	$(REBAR) get-deps

distclean:
	$(REBAR) clean

clean:
	$(REBAR) clean skip_deps=true

eunit:
	$(REBAR) eunit skip_deps=true

test: compile
	$(REBAR) eunit ct skip_deps=true

shell:
	erl -pz ebin

xref: compile
	$(REBAR) xref skip_deps=true

ct: compile
	$(REBAR) ct skip_deps=true

rel: compile
	$(REBAR) generate

dia:
	dialyzer --src -r src

.PHONY: all compile_all compile deps clean test shell xref ct
