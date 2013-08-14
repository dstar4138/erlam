#
# Build for ErLam Compiler.
#

ERL=$(shell which erl)
REBAR=$(CURDIR)/bin/rebar

.PHONY: erlam test clean examples

erlam: 
	$(REBAR) compile

clean:
	$(REBAR) clean

test:
	$(REBAR) compile eunit

examples:
	@echo "Not implemented yet."
