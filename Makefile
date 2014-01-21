#
# Build for ErLam Compiler.
#

ERL=$(shell which erl)
REBAR=$(CURDIR)/bin/rebar
ELSC=$(CURDIR)/bin/els

.PHONY: erlam clean distclean doc test examples

erlam: 
	$(REBAR) compile
	-chmod +x bin/els

clean:
	$(REBAR) clean

distclean: clean
	-rm -r ebin
	-rm -r doc
	-rm examples/*.ex

doc: erlam
	$(REBAR) doc

test:
	$(REBAR) compile eunit

examples: erlam
	$(ELSC) examples/*
	chmod +x examples/*.ex


