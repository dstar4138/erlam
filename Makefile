#
# Build for ErLam Compiler.
#

ERL=$(shell which erl)
REBAR=$(CURDIR)/bin/rebar

.PHONY: erlam clean distclean doc test examples

erlam: 
	$(REBAR) compile

clean:
	$(REBAR) clean

distclean: clean
	-rm -r ebin
	-rm -r doc

doc: erlam
	$(REBAR) doc

test:
	$(REBAR) compile eunit

examples:
	@echo "Not implemented yet."
