#
# Build for ErLam Compiler.
#

ERL=$(shell which erl)
REBAR=$(CURDIR)/bin/rebar

.PHONY: erlam test 

erlam: 
	$(REBAR) compile

test:
	$(REBAR) eunit

