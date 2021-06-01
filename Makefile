include Makefile.base

.PHONY: exe
exe: build
	stack exec -- buh-exe
