include Makefile.base

.PHONY: exe
exe: build
	stack exec -- buh-exe

.PHONY: demo
demo: build
	stack exec buh-exe -- package.yaml
