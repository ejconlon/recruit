include Makefile.base

.PHONY: exe
exe: build
	stack exec -- recruit-exe

.PHONY: demo
demo: build
	stack exec recruit-exe -- package.yaml
