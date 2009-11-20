PROJECT := Swarm
OS      := $(shell uname -s)

help:
	@echo "To build "
	@echo "make build"
	@echo "make install"
	@echo "make clean"

default: build;

include makefile.d/${OS}.Makefile

build: .${OS}_build

install: .${OS}_build ${OS}_install

x: ${OS}_x

info: ${OS}_info

clean: ${OS}_clean
	-rm -f .${OS}_*

################################################################################
.PHONY: all clean info ${OS}_clean ${OS}_info ${OS}_x
