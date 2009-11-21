MAKEFLAGS := -e
PROJECT   := Swarm

SRC_ROOT      := $(realpath ../../)
SRC_SWARM     := $(SRC_ROOT)/swarm

OS        := $(shell uname -s)

help:

include makefile.d/${OS}.Makefile

help: ${OS}_help

default: build;

build: .${OS}_build

install: .${OS}_build ${OS}_install

x: ${OS}_x

info: ${OS}_info

clean: ${OS}_clean
	-rm -f .${OS}_*

################################################################################
.PHONY: all clean info ${OS}_clean ${OS}_info ${OS}_help ${OS}_x
