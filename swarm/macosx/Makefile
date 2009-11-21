SED := $(shell (which sed && sed --version) > /dev/null 2>&1 && echo 1 || echo 0)

MAKEFLAGS := -e
PROJECT   := Swarm

SRC_ROOT      := $(realpath ../../)
SRC_SWARM     := $(SRC_ROOT)/swarm

OS        := $(shell uname -s)

ifeq ($(SED),0)
fail:
	@echo "ERROR: Your version of \`sed' is incompatible!"
	@echo "       Please install GNU sed, and make sure it's first in you path!"
	@exit 1
else
help: ${OS}_help

include makefile.d/${OS}.Makefile

build: .${OS}_build

install: .${OS}_build ${OS}_install
endif

x: ${OS}_x

info: ${OS}_info

clean: ${OS}_clean
	-rm -f .${OS}_*

################################################################################
.PHONY: all clean info ${OS}_clean ${OS}_info ${OS}_help ${OS}_x
