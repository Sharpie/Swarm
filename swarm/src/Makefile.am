SUBDIRS = misc collections defobj activity \
	objectbase \
	tclobjc tkobjc gui \
	random \
	simtools simtoolsgui \
	analysis space

SWARMMODULES = space analysis simtoolsgui simtools random tkobjc tclobjc \
	objectbase activity defobj collections misc

EXTRA_DIST = Makefile.rules

.PHONY: headers

all-recursive: headers

headers:
	for i in $(SUBDIRS) ; do $(MAKE) -C $$i headers ; done

include $(top_builddir)/etc/Makefile.common

CURRENT = 0
REVISION = 0
AGE = 0

lib_LTLIBRARIES = libswarm.la
libswarm_la_SOURCES = empty.m
LIBS = $(foreach module,$(SWARMMODULES),-l$(module)) $(SYSLIBS)

empty.m:
	echo > empty.m

libswarm_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE) $(SUPPORTLDFLAGS) $(foreach module,$(SWARMMODULES),-L$(module)) -L$(dir $(shell $(CC) -print-libgcc-file-name)) -L../libobjc

swarm.dll: libswarm.la
	dllwrap --export-all --output-def swarm.def --implib libswarm.a --driver-name $(CC) -o swarm.dll $(foreach module,$(SWARMMODULES),$(module)/.libs/lib$(module).al $(SUPPORTLDFLAGS) $(SYSLIBS)


