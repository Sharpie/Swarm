#SUBDIRS = collections defobj activity \
	objectbase \
	tclobjc tkobjc gui \
	random \
	simtools simtoolsgui \
	analysis space \
	misc

SWARMMODULES = space analysis simtools simtoolsgui tkobjc tclobjc activity random objectbase collections defobj misc

SUBDIRS = $(SWARMMODULES)

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
LIBS = $(foreach module,$(SWARMMODULES),-l$(module)) -lgcc # $(SYSLIBS)

empty.m:
	echo > empty.m

libswarm_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE) $(SUPPORTLDFLAGS) $(foreach module,$(SWARMMODULES),-L$(module)) -L$(dir $(shell $(CC) -print-libgcc-file-name)) # -L../libobjc







