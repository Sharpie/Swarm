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

EXTRA_PROGRAMS = swarm.dll
bin_PROGRAMS = @SWARMDLL@

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
	$(RM) -r .dll
	mkdir .dll
	cd .dll; for module in $(SWARMMODULES); do mkdir $$module; (cd $$module; ar x ../../$$module/.libs/lib$$module.al); done
	$(CC) -c $(srcdir)/dllinit.c -o .dll/dllinit.lo
	dllwrap --export-all-symbols --output-def swarm.def --output-lib libswarm.a --driver-name $(CC) -o swarm.dll .dll/dllinit.lo $(foreach module,$(SWARMMODULES),`ls .dll/$(module)/*.lo`) $(SUPPORTLDFLAGS) $(SYSLIBS)  

install-data-local: 
	test -n "$(SWARMDLL)" && $(INSTALL) libswarm.a $(libdir) && ranlib $(libdir)/libswarm.a
