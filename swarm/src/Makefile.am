SUBDIRS = misc collections defobj activity \
	objectbase \
	tclobjc tkobjc gui \
	random \
	simtools simtoolsgui \
	analysis space

SWARMMODULES = space analysis simtoolsgui simtools random tkobjc tclobjc \
	objectbase activity defobj collections misc

EXTRA_DIST = Makefile.rules genctors

.PHONY: headers

all-recursive: headers

EXTRA_PROGRAMS = swarm.dll
bin_PROGRAMS = @SWARM_DLL@

headers:
	for i in $(SUBDIRS) ; do $(MAKE) -C $$i headers ; done

include $(top_builddir)/etc/Makefile.common

CURRENT = 0
REVISION = 0
AGE = 0
DLLWRAP = @DLLWRAP@

lib_LTLIBRARIES = @LIBSWARM_LA@
EXTRA_LTLIBRARIES = libswarm.la

libswarm_la_SOURCES = 

LIBS = $(foreach module,$(SWARMMODULES),-l$(module)) $(SYSLIBS)

libswarm_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE) $(SUPPORTLDFLAGS) $(foreach module,$(SWARMMODULES),-L$(module)) -L$(dir $(shell $(CC) -print-libgcc-file-name)) -L../libobjc -rpath $(libdir)

CTOR = swarm_constructors
OBJS = $(foreach module,$(SWARMMODULES),$(module)/*.o)

swarm.dll:
	$(CC) -c $(srcdir)/dllinit.c
	$(srcdir)/genctors $(CTOR) $(OBJS)
	$(CC) -c $(CTOR).c
	$(DLLWRAP) --export-all --output-def swarm.def --implib libswarm.a --driver-name $(CC) -o swarm.dll dllinit.o $(CTOR).o $(OBJS) -L../libobjc $(SUPPORTLDFLAGS) $(SYSLIBS)  

swarm_dll_SOURCES = dllinit.c

install-data-local: 
	if test $(USEDLL) = yes; then \
	  $(INSTALL) libswarm.a $(libdir) && ranlib $(libdir)/libswarm.a ; \
	fi

