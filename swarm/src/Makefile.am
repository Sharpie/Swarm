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
bin_PROGRAMS = @SWARM_DLL@

headers:
	for i in $(SUBDIRS) ; do $(MAKE) -C $$i headers ; done

include $(top_builddir)/etc/Makefile.common

CURRENT = 0
REVISION = 0
AGE = 0

lib_LTLIBRARIES = @LIBSWARM_LA@
EXTRA_LTLIBRARIES = libswarm.la
libswarm_la_DEPENDENCIES = $(foreach module,$(SWARMMODULES),$(module)/lib$(module).la)

libswarm_la_SOURCES = empty.c

empty.c:
	echo > empty.c

LIBS = $(foreach module,$(SWARMMODULES),-l$(module)) -lobjc $(SYSLIBS)

libswarm_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE) -rpath $(libdir) $(foreach module,$(SWARMMODULES),-L$(module)) -L../libobjc $(SUPPORTLDFLAGS) -L$(dir $(shell $(CC) -print-libgcc-file-name)) 

DLLWRAP = @DLLWRAP@
if USEDLL
OBJS = $(foreach module,$(SWARMMODULES),$(module)/*.lo)

swarm.dll: swarm.dll.c $(OBJS)
	$(CC) -g -c $(srcdir)/swarm.dll.c
	$(DLLWRAP) --export-all-symbols --output-def swarm.def --implib libswarmdll.a --driver-name $(CC) -o swarm.dll swarm.dll.o $(OBJS) $(SUPPORTLDFLAGS) -L../libobjc -lobjcdll $(SYSLIBS)
	#$(CC) -nostdlib -Wl,-shared -Wl,--export-all-symbols -Wl,--out-implib,libswarmdll.a -Wl,--output-def,swarm.def -o swarm.dll swarm.dll.o $(OBJS) $(SUPPORTLDFLAGS) -L../libobjc -lobjcdll $(SYSLIBS) -lcygwin -lkernel32 `$(CC) -print-libgcc-file-name`

install-data-local: 
	$(INSTALL) libswarmdll.a $(libdir)
	ranlib $(libdir)/libswarmdll.a
endif

