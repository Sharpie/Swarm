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

headers: SwarmEnvironment_getters.m
	for i in $(SUBDIRS) ; do $(MAKE) -C $$i headers ; done

 include $(top_builddir)/etc/Makefile.common

override CPPFLAGS += -DBUILDING_SWARM
 
CURRENT = 0
REVISION = 0
AGE = 0

lib_LTLIBRARIES = @LIBSWARM_LA@
EXTRA_LTLIBRARIES = libswarm.la
libswarm_la_DEPENDENCIES = $(foreach module,$(SWARMMODULES),$(module)/lib$(module).la)

libswarm_la_SOURCES = SwarmEnvironment.m

include_HEADERS = swarm.h SwarmEnvironment.h

SwarmEnvironment.lo: SwarmEnvironment_getters.m

SwarmEnvironment_getters.m: swarm.h

 include $(top_srcdir)/etc/Makefile.protocol

getters.elc: $(abs_top_builddir)/protocol.elc

$(srcdir)/SwarmEnvironment_getters.m: swarm.h getters.elc
	$(ENVEMACS) -batch -l '$(dos_abs_builddir)@PATHDELIM@getters.elc' -f generate-SwarmEnvironment-getters

LIBS = $(foreach module,$(SWARMMODULES),-l$(module)) -lobjc $(SYSLIBS)

INCLUDES = -I$(top_builddir)/libobjc -I$(top_srcdir)/libobjc -I$(srcdir)/collections -I$(srcdir)/defobj -I$(srcdir)/activity -I$(srcdir)/objectbase -I$(srcdir)/random -I$(srcdir)/simtools -I$(srcdir)/simtoolsgui -I$(srcdir)/misc -I. -I$(srcdir)

libswarm_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE) -rpath $(libdir) $(foreach module,$(SWARMMODULES),-L$(module)) -L../libobjc $(SUPPORTLDFLAGS) -L$(dir $(shell $(CC) -print-libgcc-file-name)) 

DLLWRAP = @DLLWRAP@
if USEDLL
OBJS = $(foreach module,$(SWARMMODULES),$(module)/*.lo) SwarmEnvironment.lo

swarm.dll: swarm.dll.c $(OBJS)
	$(CC) -g -c $(srcdir)/swarm.dll.c
	$(DLLWRAP) --export-all-symbols --output-def swarm.def --implib libswarmdll.a --driver-name $(CC) -o swarm.dll swarm.dll.o $(OBJS) -L../libobjc $(SUPPORTLDFLAGS) -lobjcdll $(SYSLIBS)
	#$(CC) -nostdlib -Wl,-shared -Wl,--export-all-symbols -Wl,--out-implib,libswarmdll.a -Wl,--output-def,swarm.def -o swarm.dll swarm.dll.o $(OBJS) -L../libobjc $(SUPPORTLDFLAGS) -lobjcdll $(SYSLIBS) -lcygwin -lkernel32 `$(CC) -print-libgcc-file-name`

install-data-local: 
	$(INSTALL) libswarmdll.a $(libdir)
	ranlib $(libdir)/libswarmdll.a
endif

