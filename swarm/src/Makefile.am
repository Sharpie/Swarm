MODULE = swarmtop
swarmtopdir = $(includedir)/Swarm

if ENABLE_OPENSTEP
SUBDIRS = misc collections defobj activity objectbase $(TCLTKGUIDIR) $(GUIDIR) random simtools $(SIMTOOLSGUI) analysis space swarmgstep $(SWARM_XMLRPC)
else
SUBDIRS = misc collections defobj activity objectbase $(TCLTKGUIDIR) $(GUIDIR) random simtools $(SIMTOOLSGUI) analysis space $(SWARM_XMLRPC)
endif
DIST_SUBDIRS = misc collections defobj activity objectbase tclobjc tkobjc gui random simtools simtoolsgui analysis space swarm_xmlrpc swarmgstep

SWARMMODULES = space analysis $(GNUSTEPMODULE) $(SIMTOOLSGUI) simtools random $(TCLTKGUIDIR) objectbase activity defobj collections misc $(SWARM_XMLRPC) 

EXTRA_DIST = Makefile.rules getters.el SwarmEnvironment_getters.m

.PHONY: headers swarm-link

all-recursive: swarm-link headers

#EXTRA_PROGRAMS = swarm.dll
#bin_PROGRAMS = @SWARM_DLL@

swarm-link:
	-rm -f Swarm
	-rm -f $(srcdir)/Swarm
	-$(LN_S) $(srcdir) Swarm
	-$(LN_S) . $(srcdir)/Swarm
	for i in $(SUBDIRS) ; do $(MAKE) -C $$i swarm-link ; done

headers: SwarmEnvironment_getters.m
	for i in $(SUBDIRS) ; do $(MAKE) -C $$i headers ; done

# pacify automake
SUPPORTLDFLAGS = 

 include $(top_builddir)/etc/Makefile.common

AM_CPPFLAGS = -DBUILDING_SWARM
AM_OBJCFLAGS = @CFLAGS_NOWARN_UNKNOWN_PRAGMAS@ 

CURRENT = 6
REVISION = 0
AGE = 4

lib_LTLIBRARIES = @LIBSWARM_LA@
EXTRA_LTLIBRARIES = libswarm.la
libswarm_la_DEPENDENCIES = $(foreach module,$(SWARMMODULES),$(module)/lib$(module).la) $(LIBOBJCDEP)

libswarm_la_SOURCES = SwarmEnvironment.m classList.m
BUILT_SOURCES = classList.m

swarmtop_HEADERS = SwarmTop.h SwarmEnvironment.h SwarmEnvironment_getters.h

SwarmEnvironment.lo: SwarmEnvironment_getters.m

SwarmEnvironment_getters.m: SwarmTop.h

 include $(top_srcdir)/etc/Makefile.protocol

getters.elc: $(abs_top_builddir)/protocol.elc $(abs_top_builddir)/interface.elc

$(abs_top_builddir)/interface.elc: $(abs_top_builddir)/protocol.elc

$(srcdir)/SwarmEnvironment_getters.m: SwarmTop.h getters.elc
	$(ENVEMACS) -batch $(GUIOPT) -l '$(dos_abs_builddir)@PATHDELIM@getters.elc' -f generate-SwarmEnvironment-getters

EXPORTALL=-Wl,--export-all
libswarm_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE) -rpath $(DESTDIR)$(libdir) $(foreach module,$(SWARMMODULES),-L$(module)) $(LIBOBJCLDFLAGS) $(FFILDFLAGS) $(SUPPORTLDFLAGS) -no-undefined $(if $(filter yes,$(USEDLL)),$(EXPORTALL) -shrext .dll) # -L$(dir $(shell $(CC) -print-libgcc-file-name)) 
libswarm_la_LIBADD = $(SWFFILIB)

LIBS = $(foreach module,$(SWARMMODULES),-l$(module)) $(libswarm_la_LIBADD) $(LIBOBJCLIB) $(SYSLIBS)

INCLUDES = $(LIBOBJCINCLUDES) -I$(srcdir)/collections -I$(srcdir)/defobj -I$(srcdir)/activity -I$(srcdir)/objectbase -I$(srcdir)/random -I$(srcdir)/simtools $(GUIDIRINCLUDE) $(SIMTOOLSGUIINCLUDE) -I$(srcdir)/misc -I. -I$(srcdir) $(JAVAINCLUDES) $(GNUSTEPLIBINCLUDES) $(SWARM_XMLRPC_INCLUDE)

DLLWRAP = @DLLWRAP@
if USEDLL
OBJS = $(foreach module,$(SWARMMODULES),$(module)/*.lo) SwarmEnvironment.lo

swarm.dll: swarm.dll.c $(OBJS)
	$(CC) -g -c $(srcdir)/swarm.dll.c
	$(DLLWRAP) --export-all-symbols --output-def swarm.def --implib libswarmdll.a --driver-name $(CC) -o swarm.dll swarm.dll.o $(OBJS) ../avcall/avcall.o ../avcall/avcall-i386-msvc.o -L../libobjc $(SUPPORTLDFLAGS) -lobjcdll $(filter-out $(FFILIB),$(SYSLIBS)) 

#$(CC) -nostdlib -Wl,-shared -Wl,--export-all-symbols -Wl,--out-implib,libswarmdll.a -Wl,--output-def,swarm.def -o swarm.dll swarm.dll.o $(patsubst %.lo,%.o,$(OBJS)) -L../libobjc $(SUPPORTLDFLAGS) -lobjcdll $(SYSLIBS) -lcygwin -lkernel32 `$(CC) -print-libgcc-file-name`

#install-data-local: 
#	$(INSTALL) libswarmdll.a $(DESTDIR)$(libdir)
#	ranlib $(DESTDIR)$(libdir)/libswarmdll.a
endif

classList.m:
	find $(foreach module,$(SWARMMODULES),$(srcdir)/$(module)) -name \*.h | grep -v random/test | xargs cat | egrep -v 'TARGET|TINDEX|List' | sed -n 's/^@interface  *\(.*\) *:.*/\1/p' $(if $(GUIDIR),,| egrep -v 'ActiveGraph|EZSequence|EZAverageSequence|EZBin|EZDistribution|EZGraph|FunctionGraph|Object2dDisplay|Value2dDisplay') > classList
	cat classList | sed 's/.*/@class &;/' > classList.m
	(echo 'void swarm_force_references () {' ; cat classList | sed 's/.*/  [& self];/'; echo '}') >> classList.m
	$(RM) classList
