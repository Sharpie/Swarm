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
DLLWRAP = @DLLWRAP@

lib_LTLIBRARIES = libswarm.la
libswarm_la_SOURCES = empty.m
LIBS = $(foreach module,$(SWARMMODULES),-l$(module)) $(SYSLIBS)

empty.m:
	echo > empty.m

libswarm_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE) $(SUPPORTLDFLAGS) $(foreach module,$(SWARMMODULES),-L$(module)) -L$(dir $(shell $(CC) -print-libgcc-file-name)) -L../libobjc

CTOR = swarm_constructors
OBJS = $(foreach module,$(SWARMMODULES),$(module)/*.o)

swarm.dll: libswarm.la
	$(RM) -r .dll swarm.def swarm.dll libswarm.a
	mkdir .dll
	cd .dll; for module in $(SWARMMODULES); do mkdir $$module; (cd $$module; ar x ../../$$module/.libs/lib$$module.al; for i in *.lo; do mv $$i `basename $$i .lo`.o; done); done
	$(CC) -c $(srcdir)/dllinit.c
	nm $(OBJS) | nl -s" " | sed -n "s/^ *\([0-9][0-9]*\)  *\([0-9a-f][0-9a-f]*\) \([^ ][^ ]*\) \(_GLOBAL_\$I\$.*\)/extern void $(CTOR)_sym\1 (void) __asm__ (\"\4\");/p" > $(CTOR).c; echo >> $(CTOR).c; echo "void $(CTOR) (void) {" >> $(CTOR).c; nm $libobjs | nl -s" " | sed -n "s/^ *\([0-9][0-9]*\)  *\([0-9a-f][0-9a-f]*\) \([^ ][^ ]*\) \(_GLOBAL_\$I\$.*\)/$(CTOR)_sym\1 ();/p" >> $(CTOR).c
	echo "}" >> $(CTOR).c
	$(CC) -c $(CTOR).c
	$(DLLWRAP) --export-all --output-def swarm.def --implib libswarm.a --driver-name $(CC) -o swarm.dll dllinit.o $(CTOR).o $(OBJS) $(SUPPORTLDFLAGS) $(SYSLIBS)  

install-data-local: 
	test -n "$(SWARMDLL)" && $(INSTALL) libswarm.a $(libdir) && ranlib $(libdir)/libswarm.a

