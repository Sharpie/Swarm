MODULE = defobj
CURRENT = 0
REVISION = 2
AGE = 0
defobjdir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

EXTRA_DIST = make-h2x m2h

INCLUDES = -I.. -I$(srcdir)/.. -I$(srcdir)/../collections -I$(srcdir)/../misc

lib_LIBRARIES = libdefobj.a
lib_LTLIBRARIES = libdefobj.la

module_headers = classes.h types.h

BUILT_SOURCES = $(module_headers)

SRC = Archiver.m Arguments.m Create.m Customize.m \
	DefClass.m DefObject.m Program.m Symbol.m Zone.m defobj.m

libdefobj_a_SOURCES = $(SRC)
libdefobj_la_SOURCES = $(SRC)
libdefobj_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE)

bin_SCRIPTS = m2h make-h2x

defobj_HEADERS = Archiver.h Arguments.h Create.h Customize.h \
	DefClass.h DefObject.h Program.h Symbol.h Zone.h defalloc.h deftype.h

$(OBJECTS): version.h

version.h: $(top_srcdir)/VERSION
	sed -n '1s/.*/#define SWARM_VERSION "&"/p' < $< > $@
	sed -n '2s/^\([0-9][0-9]*\)\..*/#define SWARM_MAJOR \1/p' < $< >> $@
	sed -n '2s/^[0-9][0-9]*\.\([0-9][0-9]*\)\..*/#define SWARM_MINOR \1/p' < $< >> $@
	sed -n '2s/^[0-9][0-9]*\.[0-9][0-9]*\.\([0-9][0-9]*\)/#define SWARM_SUBMINOR \1/p' < $< >> $@

install-data-local:
	$(INSTALL_DATA) classes.h $(defobjdir)
	$(INSTALL_DATA) types.h $(defobjdir)
	$(INSTALL_DATA) version.h $(defobjdir)


