MODULE = defobj
CURRENT = 2
REVISION = 0
AGE = 0
defobjdir = $(includedir)/$(MODULE)
JAVAINCLUDES = @JAVAINCLUDES@
FFIINCLUDES = @FFIINCLUDES@

include $(top_srcdir)/src/Makefile.rules

EXTRA_DIST = make-h2x m2h internal.h

INCLUDES = -I$(top_builddir)/libobjc -I$(top_srcdir)/libobjc -I.. -I$(srcdir)/.. -I$(srcdir)/../collections -I$(srcdir)/../misc $(HDF5INCLUDES) $(JAVAINCLUDES) $(FFIINCLUDES)

noinst_LTLIBRARIES = libdefobj.la

module_headers = $(TYPEINFO)

CLEANFILES += version.h

BUILT_SOURCES = $(module_headers)

SRC = Archiver.m Arguments.m Create.m Customize.m \
	DefClass.m DefObject.m HDF5Object.m Program.m Symbol.m Zone.m \
	FCall.m FArguments.m defobj.m internal.m directory.m JavaProxy.m

override OBJCFLAGS += @CFLAGS_NOWARN_UNKNOWN_PRAGMAS@

libdefobj_la_SOURCES = $(SRC)
libdefobj_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE)

bin_SCRIPTS = m2h make-h2x

defobj_HEADERS = Archiver.h Arguments.h Create.h Customize.h \
	DefClass.h DefObject.h HDF5Object.h Program.h Symbol.h Zone.h \
	FCall.h FArguments.h defalloc.h deftype.h directory.h JavaProxy.h

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



