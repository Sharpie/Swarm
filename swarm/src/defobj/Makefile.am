MODULE = defobj
CURRENT = 2
REVISION = 0
AGE = 0
defobjdir = $(includedir)/$(MODULE)
JAVAINCLUDES = @JAVAINCLUDES@
FFIINCLUDES = @FFIINCLUDES@

include $(top_srcdir)/src/Makefile.rules

EXTRA_DIST = make-h2x m2h internal.h modulemap.el fcall_objc.h fcall_java.h _fcall.m

INCLUDES = -I$(top_builddir)/libobjc -I$(top_srcdir)/libobjc -I.. -I$(srcdir)/.. -I$(srcdir)/../collections -I$(srcdir)/../misc $(HDF5INCLUDES) $(JAVAINCLUDES) $(FFIINCLUDES)

noinst_LTLIBRARIES = @noinst_LIBS@
lib_LTLIBRARIES = @inst_LIBS@
EXTRA_LTLIBRARIES = libdefobj.la

CLEANFILES += version.h

BUILT_SOURCES = $(TYPEINFO)

SRC = Archiver.m LispArchiver.m HDF5Archiver.m Arguments.m Create.m	\
	Customize.m DefClass.m DefObject.m HDF5Object.m Program.m	\
	Symbol.m Zone.m FCall.m FArguments.m defobj.m internal.m	\
	directory.m JavaProxy.m	fcall_objc.m fcall_java.m

override OBJCFLAGS += @CFLAGS_NOWARN_UNKNOWN_PRAGMAS@

libdefobj_la_SOURCES = $(SRC) modulemap.c
libdefobj_la_LDFLAGS = @inst_LDFLAGS@

bin_SCRIPTS = m2h make-h2x

defobj_HEADERS = Archiver.h LispArchiver.h HDF5Archiver.h Arguments.h	  \
	Create.h Customize.h						  \
	DefClass.h DefObject.h HDF5Object.h Program.h Symbol.h Zone.h	  \
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

 include $(top_srcdir)/etc/Makefile.protocol

modulemap.elc: $(abs_top_builddir)/protocol.elc

modulemap.c: modulemap.elc
	MODULES="$(MODULES)" SWARMSRCDIR=$(top_srcdir) $(ENVEMACS) -batch -l $(abs_builddir)/modulemap.elc -f generate-module-map
	gperf -k \* -r -t modulemap > modulemap.c
