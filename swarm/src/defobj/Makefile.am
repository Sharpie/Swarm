MODULE = defobj
CURRENT = 4
REVISION = 0
AGE = 0
defobjdir = $(includedir)/Swarm
JAVAINCLUDES = @JAVAINCLUDES@
FFIINCLUDES = @FFIINCLUDES@

include $(top_srcdir)/src/Makefile.rules

EXTRA_DIST = make-h2x m2h internal.h java.h modulemap.el fcall_objc.h fcall_java.h _fcall.m 

INCLUDES = $(LIBOBJCINCLUDES) -I$(top_builddir) -I.. -I$(srcdir)/.. -I$(srcdir)/../collections -I$(srcdir)/../misc $(JAVAINCLUDES) $(HDF5INCLUDES) $(FFIINCLUDES) $(GNUSTEPLIBINCLUDES)

noinst_LTLIBRARIES = @noinst_LIBS@
lib_LTLIBRARIES = @inst_LIBS@
EXTRA_LTLIBRARIES = libdefobj.la

CLEANFILES = $(TYPEINFO) version.h

BUILT_SOURCES = $(TYPEINFO)

SRC = Archiver.m LispArchiver.m HDF5Archiver.m Arguments.m Create.m Customize.m DefClass.m DefObject.m HDF5Object.m Program.m Symbol.m Zone.m FCall.m FArguments.m defobj.m internal.m java.m directory.m JavaProxy.m JavaClassProxy.m JavaCollection.m JavaCollectionIndex.m COMProxy.m fcall_objc.m fcall_java.m COM.m swarm_hash.m swarm-objc-gnu.m

override OBJCFLAGS += @CFLAGS_NOWARN_UNKNOWN_PRAGMAS@

libdefobj_la_SOURCES = $(SRC) modulemap.c
libdefobj_la_LDFLAGS = @inst_LDFLAGS@
libdefobj_la_LIBADD = $(FFILIB)

bin_SCRIPTS = m2h make-h2x

defobj_HEADERS = $(MODULE).h Archiver.h LispArchiver.h HDF5Archiver.h Arguments.h Create.h Customize.h DefClass.h DefObject.h HDF5Object.h Program.h Symbol.h Zone.h FCall.h FArguments.h defalloc.h deftype.h directory.h JavaProxy.h JavaClassProxy.h JavaCollection.h JavaCollectionIndex.h javavars.h COMProxy.h macros.h COM.h swarm-objc-api.h swarm_hash.h swarm-objc-gnu.h

$(libdefobj_la_OBJECTS): version.h

version.h: $(top_srcdir)/VERSION
	sed -n '1s/.*/#define SWARM_VERSION "&"/p' < $< > $@
	sed -n '2s/^\([0-9][0-9]*\)\..*/#define SWARM_MAJOR \1/p' < $< >> $@
	sed -n '2s/^[0-9][0-9]*\.\([0-9][0-9]*\)\..*/#define SWARM_MINOR \1/p' < $< >> $@
	sed -n '2s/^[0-9][0-9]*\.[0-9][0-9]*\.\([0-9][0-9]*\)/#define SWARM_SUBMINOR \1/p' < $< >> $@

install-data-local: install-defobjHEADERS
	$(INSTALL_DATA) $(MODULE)_classes.h $(DESTDIR)$(defobjdir)
	$(INSTALL_DATA) $(MODULE)_types.h $(DESTDIR)$(defobjdir)
	$(INSTALL_DATA) version.h $(DESTDIR)$(defobjdir)

 include $(top_srcdir)/etc/Makefile.protocol

modulemap.elc: $(abs_top_builddir)/protocol.elc

$(srcdir)/modulemap.c: $(foreach module,$(MODULES),$(top_srcdir)/src/$(module)/$(module).h) modulemap.elc
	MODULES="$(MODULES)" $(ENVEMACS) -batch $(GUIOPT) -l '$(dos_abs_builddir)@PATHDELIM@modulemap.elc' -f generate-module-map
	gperf -k 1-255 -r -t modulemap > $@
