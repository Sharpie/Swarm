MODULE = objectbase
CURRENT = 2
REVISION = 0
AGE = 1
objectbasedir = $(includedir)/$(MODULE)

FFIINCLUDES = @FFIINCLUDES@

include $(top_srcdir)/src/Makefile.rules

lib_LTLIBRARIES = libobjectbase.la

INCLUDES = -I.. -I$(srcdir)/.. -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../activity -I$(srcdir)/../misc $(FFIINCLUDES)

SRC = ActivityControl.m Arguments.m \
	CompleteVarMap.m CompleteProbeMap.m \
	CustomProbeMap.m DefaultProbeMap.m EmptyProbeMap.m \
	MessageProbe.m Probe.m ProbeLibrary.m ProbeMap.m Swarm.m \
	SwarmObject.m VarProbe.m probing.m swarm_rts_routines.m

libobjectbase_la_SOURCES = $(SRC)
libobjectbase_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE)

noinst_HEADERS = local.h

objectbase_HEADERS = ActivityControl.h Arguments.h \
	CompleteProbeMap.h CompleteVarMap.h \
	CustomProbeMap.h DefaultProbeMap.h EmptyProbeMap.h \
	MessageProbe.h Probe.h ProbeLibrary.h ProbeMap.h Swarm.h \
	SwarmObject.h VarProbe.h probing.h swarm_rts_routines.h

$(libobjectbase_la_OBJECTS): version.h

version.h: $(top_srcdir)/VERSION
	sed -n '1s/.*/#define SWARM_VERSION "&"/p' < $< > $@
	sed -n '2s/^\([0-9][0-9]*\)\..*/#define SWARM_MAJOR \1/p' < $< >> $@
	sed -n '2s/^[0-9][0-9]*\.\([0-9][0-9]*\)\..*/#define SWARM_MINOR \1/p' < $< >> $@
	sed -n '2s/^[0-9][0-9]*\.[0-9][0-9]*\.\([0-9][0-9]*\)/#define SWARM_SUBMINOR \1/p' < $< >> $@

install-data-local:
	$(INSTALL_DATA) version.h $(objectbasedir)
