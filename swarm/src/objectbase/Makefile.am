MODULE = objectbase
CURRENT = 0
REVISION = 0
AGE = 0
objectbasedir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

lib_LTLIBRARIES = libobjectbase.la

INCLUDES = -I.. -I$(srcdir)/.. -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../activity -I$(srcdir)/../misc -I$(ffidir)/include

SRC = ActivityControl.m Arguments.m \
	CompleteVarMap.m CompleteProbeMap.m \
	CustomProbeMap.m CustomProbeMap.m DefaultProbeMap.m EmptyProbeMap.m \
	MessageProbe.m Probe.m ProbeLibrary.m ProbeMap.m Swarm.m \
	SwarmObject.m VarProbe.m probing.m swarm_rts_routines.m

libobjectbase_la_SOURCES = $(SRC)
libobjectbase_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE)

noinst_HEADERS = local.h

objectbase_HEADERS = ActivityControl.h Arguments.h CompleteProbeMap.h \
	CompleteVarMap.h CustomProbeMap.h DefaultProbeMap.h EmptyProbeMap.h \
	MessageProbe.h Probe.h ProbeLibrary.h ProbeMap.h Swarm.h \
	SwarmObject.h VarProbe.h probing.h swarm_rts_routines.h version.h

version.h: $(top_srcdir)/VERSION
	echo '#define SWARM_VERSION ' | tr -d '\n' > version.h
	head -1 $(top_srcdir)/VERSION >> version.h
	tail -1 $(top_srcdir)/VERSION | sed 's/^\([0-9][0-9]*\)\..*/#define SWARM_MAJOR \1/' >> version.h
	tail -1 $(top_srcdir)/VERSION | sed 's/^[0-9][0-9]*\.\([0-9][0-9]*\)\..*/#define SWARM_MINOR \1/' >> version.h
	tail -1 $(top_srcdir)/VERSION | sed 's/^[0-9][0-9]*\.[0-9][0-9]*\.\([0-9][0-9]*\)/#define SWARM_SUBMINOR \1/' >> version.h 

