MODULE = objectbase
CURRENT = 3
REVISION = 0
AGE = 0
objectbasedir = $(includedir)/$(MODULE)

include $(top_srcdir)/src/Makefile.rules

noinst_LTLIBRARIES = libobjectbase.la

JAVAINCLUDES = -I$(top_srcdir)/java/c @JAVAINCLUDES@

INCLUDES = -I$(top_srcdir)/libobjc -I.. -I$(srcdir)/.. -I$(srcdir)/../collections -I$(srcdir)/../defobj -I$(srcdir)/../activity -I$(srcdir)/../misc

SRC = ActivityControl.m \
	CompleteVarMap.m CompleteProbeMap.m \
	CustomProbeMap.m DefaultProbeMap.m EmptyProbeMap.m \
	MessageProbe.m Probe.m ProbeLibrary.m ProbeMap.m Swarm.m \
	SwarmObject.m VarProbe.m probing.m swarm_rts_routines.m

libobjectbase_la_SOURCES = $(SRC)
libobjectbase_la_LDFLAGS = -version-info $(CURRENT):$(REVISION):$(AGE)

noinst_HEADERS = local.h

objectbase_HEADERS = ActivityControl.h \
	CompleteProbeMap.h CompleteVarMap.h \
	CustomProbeMap.h DefaultProbeMap.h EmptyProbeMap.h \
	MessageProbe.h Probe.h ProbeLibrary.h ProbeMap.h Swarm.h \
	SwarmObject.h VarProbe.h probing.h swarm_rts_routines.h
