SWARMHOME=../..
LIBNAME=objectbase
OBJECTS=SwarmObject.o Swarm.o Probe.o VarProbe.o MessageProbe.o \
	ProbeMap.o CustomProbeMap.o EmptyProbeMap.o ProbeLibrary.o \
	DefaultProbeMap.o CompleteProbeMap.o \
	CompleteVarMap.o probing.o swarm_rts_routines.o \
	ActivityControl.o 
HEADERS=SwarmObject.h Swarm.h Probe.h VarProbe.h MessageProbe.h \
	ProbeMap.h CustomProbeMap.h EmptyProbeMap.h ProbeLibrary.h \
	DefaultProbeMap.h CompleteProbeMap.h \
	CompleteVarMap.h probing.h swarm_rts_routines.h \
	ActivityControl.h

include $(SWARMHOME)/Makefile.lib

install-headers:
	cd $(INCLIB)/..; rm -f swarmobject swarmobject.h ; ln -s objectbase swarmobject ; ln -s objectbase.h swarmobject.h

SwarmObject.o: SwarmObject.m SwarmObject.h
Swarm.o: Swarm.m Swarm.h
ProbeLibrary.o:	ProbeLibrary.m ProbeLibrary.h
probing.o: probing.m probing.h ProbeLibrary.m ProbeLibrary.h
Probe.o: Probe.m Probe.h SwarmObject.h
VarProbe.o: VarProbe.m VarProbe.h Probe.m Probe.h 
MessageProbe.o:	MessageProbe.m MessageProbe.h Probe.m Probe.h 
ProbeMap.o: ProbeMap.m ProbeMap.h Probe.h SwarmObject.h
CustomProbeMap.o: CustomProbeMap.m CustomProbeMap.h EmptyProbeMap.h \
                  ProbeMap.h Probe.h SwarmObject.h
EmptyProbeMap.o: EmptyProbeMap.m EmptyProbeMap.h CustomProbeMap.h \
                  ProbeMap.h Probe.h SwarmObject.h
CompleteProbeMap.o: CompleteProbeMap.m CompleteProbeMap.h \
                    ProbeMap.m ProbeMap.h Probe.h SwarmObject.h
DefaultProbeMap.o: DefaultProbeMap.m DefaultProbeMap.h \
                    ProbeMap.m ProbeMap.h Probe.h SwarmObject.h
CompleteVarMap.o: CompleteVarMap.m CompleteVarMap.h \
                  ProbeMap.m ProbeMap.h Probe.h SwarmObject.h
swarm_rts_routines.o: swarm_rts_routines.m swarm_rts_routines.h
ActivityControl.o: ActivityControl.m ActivityControl.h
