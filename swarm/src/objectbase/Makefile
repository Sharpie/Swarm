SWARMHOME=../..
LIBNAME=swarmobject
OBJECTS=SwarmObject.o Swarm.o Probe.o VarProbe.o MessageProbe.o ProbeMap.o EmptyProbeMap.o Averager.o ActiveGraph.o ProbeLibrary.o CompleteProbeMap.o probing.o swarm_rts_routines.o
HEADERS=SwarmObject.h Swarm.h Probe.h VarProbe.h MessageProbe.h ProbeMap.h EmptyProbeMap.h Averager.h ActiveGraph.h ProbeLibrary.h CompleteProbeMap.h probing.h swarm_rts_routines.h
include $(SWARMHOME)/Makefile.lib

main:		main.o
	$(CC) $(CFLAGS) -L. $(LDFLAGS) -o main main.o -lswarmobject -lcollections -ldefobj -lobjc

SwarmObject.o: SwarmObject.m SwarmObject.h
Swarm.o: Swarm.m Swarm.h
ProbeLibrary.o:	ProbeLibrary.m ProbeLibrary.h
probing.o:	probing.m probing.h ProbeLibrary.m ProbeLibrary.h
Probe.o: Probe.m Probe.h SwarmObject.h
VarProbe.o:	VarProbe.m VarProbe.h Probe.m Probe.h 
MesageProbe.o:	MesageProbe.m MessageProbe.h Probe.m Probe.h 
ProbeMap.o: ProbeMap.m ProbeMap.h Probe.h SwarmObject.h
EmptyProbeMap.o: EmptyProbeMap.m EmptyProbeMap.h ProbeMap.h Probe.h SwarmObject.h
Averager.o: Averager.m Averager.h SwarmObject.h
ActiveGraph.o: ActiveGraph.m ActiveGraph.h SwarmObject.h 
CompleteProbeMap.o:	CompleteProbeMap.m CompleteProbeMap.h \
                    ProbeMap.m ProbeMap.h Probe.h SwarmObject.h
swarm_rts_routines.o: swarm_rts_routines.m swarm_rts_routines.h
main.o: main.m
