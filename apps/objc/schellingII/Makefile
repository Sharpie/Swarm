ifeq ($(SWARMHOME),)
SWARMHOME=/usr
endif


APPLICATION=schellingSwarm
OBJECTS=main.o ObserverSwarm.o ModelSwarm.o SchellingWorld.o  Person.o  Nhood2dCounter.o Output.o BatchSwarm.o Parameters.o BatchColormap.o BatchRaster.o

include $(SWARMHOME)/etc/swarm/Makefile.appl
main.o: main.m ObserverSwarm.h ModelSwarm.h BatchSwarm.h
SchellingWorld.o: SchellingWorld.m SchellingWorld.h
Person.o: Person.h Person.m
ObserverSwarm.o: ObserverSwarm.m ObserverSwarm.h ModelSwarm.h Parameters.h
ModelSwarm.o: ModelSwarm.m ModelSwarm.h Output.h Parameters.h
Nhood2dCounter.o: Nhood2dCounter.h Nhood2dCounter.m
Output.o: Output.h Output.m Parameters.h
BatchSwarm.o: BatchSwarm.h BatchSwarm.m Parameters.h
Parameters.o: Parameters.h Parameters.m
BatchRaster.o: BatchRaster.h BatchRaster.m
BatchColormap.o: BatchColormap.h BatchColormap.m
