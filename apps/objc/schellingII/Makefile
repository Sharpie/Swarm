ifeq ($(SWARMHOME),)
SWARMHOME=/usr
endif


APPLICATION=schellingSwarm
OBJECTS=main.o ObserverSwarm.o ModelSwarm.o SchellingWorld.o  Person.o  Nhood2dCounter.o

include $(SWARMHOME)/etc/swarm/Makefile.appl
main.o: main.m ObserverSwarm.h ModelSwarm.h
SchellingWorld.o: SchellingWorld.m SchellingWorld.h
Person.o: Person.h Person.m
ObserverSwarm.o: ObserverSwarm.m ObserverSwarm.h ModelSwarm.h
ModelSwarm.o: ModelSwarm.m ModelSwarm.h
Nhood2dCounter.o: Nhood2dCounter.h Nhood2dCounter.m
