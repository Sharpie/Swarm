ifeq ($(SWARMHOME),)
SWARMHOME=/usr
endif
APPLICATION=arborgames
APPVERSION=2.1
BUGADDRESS=bug-swarm@swarm.org
OBJECTS=SeedSpace.o Forest.o   \
        Fire.o Tree.o Species.o ForestModelSwarm.o ForestObserverSwarm.o \
        ForestBatchSwarm.o main.o
DATAFILES = species.scm batch.setup experiment.setup

include $(SWARMHOME)/etc/swarm/Makefile.appl

main.o: main.m ForestObserverSwarm.h ForestBatchSwarm.h
ForestObserverSwarm.o: ForestObserverSwarm.m ForestObserverSwarm.h \
  ForestModelSwarm.h
ForestBatchSwarm.o: ForestBatchSwarm.m ForestBatchSwarm.h ForestModelSwarm.h
ForestModelSwarm.o: ForestModelSwarm.m ForestModelSwarm.h Tree.h 
Tree.o: Tree.m Tree.h Species.h
Species.o: Species.m Species.h SeedSpace.h
SeedSpace.o: SeedSpace.m SeedSpace.h
Forest.o: Forest.m Forest.h Tree.h
Fire.o: Fire.m Fire.h Tree.h 




