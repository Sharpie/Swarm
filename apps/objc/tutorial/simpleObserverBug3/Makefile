ifeq ($(SWARMHOME),)
SWARMHOME=/usr
endif
APPLICATION=bug
APPVERSION=2.1
BUGADDRESS=bug-swarm@swarm.org
OBJECTS=Bug.o  main.o ObserverSwarm.o ModelSwarm.o FoodSpace.o Parameters.o
include $(SWARMHOME)/etc/swarm/Makefile.appl

main.o: main.m ObserverSwarm.h Bug.h FoodSpace.h  Parameters.h
Parameters.o: Parameters.h Parameters.m
Bug.o: Bug.h Bug.m
ModelSwarm.o: ModelSwarm.h ModelSwarm.m Parameters.h
FoodSpace.o: FoodSpace.h FoodSpace.m
ObserverSwarm.o: ObserverSwarm.h ObserverSwarm.m ModelSwarm.h

