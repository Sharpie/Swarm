ifeq ($(SWARMHOME),)
SWARMHOME=../../swarm-2.0
endif
APPLICATION=bug
APPVERSION=2.0
BUGADDRESS=bug-swarm@santafe.edu
OBJECTS=Bug.o  main.o ObserverSwarm.o ModelSwarm.o FoodSpace.o
APPLIBS=-lspace
include $(SWARMHOME)/etc/swarm/Makefile.appl

main.o: main.m ObserverSwarm.h Bug.h FoodSpace.h 
Bug.o: Bug.h Bug.m
ModelSwarm.o: ModelSwarm.h ModelSwarm.m
FoodSpace.o: FoodSpace.h FoodSpace.m
ObserverSwarm.o: ObserverSwarm.h ObserverSwarm.m ModelSwarm.h


