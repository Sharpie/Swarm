ifeq ($(SWARMHOME),)
SWARMHOME=../../swarm-1998-08-17
endif
APPLICATION=bug
OBJECTS=Bug.o  main.o ObserverSwarm.o ModelSwarm.o FoodSpace.o
APPLIBS=-lspace
include $(SWARMHOME)/etc/swarm/Makefile.appl

main.o: main.m ObserverSwarm.h Bug.h FoodSpace.h 
Bug.o: Bug.h Bug.m
ModelSwarm.o: ModelSwarm.h ModelSwarm.m
FoodSpace.o: FoodSpace.h FoodSpace.m
ObserverSwarm.o: ObserverSwarm.h ObserverSwarm.m ModelSwarm.h


