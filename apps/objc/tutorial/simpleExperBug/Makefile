ifeq ($(SWARMHOME),)
SWARMHOME=../../swarm-2.0
endif
APPLICATION=bug
APPVERSION=2.0
BUGADDRESS=bug-swarm@santafe.edu
OBJECTS=Bug.o  main.o ExperSwarm.o ModelSwarm.o FoodSpace.o
DATAFILES = experiment.setup

include $(SWARMHOME)/etc/swarm/Makefile.appl

main.o: main.m ExperSwarm.h Bug.h FoodSpace.h 
Bug.o: Bug.h Bug.m
ModelSwarm.o: ModelSwarm.h ModelSwarm.m
FoodSpace.o: FoodSpace.h FoodSpace.m
ExperSwarm.o: ExperSwarm.h ExperSwarm.m ModelSwarm.h


