ifeq ($(SWARMHOME),)
SWARMHOME=../../swarm-2.1.1
endif
APPLICATION=bug
APPVERSION=2000-04-28
BUGADDRESS=bug-swarm@swarm.org
OBJECTS=Bug.o main.o ExperSwarm.o ModelSwarm.o FoodSpace.o
OTHERCLEAN = output.hdf output.scm
DATAFILES = bug.scm

include $(SWARMHOME)/etc/swarm/Makefile.appl

main.o: main.m ExperSwarm.h Bug.h FoodSpace.h 
Bug.o: Bug.h Bug.m
ModelSwarm.o: ModelSwarm.h ModelSwarm.m
FoodSpace.o: FoodSpace.h FoodSpace.m
ExperSwarm.o: ExperSwarm.h ExperSwarm.m ModelSwarm.h


