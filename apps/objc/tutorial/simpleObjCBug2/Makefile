ifeq ($(SWARMHOME),)
SWARMHOME=../../swarm-2.1
endif
APPLICATION=bug
APPVERSION=2.1
BUGADDRESS=bug-swarm@swarm.org
OBJECTS= main.o Bug.o FoodSpace.o

include $(SWARMHOME)/etc/swarm/Makefile.appl

main.o: main.m Bug.h FoodSpace.h
Bug.o: Bug.h Bug.m
FoodSpace.o: FoodSpace.h FoodSpace.m
