ifeq ($(SWARMHOME),)
SWARMHOME=../../swarm-1998-08-17
endif
APPLICATION=bug
OBJECTS= main.o  Bug.o FoodSpace.o
APPLIBS=-lspace

include $(SWARMHOME)/etc/swarm/Makefile.appl

main.o: main.m Bug.h FoodSpace.h
Bug.o: Bug.h Bug.m
FoodSpace.o: FoodSpace.h FoodSpace.m
