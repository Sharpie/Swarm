ifeq ($(SWARMHOME),)
SWARMHOME=../../swarm
endif
APPLICATION=bug
OBJECTS= main.o  Bug.o FoodSpace.o
APPLIBS=-lspace

include $(SWARMHOME)/Makefile.appl

main.o: main.m Bug.h FoodSpace.h
Bug.o: Bug.h Bug.m
FoodSpace.o: FoodSpace.h FoodSpace.m
