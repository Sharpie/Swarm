ifeq ($(SWARMHOME),)
SWARMHOME=../../swarm-1998-08-17
endif
APPLICATION=bug
OBJECTS= main.o  Bug.o
include $(SWARMHOME)/etc/swarm/Makefile.appl

main.o: main.m Bug.h
Bug.o: Bug.h Bug.m

