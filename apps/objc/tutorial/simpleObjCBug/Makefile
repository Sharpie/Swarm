ifeq ($(SWARMHOME),)
SWARMHOME=../../swarm-1999-03-09
endif
APPLICATION=bug
OBJECTS= main.o  Bug.o
include $(SWARMHOME)/etc/swarm/Makefile.appl

main.o: main.m Bug.h
Bug.o: Bug.h Bug.m

