ifeq ($(SWARMHOME),)
SWARMHOME=../../swarm-2.0
endif
APPLICATION=bug
APPVERSION=2.0
BUGADDRESS=bug-swarm@santafe.edu
OBJECTS= main.o  Bug.o
include $(SWARMHOME)/etc/swarm/Makefile.appl

main.o: main.m Bug.h
Bug.o: Bug.h Bug.m

